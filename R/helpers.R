# Merges given indexed row into larger neighbor !! Does not delete row
merge_into_larger_neighbor <- function(i, bouts) {
  if(i == 1)
    method = 'merge_down'
  else if(i == nrow(bouts))
    method = 'merge_up'
  else if(bouts[i-1,]$length >= bouts[i+1,]$length)
    method = 'merge_up'
  else
    method = 'merge_down'
  
  if(method == 'merge_up') {
    # merge up
    merge_i <- i-1
    
    bouts[merge_i,]$length <- bouts[merge_i,]$length + bouts[i,]$length
    bouts[merge_i,]$end_labtime <- bouts[i,]$end_labtime
  }
  else {
    # merge down
    merge_i <- i+1  
    
    bouts[merge_i,]$length <- bouts[merge_i,]$length + bouts[i,]$length
    bouts[merge_i,]$start_labtime <- bouts[i,]$start_labtime    
  }
  bouts
}


# Create chunks from bout data by merging conitnuous epoch types
chunk_epochs <- function(df) {
  df_iterator <- iter(df, by='row')
  
  # Initialize
  first_row <- nextElem(df_iterator)
  bouts <- data.frame(bout_type=first_row$epoch_type, length=1, start_labtime=first_row$labtime, end_labtime=first_row$labtime)
  
  # Make bouts
  for(i in 2:df_iterator$length) {
    row <- nextElem(df_iterator)
    
    
    if(bouts[nrow(bouts),]$bout_type == row$epoch_type) {
      # Same epoch type - add to existing row
      bouts[nrow(bouts),]$length <- bouts[nrow(bouts),]$length + 1
      bouts[nrow(bouts),]$end_labtime <- row$labtime
    }
    else {
      # different epoch - initialize new row
      bouts <- rbind(bouts, data.frame(bout_type=row$epoch_type, length=1, start_labtime=row$labtime, end_labtime=row$labtime))
    } 
  }
  
  bouts
}

# Merge undefined bouts into neighbors
merge_undefined_bouts <- function(bouts) {
  undefined_bouts <- which(bouts$bout_type == 'UNDEF')
  for(undef_i in undefined_bouts) {
    bouts <- merge_into_larger_neighbor(undef_i, bouts)
  }
  bouts <- bouts[undefined_bouts*-1,]
  bouts
}

# Combine neighbors that have identical types
combine_identical_neighbors <- function(bouts) {
  if(nrow(bouts) > 1) {
    remove_i <- c()
    for(i in 2:nrow(bouts)) {
      if(bouts[i,]$bout_type == bouts[i-1,]$bout_type) {
        bouts[i,]$length <- bouts[i,]$length + bouts[i-1,]$length
        bouts[i,]$start_labtime <- bouts[i-1,]$start_labtime
        remove_i <- append(remove_i, i-1)
      }
    }
    bouts[remove_i*-1,]      
  }
  else
    bouts
}

# Merge seed bouts 
#   seeds_to_merge holds indeces of bouts that will be merged
#   stop_seeds hold indeces of bouts that mark the end of a merging session
merge_seed_bouts <- function(remove_list, seeds_to_merge, stop_seeds,  bouts) {
  for(i in seeds_to_merge) {
    j <- i+1
    while(!(j %in% stop_seeds) && j <= nrow(bouts)) {
      bouts[i,]$end_labtime <- bouts[j,]$end_labtime
      bouts[i,]$length <- bouts[i,]$length + bouts[j,]$length
      remove_list <- append(remove_list, j)
      j <- j + 1
    }
  }

  list(bouts=bouts, remove_i=remove_list)
}


# Merge into final nrem and rem periods
create_nrem_rem_periods <- function(bouts) {
  # Find seed bouts
  nrem_seed_bouts <- which(bouts$length > NREM_MIN_PERIOD_LENGTH & bouts$bout_type == 'NREM')
  rem_seed_bouts <- which(bouts$length > REM_MIN_PERIOD_LENGTH & bouts$bout_type == 'REM')
  
  remove_i <- c()
  # First, merge NREM periods
  res <- merge_seed_bouts(remove_i, nrem_seed_bouts, rem_seed_bouts, bouts)
  bouts <- res$bouts
  remove_i <- res$remove_i
  # Merge REM periods after NREM
  res <- merge_seed_bouts(remove_i, rem_seed_bouts, nrem_seed_bouts, bouts)
  bouts <- res$bouts
  remove_i <- res$remove_i
  
  if(length(remove_i) > 0)
    bouts <- bouts[remove_i*-1,]
  
  bouts
}



# Used by ddply on chunked sleep data to compute consecutive chucks of a given epoch_type
compute_chunk_info <- function(chunk) {
  # Only returns sleep periods
  if(chunk$period_type[1] == 'sp') {
    # Computes the length of the chunk, and find the first and last labtimes
    data.frame(length=dim(chunk)[1], start_labtime=min(chunk$labtime), end_labtime=max(chunk$labtime))    
  }
}

# Not sure what this function does
start_end_times <- function(df) { c(min(df$day_labtime), max(df$day_labtime)) }

# Maps numerical values to types of epochs
map_epoch_type <- function(x) {
  if (x >= 1 & x <=4) { "NREM" }
  else if (x == 5) { "WAKE" }
  else if (x == 6) { "REM" }
  else { "UNDEF" }
}

# Uses the sleep data to determine the most frequent type of epoch in each chunk
# Creates bouts with start and end labtimes
calculate_bouts <- function(start_index=NA, end_index=NA, df=NA) {
  # get most frequent type
  c(names(sort(-table(df[start_index:end_index,]$epoch_type)))[1], df$labtime[start_index], df$labtime[end_index])
}


# Calculates the day number, wake or sleep period, and epoch type
set_up_data_frame <- function(df, t_cycle) {
  df$day_number <- floor(df$labtime / t_cycle)
  df$day_labtime <- (df$labtime - (df$day_number * t_cycle))
  df$day_number <- df$day_number - (min(df$day_number) - 1)
  df$period_type <- factor(df$sleep_wake_period < 0, labels=c("sp", "wp")) 
  
  # Label NREM, REM, WAKE, UNDEF
  df$epoch_type <- factor(sapply(df$stage, map_epoch_type))
  df
}

# Sets something up...
setup_intervals <- function(df, t_cycle) {
  
  # set day numbers + labtimes
  df$start_day_number <- floor(df$start / t_cycle)
  min_day_num <- min(df$start_day_number)  
  df$start_day_labtime <- (df$start - (df$start_day_number * t_cycle))
  df$start_day_number <- df$start_day_number - (min_day_num - 1)
  df$end_day_number <- floor(df$end / t_cycle)
  df$end_day_labtime <- (df$end - (df$end_day_number * t_cycle))
  df$end_day_number <- df$end_day_number - (min_day_num - 1)
  
  clean <- df[!df$start_day_number != df$end_day_number,]
  
  # clean bouts that span across days
  to_clean <- df[df$start_day_number != df$end_day_number,]
  # deal with bouts that start and end with day
  first_cleaned <- ddply(to_clean, .(start_day_number), first_div, t_cycle)
  second_cleaned <- ddply(to_clean, .(start_day_number), second_div)
  first_cleaned <- first_cleaned[, -1]
  second_cleaned <- second_cleaned[, -1]
  colnames(first_cleaned) <- colnames(clean)
  colnames(second_cleaned) <- colnames(clean)
  
  clean <- rbind(first_cleaned, second_cleaned, clean) #rbind(clean, first_cleaned, second_cleaned)
  # make numeric again =/
  # clean$start_day_number <- as.numeric(as.character(clean$start_day_number))
  clean$day_number <- clean$start_day_number
  # clean$day_number <- as.numeric(clean$day_number)
  # #clean$start <- as.numeric(clean$start)
  # #clean$end <- as.numeric(clean$end)
  # clean$start_day_labtime <- as.numeric(as.character(clean$start_day_labtime))
  # clean$end_day_labtime <- as.numeric(as.character(clean$end_day_labtime))
  clean
  #list(clean, first_cleaned, second_cleaned)
}



# Graphing ?
first_div <- function(x, t_cycle) {
  #c(I(x$bout_type), I(x$start), I(x$end), I(x$start_day_number), I(x$start_day_labtime), I(x$start_day_number), I(24.0-epoch_length))
  c(I(x$sleep_wake_period), I(x$start), I(x$end), I(x$length), I(x$start_day_number), I(x$start_day_labtime), I(x$start_day_number), I(t_cycle-epoch_length))  
}

second_div <- function(x) {
  #c(I(x$bout_type), I(x$start), I(x$end), I(x$end_day_number), I(0.00), I(x$end_day_number), I(as.numeric(x$end_day_labtime)))
  c(I(x$sleep_wake_period), I(x$start), I(x$end), I(x$length), I(x$end_day_number), I(0.00), I(x$end_day_number), I(as.numeric(x$end_day_labtime)))
}

double_plot <- function(df) {
  r_df <- df
  l_df <- df
  
  l_df$double_plot_pos <- 0
  r_df$double_plot_pos <- 1
  r_df$day_number <- r_df$day_number - 1
  
  return(rbind(l_df, r_df))
}

get_periods <- function(df) {
  # Get period start and end times
  periods <- ddply(df, .(period_type, day_number, sleep_wake_period, double_plot_pos), start_end_times)
  colnames(periods) <- c("period_type", "day_number", "sleep_or_wake_period", "start", "end")
  periods
}




