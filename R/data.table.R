# MAIN FUNCTION

## Load Sleep Stage Data
load_sleep_data.dt <- function(subjects) {
  #sleep_data <- subjects[, load_sleep_file.dt(file_path), by=subject_code]
  sleep_data <- rbindlist(lapply(subjects$file_path, load_sleep_file.dt))
  #sleep_data[,V1:=NULL]
  setnames(sleep_data, c('subject_code', 'activity_or_bedrest_episode', 'labtime', 'stage'))
  setkey(sleep_data, subject_code, labtime)
  # Generate row indeces
  sleep_data[, pk:=.I]
  # Map stages to epoch types
  sleep_data[,epoch_type:=as.factor(as.character(lapply(stage, map_epoch_type))),]
  sleep_data
}


#################
## Classic Bouts
#################
generate.episodes.classic.dt <- function(dt, wake=FALSE, undef=FALSE, min_nrem_length=NULL, min_rem_length=NULL, min_wake_length=NULL) {
  sequences <- dt[, chunk(epoch_type, pk), by='subject_code,activity_or_bedrest_episode']
  
  if(!undef)
    sequences <- remove.target.label.dt(sequences, target_label="UNDEF")    
  
  # Merge around seeds
  sequences[,c('label', 'group'):=merge_around_seeds(label, length, wake=wake, min_nrem_length=min_nrem_length, min_rem_length=min_rem_length, min_wake_length=min_wake_length), by='subject_code,activity_or_bedrest_episode']

  # Collapse groups
  episodes <- sequences[,merge_group(start_position, end_position, label, length), by='subject_code,activity_or_bedrest_episode,group']
  episodes[,`:=`(group=NULL, method='classic')]
  
  episodes
}

##################
## Iterative Bouts
##################
generate.episodes.iterative.dt <- function(dt, wake=TRUE, undef=FALSE, min_nrem_length=NULL, min_rem_length=NULL, min_wake_length=NULL) {
  sequences <- dt[, chunk(epoch_type, pk), by='subject_code,activity_or_bedrest_episode']
  
  if(!undef)
    sequences <- remove.target.label.dt(sequences, target_label="UNDEF")  
  if(!wake)
    sequences <- remove.target.label.dt(sequences, target_label="WAKE")
  
  episodes <- iterative_merge(sequences)
  episodes[,method:='iterative']
  
  episodes
}

####################
## Changepoint Bouts
####################
generate.episodes.changepoint.dt <- function(dt, wake=TRUE, undef=FALSE, cpmType="Mann-Whitney", ARL0=10000, startup=20) {
  dt[,group:=set_changepoint_group(epoch_type,cpmType=cpmType,ARL0=ARL0,startup=startup),by='subject_code,activity_or_bedrest_episode']
  episodes <- dt[,merge_epochs(pk,epoch_type),by='subject_code,activity_or_bedrest_episode,group']
  episodes[,group:=NULL]
  
  if(!undef)
    episodes <- remove.target.label.dt(episodes, target_label="UNDEF")  
  if(!wake)
    episodes <- remove.target.label.dt(episodes, target_label="WAKE")
  
  episodes[, method:='changepoint']
  episodes
}

###############
## Sleep Cycles
###############
# Either REM or NREM
# For NREM: start at first stage 2 of NREM cycle
find.cycles <- function(dt, sleep_data, type="NREM", start_fn=find_nrem_start, until_end=TRUE) {
  episodes <- copy(dt)
  setkey(episodes, label)
  stages <- copy(sleep_data$stage)
  setkey(sleep_data, subject_code, activity_or_bedrest_episode)
  episodes[,pik:=.I]
  episodes[, last_position:=last_se_position(sleep_data, subject_code, activity_or_bedrest_episode), by='subject_code,activity_or_bedrest_episode']
  episodes[type, cycle_start:=start_fn(stages[start_position:end_position], start_position), by=pik]
  cycles <- episodes[type,find_cycles_in_sleep_episode(cycle_start, last_position, until_end),by='subject_code,activity_or_bedrest_episode,method']  
  cycles[,cycle_number:=seq(.N),by='subject_code,activity_or_bedrest_episode,method']
  setkey(sleep_data, pk)
  cycles
}

# Get last position of a sleep episode
last_se_position <- function(sd, sc, e) {
  cat(sc, " ", e, "\n")
  max(sd[c(sc, e)]$pk)
  
  #max(sd[subject_code==sc & activity_or_bedrest_episode==e]$pk)
}

# First occurance of Stage 2
find_nrem_start <- function(stages, start_position) {
  get_first_stage(stages, start_position, 2)
}

find_rem_start <- function(stages, start_position) {
  get_first_stage(stages, start_position, 6)
}
# 
# find.nrem.cycles <- function(dt, sleep_data) {
#   episodes <- copy(dt)
#   setkey(episodes, label)
#   stages <- sleep_data$stage
#   episodes[,pik:=.I]
#   episodes["NREM",first_stage_2:=get_first_stage(stages,start_position,end_position,2), by=pik]
#   cycles <- episodes["NREM",find_cycles_in_sleep_episode(first_stage_2),by='subject_code,activity_or_bedrest_episode,method']  
#   cycles[,cycle_number:=seq(.N),by='subject_code,activity_or_bedrest_episode,method']
#   cycles
# }
# 
# 
# find.rem.cycles <- function(dt, sleep_data) {
#   periods <- copy(dt)
#   setkey(periods, label)
#   stages <- sleep_data$stage
#   periods[,pik:=.I]
#   #periods["REM", rem_start:=start_position,2), by=pik]
#   cycles <- periods["REM",find_cycles_in_sp(start_position),by='subject_code,activity_or_bedrest_episode,method']  
#   cycles[,cycle_number:=seq(.N),by='subject_code,activity_or_bedrest_episode,method']
#   cycles
# }


## Read Subject Info
read.subject_info <- function(file_path) {
  subjects <- fread(file_path)
  subjects[,file_exists:=file.exists(file_path)]
  setkey(subjects, subject_code)

  subjects
}


load_sleep_file.dt <- function(file_path) {  
  use <- TRUE
  results <- data.table(character())
  
  if(file.exists(file_path)) {
    tryCatch(
      results<-fread(file_path), 
      error = function(e) { print(paste("ERROR!", file_path)); use <- FALSE }, 
      warning = function(w) {print(paste("WARNING!", file_path)); use <- FALSE}, 
      finally = {
        if(ncol(results) == 4)
          print(paste(file_path, ncol(results)))
        else
          use <- FALSE
      }
    )
  
  }
  
  if(use) {
    results
  } else {
    NULL
  }

}

# Strip all periods until first NREM
strip_until_sleep_onset <- function(dt) {
  #print(typeof(dt))
  min_labtime <- dt[bout_type=="NREM", min(start_labtime)]
    
  nrow(dt) - nrow(dt[start_labtime >= min_labtime])
      
}


## BOXPLOTS



## Every combo of subject_code and method needs to have: NREM, REM, WAKE, UNDEF
generate_missing_combinations <- function(bout_type, by) {
  if(TRUE %in% !(levels(bout_type) %in% unique(bout_type))) {
    # cat(sprintf("%s | %s\n", by[[1]], levels(bout_type)[!(levels(bout_type) %in% unique(bout_type))]))
    data.table(bout_type=levels(bout_type)[!(levels(bout_type) %in% unique(bout_type))],activity_or_bedrest_episode=c(0), start_labtime=c(-1), end_labtime=c(-1), length=c(-1))
#     lapply(, function(level) {
#       (bout_type=level, )
#     })
  }
}

## Set up data for transformation
set_min_day_num <- function(subjects, sleep_data) {
  r <- sleep_data[,(min(floor(labtime / T_CYCLE)) - 1),by=subject_code]
  setnames(r, c('subject_code', 'min_day_number'))

  merge(subjects, r, all.x=TRUE)           
}

set_up_days <- function(labtime, min_day_number, t_cycle) {
  day_number <- floor(labtime / t_cycle)
  day_labtime <- labtime - (day_number * t_cycle)
  day_number <- day_number - min_day_number
  
  list(day_number, day_labtime)
}

 
### ACTUAL HELPERS!!
# Maps numerical values to types of epochs
map_epoch_type <- function(x) {
  ## Possibly speed up if x is a factor??
  if (x >= 1 & x <=4) { res <- "NREM" }
  else if (x == 5) { res <- "WAKE" }
  else if (x == 6) { res <- "REM" }
  else { res <- "UNDEF" }
  
  res
}

chunk <- function(categories, indeces) {
  ## Might be possible to elaborate on this function using the code for rle
  
  reference = indeces[1] - 1
  rle_results <- rle(as.character(categories))
  
  # Get positions by cumulative sum of lengths
  positions <- cumsum(rle_results$lengths)
  
  # The ending positions for each chunk are represented by the cumsum of lengths.
  # The only correction we need is for the reference index
  
  # Since each following chunk starts one position after the end of the previous chunk
  # we calculate the start positions by adding 1 to each end position and artificially inserting
  # the start position of the first chunk==1. 
  
  # Again, we correct for the reference index, and also trim to get rid of the last value
  # (since it references a bout that does not exist). 
  
  start_positions <- (c(0, positions)+1)[1:length(positions)] + reference
  end_positions <- positions + reference
  
  #end_positions <- res$lengths
  
  data.table(label=rle_results$values, start_position=start_positions, end_position=end_positions, length=rle_results$lengths)
}


### CLASSIC

merge_label <- function(labels, lengths, label_to_merge) {
  i <- which(labels==label_to_merge)
  #  cat(sprintf("I: %s", i))
  mapply(determine_merge_direction, i-1, i+1, MoreArgs=list(labels, lengths))
}



# determine_merge_direction <- function(b,a,labels,lengths){  
#   #   cat(sprintf("label_a: %s\n", labels[a]))
#   #   cat(sprintf("label_b: %s\n", labels[b]))
#   #   cat(sprintf("length_a: %s\n", lengths[a]))
#   #   cat(sprintf("length_b: %s\n", lengths[b]))
#   #  cat(sprintf("%s | %s || %s | %s"), labels[a], labels[b], lengths[a], lengths[b])
#   if(length(labels[b])==0L || is.na(labels[b]))
#     return(1)
#   if(length(labels[a])==0L || is.na(labels[a]))
#     return(-1)
#   if(labels[a]==labels[b])
#     return(0)
#   if(lengths[a] >= lengths[b])
#     return(1)
#   else
#     return(-1)
# }

determine_merge_direction <- function(b,a,labels,lengths){  
  #   cat(sprintf("label_a: %s\n", labels[a]))
  #   cat(sprintf("label_b: %s\n", labels[b]))
  #   cat(sprintf("length_a: %s\n", lengths[a]))
  #   cat(sprintf("length_b: %s\n", lengths[b]))
  #  cat(sprintf("%s | %s || %s | %s"), labels[a], labels[b], lengths[a], lengths[b])
  if(length(labels[b])==0L || is.na(labels[b]))
    return(1)
  if(length(labels[a])==0L || is.na(labels[a]))
    return(-1)
  if(lengths[a] >= lengths[b])
    return(1)
  else
    return(-1)
}

#
relabel_to_biggest_neighbor <- function(labels, lengths, label_to_merge) {
  i <- which(labels==label_to_merge)
  if(length(i)!=0L) {
    r <- merge_label(labels, lengths, label_to_merge)
    f <- i + r
    labels[i] = labels[f]    
  }
  labels
}

# Sets groups according to consecutive same labels
set_group <- function(labels) {
  n <- length(labels)
  label_changed <- labels[-1L] != labels[-n]
  change_locations <- c(which(label_changed | is.na(label_changed)), n)
  lengths <- diff(c(0L, change_locations))
  groups <- rep.int(seq(1,length(lengths)), lengths)  
  groups
}

merge_epochs <- function(pks, labels) {
  start_position <- min(pks)
  end_position <- max(pks)
  list(start_position=start_position, end_position=end_position, label=names(which.max(table(labels))), length=(end_position - start_position + 1))
}
merge_group <- function(start_positions, end_positions, labels, lengths) {
  list(start_position=min(start_positions), end_position=max(end_positions), label=names(which.max(table(labels))), length=sum(lengths))  
}

# For classic!
merge_around_seeds <- function(labels, lengths, wake=FALSE, min_wake_length=10, min_rem_length=10, min_nrem_length=30) {
  # Find seeds
  seed_nrem <- intersect(which(labels=='NREM'), which(lengths >= min_nrem_length))
  seed_rem <- intersect(which(labels=='REM'), which(lengths >= min_rem_length))
  if(wake) {
    seed_wake <- intersect(which(labels=='WAKE'), which(lengths >= min_wake_length))    
  }
  else
    seed_wake <- c()
  seeds <- sort(c(seed_nrem, seed_rem, seed_wake))
  
  if(length(seeds) > 0) {
    # wake until first NREM!
    if(length(seed_nrem) > 0) {
      first_nrem <- min(seed_nrem)    
      if(first_nrem > 1) {
        labels[1] = 'WAKE'
        seeds <- c(1L, seeds[which(seeds >= first_nrem)])
      }
    }
    else {
      # if no first NREM, fall back on just initializing
      if(min(seeds) > 1) {
        labels[1] = 'WAKE'
        seeds <- c(1L, seeds)
      }        
    }
    
    
    group_lengths <- diff(c(seeds, (length(labels)+1)))
    new_labels <- rep.int(labels[seeds], group_lengths)
    
    
    groups <- set_group(new_labels)
    
    #new_labels
    
    
    #groups <- rep.int(seq(1L, length(group_lengths)), group_lengths)
    
  }
  else {
    #new_labels <- rep.int(labels[seeds], group_lengths)
    groups <- rep.int(1L, length(labels))#rep.int(seq(1, length(group_lengths)), group_lengths)
    new_labels <- labels
    # list(labels=labels, groups=groups)
    #    list(labels=labels, groups=rep.int(1, length(labels)))  
    
  }  
  list(labels=new_labels, groups=groups)
}

relabel_by_length <- function(target_length, labels, lengths) {
  # Re-label
  targets <- which(lengths==target_length)
  if(length(targets) > 0) {
    prev_lengths <- lengths[targets-1]
    next_lengths <- lengths[targets+1]
    
    if(length(prev_lengths) < length(targets))
      prev_lengths <- c(0L, prev_lengths)
    if(length(next_lengths) < length(targets))
      next_lengths <- c(next_lengths, 0L)
    
    merge_direction <- as.integer(as.vector(factor((prev_lengths >= next_lengths), c(TRUE, FALSE), c(-1,1))))
    sources <- targets + merge_direction
    labels[targets] <- labels[sources]
    
  }
  
  # Group
  groups <- set_group(labels)  

  list(label=labels, group=groups)  
}

iterative_merge <- function(sequences, min_nrem_length=30, min_rem_length=10) {
  for(i in 1:min(min_nrem_length, min_rem_length)) {
    
    # Re-label
    sequences[,c('label','group'):=relabel_by_length(i,label,length),by='subject_code,activity_or_bedrest_episode']
    
    # Merge
    sequences <- sequences[,merge_group(start_position, end_position, label, length), by='subject_code,activity_or_bedrest_episode,group']
    sequences[,group:=NULL]  
    # Repeat!
  }
  
  sequences
}

set_changepoint_group <- function(epoch_type, cpmType="Mann-Whitney", ARL0=10000, startup=20) {
  changepoints <- processStream(epoch_type, cpmType=cpmType, ARL0=ARL0, startup=startup)$changePoints
  
  # Add end of last group
  if(!length(epoch_type)%in%changepoints)
    changepoints <- c(changepoints, length(epoch_type))
  
  # Get lengths of each group
  lengths <- diff(c(0L, changepoints))
  
  # Label groups
  rep.int(seq(1,length(lengths)), lengths)
}


## Get rid of sequences with target label
remove.target.label.dt <- function(sequences, target_label='UNDEF') {
  # Re-label undefs
  sequences[,label:=relabel_to_biggest_neighbor(label,length,target_label),by='subject_code,activity_or_bedrest_episode']
  # Group by labels
  sequences[,group:=set_group(label),by='subject_code,activity_or_bedrest_episode']
  # Collapse groups
  sequences <- sequences[,merge_group(start_position, end_position, label, length), by='subject_code,activity_or_bedrest_episode,group']
  sequences[,group:=NULL]
  
  sequences
  
}


# get index (for main sleep data) of first instance of a given stage in a given list of stages
get_first_stage <- function(stages, start_position, target, not_found=NA) {
  #print(cat(stages))
  first_occurance <- (which(stages == target)[1] - 1 + start_position)
  
  if(is.na(first_occurance)) {
    first_occurance <- not_found
  }
   
  as.integer(first_occurance)
}

# Calculate starts and ends of NREM cycles for a given sleep period
#   If a nrem period does not include a stage 2 instance, it is ignored!
find_cycles_in_sleep_episode <- function(border_locations, sleep_episode_end, include_end=FALSE) {
  #border_locations <- border_locations[-which(is.na(border_locations))]
#   
#   cat(dim(sleep_data[J(subject_code, activity_or_bedrest_episode)]))
#   
  #sleep_episode_end <- max(sleep_data[J(subject_code, activity_or_bedrest_episode)]$pk)
  sleep_episode_end <- max(sleep_episode_end)
  
  if(include_end)
    border_locations <- c(border_locations, sleep_episode_end)
  
  if(any(is.na(border_locations)))
    border_locations <- border_locations[-which(is.na(border_locations))]
  
  starts <- border_locations[-length(border_locations)]
  ends <- border_locations[-1L]-1
  lengths <- ends - starts + 1
  list(start_position=starts, end_position=ends, length=lengths)  
}
