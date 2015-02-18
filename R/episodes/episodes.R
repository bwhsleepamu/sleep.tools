source("R/episodes/classic.R")
source("R/episodes/changepoint.R")
source("R/episodes/iterative.R")

## HELPERS
label_wake <- function(dt) {
  
  sleep_onset <- min(match(c("NREM", "REM"), dt$label, nomatch=1L))
  sleep_end <- (length(dt$label) - min(match(c("NREM", "REM"), rev(dt$label), nomatch=1L))) + 1L
  
  dt[sleep_onset:sleep_end]
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

## USED IN EPISODES
merge_epochs <- function(pks, labels) {
  start_position <- min(pks)
  end_position <- max(pks)
  list(start_position=start_position, end_position=end_position, label=names(which.max(table(labels))), length=(end_position - start_position + 1))
}

## USED IN EPISODES
merge_group <- function(start_positions, end_positions, labels, lengths) {
  list(start_position=min(start_positions), end_position=max(end_positions), label=names(which.max(table(labels))), length=sum(lengths))  
}

# USED HERE IN MERGE LABEL
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

# USED HERE in relabel_to_biggest_neighbor
merge_label <- function(labels, lengths, label_to_merge) {
  i <- which(labels==label_to_merge)
  #  cat(sprintf("I: %s", i))
  mapply(determine_merge_direction, i-1, i+1, MoreArgs=list(labels, lengths))
}


# USED HERE in remove.target.label.dt
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
# USED HERE in remove.target.label.dt, merge_around_seeds, relabel_by_length
set_group <- function(labels) {
  n <- length(labels)
  label_changed <- labels[-1L] != labels[-n]
  change_locations <- c(which(label_changed | is.na(label_changed)), n)
  lengths <- diff(c(0L, change_locations))
  groups <- rep.int(seq(1,length(lengths)), lengths)  
  groups
}


## Get rid of sequences with target label
## USED IN ALL EPISODES!!
remove.target.label <- function(sequences, target_label='UNDEF') {
  # Re-label undefs
  sequences[,label:=relabel_to_biggest_neighbor(label,length,target_label),by='subject_code,activity_or_bedrest_episode']
  # Group by labels
  sequences[,group:=set_group(label),by='subject_code,activity_or_bedrest_episode']
  # Collapse groups
  sequences <- sequences[,merge_group(start_position, end_position, label, length), by='subject_code,activity_or_bedrest_episode,group']
  sequences[,group:=NULL]
  
  sequences
  
}



setup_episodes <- function() {
  
  ### NREM and REM Episodes
  ## Calculate episodes using different methods.
  ######## Classic
  episodes.classic <- generate_episodes.classic(sleep_data, wake=FALSE, min_nrem_length=mnl, min_rem_length=mrl, min_wake_length=mwl)
  ######## Iterative
  episodes.iterative <- generate_episodes.iterative(sleep_data, wake=TRUE, undef=FALSE, min_nrem_length=mnl, min_rem_length=mrl, min_wake_length=mwl)
  ######## Changepoint
  episodes.changepoint <- generate_episodes.changepoint(sleep_data, wake=TRUE, undef=FALSE, cpmType=cpmType, ARL0=ARL0, startup=startup)
  ## Merge methods into one table
  episodes <<- rbindlist(list(episodes.changepoint,episodes.classic,episodes.iterative))
  # Get rid of wake episodes
  episodes <<- episodes[activity_or_bedrest_episode > 0]
  # Merge with information about each episode
  setkey(sleep_data,pk)
  episodes <<- merge(episodes, subjects, all.x=TRUE, all.y=FALSE, by='subject_code')
  episodes[,`:=`(start_labtime=sleep_data[start_position]$labtime, end_labtime=sleep_data[end_position]$labtime)]
  episodes[,schedule_label:=label_by_schedule(start_labtime, end_labtime, start_analysis, end_analysis)]  
  
  episodes <<- merge(episodes,sleep_efficiency,all.x=TRUE, all.y=FALSE)
  episodes[,agreement:=(sum(sleep_data[start_position:end_position]$epoch_type == label)/length),by=id]
  
  ### sleep_episodes and bedrest_episodes
  # Get Sleep Episodes (from sleep onset) and Bedrest episodes
  bedrest_episodes <<- sleep_data[activity_or_bedrest_episode>0,list(start_position=min(.I), end_position=max(.I), start_labtime=min(labtime), end_labtime=max(labtime)),by='subject_code,activity_or_bedrest_episode']
  sleep_episodes <<- copy(bedrest_episodes)
  sleep_episodes[,sleep_onset:=get_first_stage(sleep_data$stage[start_position:end_position], start_position, 2, not_found=start_position), by='subject_code,activity_or_bedrest_episode']
  sleep_episodes[,`:=`(start_position=sleep_onset, sleep_onset=NULL)]
  #sleep_episodes <- sleep_episodes[!is.na(start_position)]
  
  # Join subject data with episodes and cycles
  setkey(bedrest_episodes, subject_code)
  setkey(sleep_episodes, subject_code)
  bedrest_episodes <<- bedrest_episodes[fd_times]
  sleep_episodes <<- sleep_episodes[fd_times]
  
  # Label FD, recovery, baseline
  sleep_episodes[,schedule_label:=label_by_schedule(start_labtime, end_labtime, start_analysis, end_analysis)]
  bedrest_episodes[,schedule_label:=label_by_schedule(start_labtime, end_labtime, start_analysis, end_analysis)]
  
  # Label by sleep efficiency
  setkey(bedrest_episodes, subject_code, activity_or_bedrest_episode)
  setkey(sleep_episodes, subject_code, activity_or_bedrest_episode)
  sleep_episodes <<- sleep_episodes[sleep_efficiency]
  bedrest_episodes <<- bedrest_episodes[sleep_efficiency]
  
  # Fix for NAs in start or end positions
  sleep_episodes <<- sleep_episodes[!is.na(start_position) & !is.na(end_position)]
  bedrest_episodes <<- bedrest_episodes[!is.na(start_position) & !is.na(end_position)]
  
  # Label SLEEP/WAKE parts of sleep period (from sleep onset to wake)
  episodes[,first_sleep_id:=id[min(match(c("NREM", "REM"), label, nomatch=length(l)))],by='subject_code,activity_or_bedrest_episode,method']
  episodes[,last_sleep_id:=id[length(label) - min(match(c("NREM", "REM"), rev(label), nomatch=length(l))) + 1L],by='subject_code,activity_or_bedrest_episode,method']
  episodes[,sleep_wake_label:="WAKE"]
  episodes[id <= last_sleep_id & id >= first_sleep_id, sleep_wake_label:="SLEEP"]  
}
