##################
## Classic V1
##################

generate_episodes.classic <- function(dt, wake=FALSE, undef=FALSE, min_nrem_length=NULL, min_rem_length=NULL, min_wake_length=NULL) {
  # Take series of epochs and collapse them into sequences of the same type  
  sequences <- dt[, chunk(epoch_type, pk), by='subject_code,activity_or_bedrest_episode']
  
  # Re-label undefined sequences as their biggest neighbor
  ## TODO: NAs show up!
  if(!undef)
    sequences <- remove.target.label.dt(sequences, target_label="UNDEF")    
  
  # Merge around seeds by setting a label and group number for each sequence.
  sequences[,c('label', 'group'):=merge_around_seeds(label, length, wake=wake, min_nrem_length=min_nrem_length, min_rem_length=min_rem_length, min_wake_length=min_wake_length), by='subject_code,activity_or_bedrest_episode']
  
  # Collapse all sequences within a group number into one episode
  episodes <- sequences[,merge_group(start_position, end_position, label, length), by='subject_code,activity_or_bedrest_episode,group']
  episodes[,`:=`(group=NULL, method='classic')]
  
  episodes
}

##################
## Classic V2
##################

### THIS IS A METHOD THAT JUMPS STRAIGHT TO CYCLES

classic_episodes <- function(dt, min_nrem_length=30, min_rem_length=10, completion_cuttoff=10) {
  # Take series of epochs and collapse them into sequences of the same type  
  sequences <- dt[, chunk(epoch_type, pk), by='subject_code,activity_or_bedrest_episode']
  
  # Re-label undefined sequences as their biggest neighbor
  ## TODO: NAs show up!
  sequences <- sequences[label %in% c("NREM", "REM")]
    
  ## Combine neigboring sequences of same type
  sequences <- sequences[,merge_same_neighbors(.SD),by='subject_code,activity_or_bedrest_episode']
  
  ## Number sequences in each bedrest episode
  sequences[,`:=`(seq_id=seq(1,.N),seq_num=.N),by='subject_code,activity_or_bedrest_episode,label']
  
  ## Tag last sequence that can signify completion
  sequences[, last:=FALSE]
  View(sequences[length >= completion_cuttoff])
  
  
  
  ## Keep sequences above thresholds
  sequences[,keep:=FALSE]
  sequences[label=="NREM" & length >= min_nrem_length, keep:=TRUE]
  sequences[label == "REM" & (length >= min_rem_length | seq_num == 1), keep:=TRUE]
  
  
  
  episodes <- copy(sequences[keep==TRUE])
  episodes[,`:=`(group=NULL,seq_num=NULL,keep=NULL)]
  
  episodes <- r <- episodes[,merge_same_neighbors(.SD),by='subject_code,activity_or_bedrest_episode']
  
  episodes
  
  #   
  #   
  #   # Merge around seeds by setting a label and group number for each sequence.
  #   sequences[,c('label', 'group'):=merge_around_seeds(label, length, wake=wake, min_nrem_length=min_nrem_length, min_rem_length=min_rem_length, min_wake_length=min_wake_length), by='subject_code,activity_or_bedrest_episode']
  #   
  #   # Collapse all sequences within a group number into one episode
  #   episodes <- sequences[,merge_group(start_position, end_position, label, length), by='subject_code,activity_or_bedrest_episode,group']
  #   episodes[,`:=`(group=NULL, method='classic')]
  #   
  #   episodes
}

generate_cycles.classic.strict <- function(dt, ) {
  
}

merge_same_neighbors <- function(dt) {
  
  n <- nrow(dt)
  y <- dt$label[-1L] != dt$label[-n]
  i <- c(which(y | is.na(y)), n)
  
  diffs <- diff(c(0L, i))
  values <- labels[i]
  
  result <- copy(dt)
  result[,group:=rep(seq(1,length(values)), diffs)]
  result <- result[,list(start_position=min(start_position), end_position=max(end_position), length=sum(length)), by='group']
  result[,label:=values]
  
  result
}


