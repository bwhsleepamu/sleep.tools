##################
## Iterative
##################
generate_episodes.iterative <- function(dt, wake=TRUE, undef=FALSE, min_nrem_length=NULL, min_rem_length=NULL, min_wake_length=NULL) {
  # Take series of epochs and collapse them into sequences of the same type  
  sequences <- dt[, chunk(epoch_type, pk), by='subject_code,activity_or_bedrest_episode']
  
  if(!undef)
    sequences <- remove.target.label(sequences, target_label="UNDEF")  
  if(!wake)
    sequences <- remove.target.label(sequences, target_label="WAKE")
  
  episodes <- iterative_merge(sequences)
  
  episodes[,method:='iterative']
  
  episodes
}

## Used here in iterative merge
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

## Used in episodes (iterative)
iterative_merge <- function(sequences, min_nrem_length=30, min_rem_length=10) {
  for(i in 1:min(min_nrem_length, min_rem_length)) {
    print(i)
    
    # Re-label
    sequences[,c('label','group'):=relabel_by_length(i,label,length),by='subject_code,activity_or_bedrest_episode']
    
    # Merge
    sequences <- sequences[,merge_group(start_position, end_position, label, length), by='subject_code,activity_or_bedrest_episode,group']
    sequences[,group:=NULL]  
    # Repeat!
  }
  
  sequences
}
