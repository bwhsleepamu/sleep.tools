##################
## Iterative
##################
generate_episodes.iterative <- function(dt, min_nrem_length=30, min_rem_length=10, min_wake_length=10) {
  # Take series of epochs and collapse them into sequences of the same type  
  min_nrem_length=30
  min_rem_length=10
  min_wake_length=10
  
  sequences <- dt[, chunk(epoch_type, pk), by='subject_code,activity_or_bedrest_episode']
  
  sequences <- remove.target.label(sequences, target_label="UNDEF")  
  
  episodes <- iterative_merge(sequences, min_nrem_length, min_rem_length, min_wake_length)
  
  episodes[,method:='iterative']
  
  episodes
}

## Used in episodes (iterative)
# basically, go from smallest to largest, merging until non are below cutoff
iterative_merge <- function(sequences, min_nrem_length, min_rem_length, min_wake_length) {
  for(i in 1:max(min_nrem_length, min_rem_length, min_wake_length)) {
    print(i)
    # Re-label
    if(i <= min_wake_length) {
      sequences[,c('label','group'):=relabel_by_length("WAKE", i,label,length),by='subject_code,activity_or_bedrest_episode']
      sequences <- sequences[,merge_group(start_position, end_position, label, length), by='subject_code,activity_or_bedrest_episode,group']
    }
    if(i <= min_rem_length) {
      sequences[,c('label','group'):=relabel_by_length("REM", i,label,length),by='subject_code,activity_or_bedrest_episode']
      sequences <- sequences[,merge_group(start_position, end_position, label, length), by='subject_code,activity_or_bedrest_episode,group']      
    }
    if(i <= min_nrem_length) {
      sequences[,c('label','group'):=relabel_by_length("NREM", i,label,length),by='subject_code,activity_or_bedrest_episode']
      sequences <- sequences[,merge_group(start_position, end_position, label, length), by='subject_code,activity_or_bedrest_episode,group']
    }

    # Merge
    sequences[,group:=NULL]  
    # Repeat!
  }
  
  sequences
}


## Used here in iterative merge
relabel_by_length <- function(target_label, target_length, labels, lengths) {
#   print("------")
#   print(target_label)
#   print(target_length )
#   print(labels)
#   print(lengths)
  # Re-label
  targets <- which(lengths==target_length & labels==target_label)

  if(length(targets) > 0) {
    # Get neighboring lengths
    prev_lengths <- lengths[targets-1]
    next_lengths <- lengths[targets+1]
    
    prev_lengths <- prev_lengths[!is.na(prev_lengths)]
    next_lengths <- next_lengths[!is.na(next_lengths)]
    
    # Adjust if neighbors are on the ends of list
    if(length(prev_lengths) < length(targets))
      prev_lengths <- c(0L, prev_lengths)
    if(length(next_lengths) < length(targets))
      next_lengths <- c(next_lengths, 0L)
    
    # Determine which neighbor to merge into
    merge_direction <- as.integer(as.vector(factor((prev_lengths >= next_lengths), c(TRUE, FALSE), c(-1,1))))
        
    # Get index of neighbors to merge into
    sources <- targets + merge_direction
    # Re-label to neighbors
    if(length(labels[sources]) > 0)
      labels[targets] <- labels[sources]    
  }
  
  # Set groups based on new label values
  groups <- set_group(labels)  
  
  data.table(label=labels, group=groups)  
}

