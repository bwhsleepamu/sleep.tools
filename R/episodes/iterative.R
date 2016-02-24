##################
## Iterative
##################
set_weight_stats <- function(label, length) {
  l <- list(l_1=0L,l_2=0L,l_34=0L,l_r=0L,l_w=0L)
  length <- as.integer(length)
  
  switch(as.character(label),
         '1' = {l$l_1 <- length},
         '2' = {l$l_2 <- length},
         '3' = {l$l_34 <- length},
         '4' = {l$l_34 <- length},
         '5' = {l$l_w <- length},
         '6' = {l$l_r <- length},
         '16' = {l$l_r <- length}
  )
  
  
  l
}

generate_episodes.iterative <- function(dt, min_nrem_length=30, min_rem_length=10, min_wake_length=10, undef_cutoff=2) {
  # Take series of epochs and collapse them into sequences of the same type  
  min_nrem_length=30
  min_rem_length=10
  min_wake_length=20
  undef_cutoff=2

  # Create Sequences
  sequences <- dt[, chunk(stage, pk), by='subject_code,activity_or_bedrest_episode']
  
  # Initialize epoch stats for each sequence
  sequences <- sequences[,set_weight_stats(label,length),by='subject_code,activity_or_bedrest_episode,label,start_position,end_position,length']
  
  # Initialize columns
  sequences[,protect_from_merge:=FALSE]
  
  # Group sequences by epoch type
  sequences[,label:=map_epoch_type(label),by='start_position']
  sequences[,group:=set_group(sequences$label)]
  sequences <- sequences[,merge_group.iterative(.SD), by='subject_code,activity_or_bedrest_episode,group']
  
  # Clean up short UNDEFs and protect long ones
  sequences[label == "UNDEF" & length > undef_cutoff, protect_from_merge:=TRUE]
  
  sequences <- remove_target_label.iterative(sequences, "UNDEF")  
  
  # First REM conversion
  sequences[label=="SREM", label:="REM"]
  
  episodes <- iterative_merge(sequences, min_nrem_length, min_rem_length, min_wake_length)
  
  episodes[,mean(length),by='label']
  
  episodes[,method:='iterative']
  episodes[,`:=`(l_1=NULL, l_2=NULL, l_34=NULL,l_r=NULL,l_w=NULL,protect_from_merge=NULL)]
  
  episodes
}

## Used in episodes (iterative)
# basically, go from smallest to largest, merging until non are below cutoff
iterative_merge <- function(sequences, min_nrem_length, min_rem_length, min_wake_length) {
  # Find First REM sequences in each sleep episode
  sequences[,first_nrem_position:=min(.SD[label=="NREM"]$start_position), .SDcols=c("label", "start_position"), by='subject_code,activity_or_bedrest_episode']
  sequences[label=="REM" & start_position > first_nrem_position, rem_id:=seq(1,.N),by='subject_code,activity_or_bedrest_episode']
  sequences[label == "REM" & rem_id==1,protect_from_merge:=TRUE]
  sequences[,`:=`(rem_id=NULL, first_nrem_position=NULL)]
  
  for(i in 1:max(min_nrem_length, min_rem_length, min_wake_length)) {
    print(paste("Merge iteration:", i))
    
    min_lengths <- list()
    min_lengths[["WAKE"]] <- min_wake_length
    min_lengths[["NREM"]] <- min_nrem_length
    min_lengths[["REM"]] <- min_rem_length
    for(type in c("REM", "WAKE", "NREM")) {
      sequences[,weight:=calculate_weights(label,length,l_1,l_2,l_34,l_r,l_w),by='start_position']
      if(i <= min_lengths[[type]]) {
        # Re-label
        sequences[,c('label','group'):=relabel_by_weight(type, i,label,length,weight,protect_from_merge),by='subject_code,activity_or_bedrest_episode']
        # Merge
        sequences <- sequences[,merge_group.iterative(.SD), by='subject_code,activity_or_bedrest_episode,group']
      }
    }

    sequences[,group:=NULL]  
    # Repeat!
  }
  sequences
}


## Used here in iterative merge
relabel_by_weight <- function(target_label, target_length, labels, lengths, weights, protected) {
#  print("------")
#   print(target_label)
#   print(target_length )
#   print(labels)
#   print(lengths)
#  print(weights)
  # Re-label
  targets <- which(lengths==target_length & labels==target_label & !protected)
#  print(targets)

  if(length(targets) > 0) {
    # Get neighboring lengths
    prev_weights <- weights[targets-1]
    next_weights <- weights[targets+1]
    
    prev_weights <- prev_weights[!is.na(prev_weights)]
    next_weights <- next_weights[!is.na(next_weights)]
    
    # Adjust if neighbors are on the ends of list
    if(length(prev_weights) < length(targets))
      prev_weights <- c(-1.0, prev_weights)
    if(length(next_weights) < length(targets))
      next_weights <- c(next_weights, -1.0)
    
    # Determine which neighbor to merge into
#    print(prev_weights)
#    print(next_weights)
    merge_direction <- as.integer(as.vector(factor((prev_weights >= next_weights), c(TRUE, FALSE), c(-1,1))))
#    print(merge_direction)
    # Get index of neighbors to merge into
    sources <- targets + merge_direction
#    print(sources)
    # Re-label to neighbors
    if(length(labels[sources]) > 0)
      labels[targets] <- labels[sources]    
  }
  
  # Set groups based on new label values
  groups <- set_group(labels)  
  
  data.table(label=labels, group=groups)  
}

merge_group.iterative <- function(d) {
  data.table(start_position=min(d$start_position), 
             end_position=max(d$end_position), 
             label=names(which.max(table(d$label))), 
             length=sum(d$length),
             l_1=sum(d$l_1),
             l_2=sum(d$l_2),
             l_34=sum(d$l_34),
             l_r=sum(d$l_r),
             l_w=sum(d$l_w),
             protect_from_merge=any(d$protect_from_merge))  
}



calculate_weights <- function(label, length, l_1, l_2, l_34, l_r, l_w) {
  r <- switch(label,
         NREM = l_34,
         REM = 2*l_r,
         WAKE = l_w,
         0)
  #r <- length
  data.table(weight=as.double(r))
}

remove_target_label.iterative <- function(sequences, target_label) {
  # Re-label
  sequences[,label:=relabel_to_neighbor.iterative(target_label, .SD),by='subject_code,activity_or_bedrest_episode']
  
  # Group by labels
  sequences[,group:=set_group(label),by='subject_code,activity_or_bedrest_episode']
  
  # Collapse groups
  sequences <- sequences[, merge_group.iterative(.SD), by='subject_code,activity_or_bedrest_episode,group']
  
  sequences[,group:=NULL]
  
  sequences
}

relabel_to_neighbor.iterative <- function(label_to_merge, d) {
  weights <- d[,calculate_weights(label, length, l_1, l_2, l_34, l_r, l_w),by='start_position']$weight
  labels <- d$label
  i <- which(labels==label_to_merge & !d$protect_from_merge)
  if(length(i)!=0L & length(labels) > 1) {
    r <- merge_label.iterative(labels, weights, i, label_to_merge)
    f <- i + r
    labels[i] = labels[f]    
  }
  labels
}

merge_label.iterative <- function(labels, weights, i, label_to_merge) {
#   print(labels)
#   print(sprintf("I: %s", i))
  mapply(determine_merge_direction.iterative, i-1, i+1, MoreArgs=list(labels, weights))
}


determine_merge_direction.iterative <- function(b,a,labels,weights){  
#   print("HAAA")
#   print(sprintf("label_a: %s\n", labels[a]))
#   print(sprintf("label_b: %s\n", labels[b]))
#   print(sprintf("length_a: %s\n", lengths[a]))
#   print(sprintf("length_b: %s\n", lengths[b]))
#   print(sprintf("%s | %s || %s | %s"), labels[a], labels[b], lengths[a], lengths[b])
#   
  if(length(labels[b])==0L || is.na(labels[b]))
    return(1)
  if(length(labels[a])==0L || is.na(labels[a]))
    return(-1)
  if(weights[a] >= weights[b])
    return(1)
  else
    return(-1)
}

weight_map <- function(stage) {
  switch(as.character(stage),
         '1' = .25,
         '2' = .5,
         '3' = 1,
         '4' = 1,
         '5' = 1,
         '6' = 1,
         {0.0})
}

