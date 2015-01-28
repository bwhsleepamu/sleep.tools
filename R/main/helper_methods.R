## USED IN CYCLES AND EPISODES
label_by_schedule <- function(start_labtime, end_labtime, start_analysis, end_analysis) {
  labels <- rep(NA, length(start_labtime))
  labels[which(start_labtime >= start_analysis & end_labtime <= end_analysis)] <- "fd"
  labels[which(start_labtime < start_analysis)] <- "baseline"
  labels[which(end_labtime > end_analysis)] <- "recovery"
  
  labels
}



# strip_until_sleep_onset <- function(dt) {
#   #print(typeof(dt))
#   min_labtime <- dt[bout_type=="NREM", min(start_labtime)]
#   
#   nrow(dt) - nrow(dt[start_labtime >= min_labtime])
#   
# }


## Every combo of subject_code and method needs to have: NREM, REM, WAKE, UNDEF
## USED IN PLAYGROUND
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
# set_min_day_num <- function(subjects, sleep_data) {
#   r <- sleep_data[,(min(floor(labtime / T_CYCLE)) - 1),by=subject_code]
#   setnames(r, c('subject_code', 'min_day_number'))
#   
#   merge(subjects, r, all.x=TRUE)           
# }

# set_up_days <- function(labtime, min_day_number, t_cycle) {
#   day_number <- floor(labtime / t_cycle)
#   day_labtime <- labtime - (day_number * t_cycle)
#   day_number <- day_number - min_day_number
#   
#   list(day_number, day_labtime)
# }


### ACTUAL HELPERS!!



### CLASSIC

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

# For classic! 
## Used In Episodes (classic)
merge_around_seeds <- function(labels, lengths, wake=FALSE, min_wake_length=10, min_rem_length=10, min_nrem_length=30) {
  # Find the seed sequences of each type
  seed_nrem <- intersect(which(labels=='NREM'), which(lengths >= min_nrem_length))
  seed_rem <- intersect(which(labels=='REM'), which(lengths >= min_rem_length))
  if(wake) {
    seed_wake <- intersect(which(labels=='WAKE'), which(lengths >= min_wake_length))    
  }
  else
    seed_wake <- c()
  
  # Sort the seed sequences
  seeds <- sort(c(seed_nrem, seed_rem, seed_wake))
  
  if(length(seeds) > 0) {
    # Label everything until first NREM Sequence as WAKE
    if(length(seed_nrem) > 0) {
      first_nrem <- min(seed_nrem)    
      if(first_nrem > 1) {
        labels[1] = 'WAKE'
        seeds <- c(1L, seeds[which(seeds >= first_nrem)])
      }
    }
    # If no first NREM Sequence, everything until first sequence as wake.    
    else {
      if(min(seeds) > 1) {
        labels[1] = 'WAKE'
        seeds <- c(1L, seeds)
      }        
    }
    
    
    group_lengths <- diff(c(seeds, (length(labels)+1)))
    new_labels <- rep.int(labels[seeds], group_lengths)
    
    groups <- set_group(new_labels)
  }
  # 
  else {
    #new_labels <- rep.int(labels[seeds], group_lengths)
    groups <- rep.int(1L, length(labels))#rep.int(seq(1, length(group_lengths)), group_lengths)
    new_labels <- labels
    # list(labels=labels, groups=groups)
    #    list(labels=labels, groups=rep.int(1, length(labels)))  
    
  }  
  list(labels=new_labels, groups=groups)
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

## used in episodes (changepoint)
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
## USED IN ALL EPISODES!!
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
## USED IN EPISODES AND CYCLES
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
## USED IN CYCLES
find_cycles_in_sleep_episode <- function(border_locations, sleep_episode_end, include_end=FALSE) {
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

