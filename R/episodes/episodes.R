## Methods



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

generate_episodes.classic.strict <- function(dt, min_nrem_length=30, min_rem_length=10, completion_cuttoff=10) {
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




##################
## Iterative
##################
generate.episodes.iterative <- function(dt, wake=TRUE, undef=FALSE, min_nrem_length=NULL, min_rem_length=NULL, min_wake_length=NULL) {
  # Take series of epochs and collapse them into sequences of the same type  
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
generate.episodes.changepoint <- function(dt, wake=TRUE, undef=FALSE, cpmType="Mann-Whitney", ARL0=10000, startup=20) {
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


## HELPERS
label_wake <- function(dt) {
  
  sleep_onset <- min(match(c("NREM", "REM"), dt$label, nomatch=1L))
  sleep_end <- (length(dt$label) - min(match(c("NREM", "REM"), rev(dt$label), nomatch=1L))) + 1L
  
  dt[sleep_onset:sleep_end]
}


### NREM and REM Episodes
## Calculate episodes using different methods.
######## Classic
episodes.classic <- generate.episodes.classic(sleep_data, wake=FALSE, min_nrem_length=mnl, min_rem_length=mrl, min_wake_length=mwl)
######## Iterative
episodes.iterative <- generate.episodes.iterative(sleep_data, wake=TRUE, undef=FALSE, min_nrem_length=mnl, min_rem_length=mrl, min_wake_length=mwl)
######## Changepoint
episodes.changepoint <- generate.episodes.changepoint(sleep_data, wake=TRUE, undef=FALSE, cpmType=cpmType, ARL0=ARL0, startup=startup)
## Merge methods into one table
episodes <- rbindlist(list(episodes.changepoint,episodes.classic,episodes.iterative))
# Get rid of wake episodes
episodes <- episodes[activity_or_bedrest_episode > 0]
# Merge with information about each episode
setkey(sleep_data,pk)
episodes <- merge(episodes, subjects, all.x=TRUE, all.y=FALSE, by='subject_code')
episodes[,`:=`(start_labtime=sleep_data[start_position]$labtime, end_labtime=sleep_data[end_position]$labtime)]
episodes[,schedule_label:=label_by_schedule(start_labtime, end_labtime, start_analysis, end_analysis)]  

episodes <- merge(episodes,sleep_efficiency,all.x=TRUE, all.y=FALSE)
episodes[,agreement:=(sum(sleep_data[start_position:end_position]$epoch_type == label)/length),by=id]

### sleep_episodes and bedrest_episodes
# Get Sleep Episodes (from sleep onset) and Bedrest episodes
bedrest_episodes <- sleep_data[activity_or_bedrest_episode>0,list(start_position=min(.I), end_position=max(.I), start_labtime=min(labtime), end_labtime=max(labtime)),by='subject_code,activity_or_bedrest_episode']
sleep_episodes <- copy(bedrest_episodes)
sleep_episodes[,sleep_onset:=get_first_stage(sleep_data$stage[start_position:end_position], start_position, 2, not_found=start_position), by='subject_code,activity_or_bedrest_episode']
sleep_episodes[,`:=`(start_position=sleep_onset, sleep_onset=NULL)]
#sleep_episodes <- sleep_episodes[!is.na(start_position)]

# Join subject data with episodes and cycles
setkey(bedrest_episodes, subject_code)
setkey(sleep_episodes, subject_code)
bedrest_episodes <- bedrest_episodes[fd_times]
sleep_episodes <- sleep_episodes[fd_times]

# Label FD, recovery, baseline
sleep_episodes[,schedule_label:=label_by_schedule(start_labtime, end_labtime, start_analysis, end_analysis)]
bedrest_episodes[,schedule_label:=label_by_schedule(start_labtime, end_labtime, start_analysis, end_analysis)]

# Label by sleep efficiency
setkey(bedrest_episodes, subject_code, activity_or_bedrest_episode)
setkey(sleep_episodes, subject_code, activity_or_bedrest_episode)
sleep_episodes <- sleep_episodes[sleep_efficiency]
bedrest_episodes <- bedrest_episodes[sleep_efficiency]

# Fix for NAs in start or end positions
sleep_episodes <- sleep_episodes[!is.na(start_position) & !is.na(end_position)]
bedrest_episodes <- bedrest_episodes[!is.na(start_position) & !is.na(end_position)]

# Label SLEEP/WAKE parts of sleep period (from sleep onset to wake)
episodes[,first_sleep_id:=id[min(match(c("NREM", "REM"), label, nomatch=length(l)))],by='subject_code,activity_or_bedrest_episode,method']
episodes[,last_sleep_id:=id[length(label) - min(match(c("NREM", "REM"), rev(label), nomatch=length(l))) + 1L],by='subject_code,activity_or_bedrest_episode,method']
episodes[,sleep_wake_label:="WAKE"]
episodes[id <= last_sleep_id & id >= first_sleep_id, sleep_wake_label:="SLEEP"]