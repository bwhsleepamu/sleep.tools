###############
## Sleep Cycles
###############
# HELPERS
## USED IN CYCLES
# First occurance of Stage 2
find_nrem_start <- function(stages, start_position) {
  get_first_stage(stages, start_position, 2)
}

find_rem_start <- function(stages, start_position) {
  get_first_stage(stages, start_position, 6)
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
  cycles[,type:=type]
  cycles
}

# Get last position of a sleep episode
last_se_position <- function(sd, sc, e) {
  cat(sc, " ", e, "\n")
  max(sd[c(sc, e)]$pk)
  
  #max(sd[subject_code==sc & activity_or_bedrest_episode==e]$pk)
}







## Environment Setup
# Get NREM Cycles
setup_cycles <- function(sleep_data, episodes) {
  nrem_cycles <- find.cycles(episodes, sleep_data, type="NREM", start_fn=find_nrem_start, until_end=TRUE) 
  rem_cycles <- find.cycles(episodes, sleep_data, type="REM", start_fn=find_nrem_start, until_end=TRUE) 
  cycles <<- data.table(rbindlist(list(nrem_cycles, rem_cycles)))
  cycles[,`:=`(start_labtime=sleep_data[start_position]$labtime, end_labtime=sleep_data[end_position]$labtime)]
  cycles <<- cycles[length > 0]
  cycles <<- cycles[!is.na(start_position) & !is.na(end_position)]
  
  cycles <<- data.table(merge(cycles, fd_times, all.x=TRUE, all.y=FALSE, by='subject_code'))
  
  ## Label the part of protocol each episode or cycle is in
  cycles[,schedule_label:=label_by_schedule(start_labtime, end_labtime, start_analysis, end_analysis)]
  
  ## FIX FOR NA start and end??
  
  cycles <<- data.table(merge(cycles,sleep_efficiency,all.x=TRUE,all.y=FALSE,by=c('subject_code','activity_or_bedrest_episode')))
  cycles <<- data.table(merge(cycles,subjects,all.x=TRUE, all.y=FALSE,by='subject_code'))
}
