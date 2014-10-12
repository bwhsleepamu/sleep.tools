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
nrem_cycles <- find.cycles(episodes, sleep_data, type="NREM", start_fn=find_nrem_start, until_end=TRUE) 
rem_cycles <- find.cycles(episodes, sleep_data, type="REM", start_fn=find_nrem_start, until_end=TRUE) 
cycles <- rbind(nrem_cycles, rem_cycles)
cycles[,`:=`(start_labtime=sleep_data[start_position]$labtime, end_labtime=sleep_data[end_position]$labtime)]
cycles <- cycles[length > 0]

setkey(cycles, subject_code)
cycles <- cycles[fd_times]



## Label the part of protocol each episode or cycle is in
cycles[,schedule_label:=label_by_schedule(start_labtime, end_labtime, start_analysis, end_analysis)]

## FIX FOR NA start and end??

setkey(cycles, subject_code, activity_or_bedrest_episode)
cycles <- cycles[sleep_efficiency]

cycles <- cycles[!is.na(start_position) & !is.na(end_position)]
