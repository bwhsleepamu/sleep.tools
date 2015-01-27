# MAIN FUNCTION








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






# Strip all periods until first NREM



## BOXPLOTS


