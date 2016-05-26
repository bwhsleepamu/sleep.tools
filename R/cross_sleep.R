# Please analyze per SP, changes in each of the following variables by NREM-REM cycle
# Subject codes (column A) with SPs (columns K&L) to be analyzed attached
# NREM2
# SWS (NREM3+4)
# NREM2+3+4
# Wake
# REM



source("R/sources.R")

to_analyze <- as.data.table(read.csv("/home/pwm4/Desktop/a1/FD-info for Cross Slp.csv"))
to_analyze <- to_analyze[1:101,,which=FALSE];
to_analyze[,Subject:=as.character(Subject)];
setnames(to_analyze, "Subject", "subject_code")

## Missing Data
to_analyze[,file_path:=paste("/home/pwm4/Documents/DATAMISSING/", subject_code, "/Sleep/", subject_code, "Slp.01.csv", sep="")]
missing_data <- load_sleep_data(to_analyze)

missing_episodes <- generate_episodes.iterative(missing_data, min_nrem_length=CLASSIC_MIN_NREM, min_rem_length=CLASSIC_MIN_REM, min_wake_length=CLASSIC_MIN_REM)
missing_episodes <- missing_episodes[activity_or_bedrest_episode > 0]
setkey(missing_data, pk)
missing_episodes[,`:=`(start_labtime=missing_data[start_position]$labtime, end_labtime=missing_data[end_position]$labtime)]

missing_cycles <- find.cycles(missing_episodes, missing_data, type="NREM", start_fn=find_nrem_start, until_end=TRUE) 
missing_cycles[,`:=`(start_labtime=missing_data[start_position]$labtime, end_labtime=missing_data[end_position]$labtime)]
missing_cycles <- missing_cycles[length > 0]
missing_cycles <- missing_cycles[!is.na(start_position) & !is.na(end_position)]

##



final_results <- find_state_frequency_changes(to_analyze, cycles[method=='iterative' & type=="NREM"], sleep_data)
missing_results <- find_state_frequency_changes(to_analyze[subject_code %in% c("2768X","2788X","27B2X")], missing_cycles, missing_data)


find_state_frequency_changes <- function(subject_list, timeframes, sleep_data) {
  subject_list <- to_analyze[subject_code %in% c("2768X","2788X","27B2X")]
  timeframes <- missing_cycles
  sleep_data <- missing_data
  
  results <- subject_list[,list(subject_code, Study, Study2, Start.analysis.SPn, End.analysis.SPn..included.),]
  setnames(results, c("subject_code", "Study", "Study2", "Start.analysis.SPn", "End.analysis.SPn..included."), c("subject_code", "study_name_1", "study_name_2", "start_analysis_sp", "end_analysis_sp"))
  results <- merge(results, timeframes, by='subject_code', all.x=TRUE, all.y=FALSE)
  results <- results[activity_or_bedrest_episode >= start_analysis_sp & activity_or_bedrest_episode <= end_analysis_sp]
  # results[,cycle_number:=1:.N,by='subject_code,activity_or_bedrest_episode']
  
  results <- results[,state_stats(sleep_data[start_position:end_position]),by='subject_code,study_name_1,study_name_2,activity_or_bedrest_episode,cycle_number,start_labtime,end_labtime,length']
  results[,`:=`(percent_nrem_2=nrem_2_epochs/total_epochs,percent_sws=sws_epochs/total_epochs, percent_nrem_234=nrem_234_epochs/total_epochs,percent_wake=wake_epochs/total_epochs,percent_rem=rem_epochs/total_epochs)]
  
  
  results
}





episodes[method=='enhanced']

to_analyze$Subject %in% unique(final_results$subject_code)

# Output:
# subject_code
# activity_or_bedrest_episode
# nrem_cycle_number
# nrem_cycle_start_labtime
# nrem_cycle_end_labtime
# time_in_(nrem2/sws/nrem_234/wake/rem)

to_analyze

state_stats <- function(sleep_data_subset) {
  list(
    total_epochs = nrow(sleep_data_subset),
    nrem_2_epochs = nrow(sleep_data_subset[stage == 2]),
    sws_epochs = nrow(sleep_data_subset[stage %in% c(3,4)]),
    nrem_234_epochs = nrow(sleep_data_subset[stage %in% c(2,3,4)]),
    wake_epochs = nrow(sleep_data_subset[stage == 5]),
    rem_epochs = nrow(sleep_data_subset[stage == 6])
  )
}


plot_data <- final_results[,list(y=mean(percent_nrem_234)),by='subject_code,cycle_number']
p <- ggplot(data=plot_data)
p + geom_line(mapping=aes(cycle_number,y,color=subject_code))

plot_data <- nrem_episode_results[,list(y=mean(percent_rem)),by='nrem_cycle_number']
p <- ggplot(data=plot_data)
p + geom_line(mapping=aes(nrem_cycle_number,y))

final_results



