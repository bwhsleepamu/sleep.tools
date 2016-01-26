View(sleep_data[1:100])


function() {
  # Label by sleep/wake
  sleep_data[, sleep_wake_label:="na"]
  sleep_data[epoch_type %in% c("NREM", "REM"), sleep_wake_label:="sleep"]
  sleep_data[epoch_type %in% c("WAKE"), sleep_wake_label:="wake"]
  
  sleep_wake_sequences <<- sleep_data[, chunk(sleep_wake_label, pk), by='subject_code,activity_or_bedrest_episode']
  sleep_wake_sequences[,length:=length/2.0,]
  sleep_wake_sequences[label=="sleep" & length >=10,seq_num:=1:.N,by='subject_code,activity_or_bedrest_episode']
  
  sleep_wake_sequences[,`:=`(start_labtime=sleep_data[start_position]$labtime, end_labtime=sleep_data[end_position]$labtime)]
  sleep_onsets <- sleep_wake_sequences[label=="sleep" & seq_num==1,list(sleep_onset_labtime=start_labtime),by='subject_code,activity_or_bedrest_episode']
  
  
  sleep_episodes <<- merge(sleep_episodes, sleep_onsets, by=c("subject_code","activity_or_bedrest_episode"), all.x=TRUE, all.y=FALSE)
  
  # Collapse by sleep/wake to get sleep/wake sequences

  
}
sleep_data[]