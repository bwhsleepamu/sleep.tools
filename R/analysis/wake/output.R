# Latencies
common_columns <- c("subject_code", "activity_or_bedrest_episode", "label", "start_position", "end_position", "length")


setkeyv(rem_latencies, common_columns)
setkeyv(nrem_latencies, common_columns)
setkeyv(wake_latencies, common_columns)
setkeyv(sequences, common_columns)

# Combine seperate tables into one master table
sequences_with_latency <- merge(sequences, rem_latencies, all=TRUE, suffixes=c("", ".y1"))
sequences_with_latency <- merge(sequences_with_latency, nrem_latencies, all=TRUE, suffixes=c("", ".y2"))
sequences_with_latency <- merge(sequences_with_latency, wake_latencies, all=TRUE, suffixes=c("", ".y3"))
sequences_with_latency[,`:=`(length_class.y1=NULL, length_class.y2=NULL, length_class.y3=NULL, episode_type.y1=NULL, episode_type.y2=NULL, episode_type.y3=NULL, cycle_number.y1=NULL, cycle_number.y2=NULL, cycle_number.y3=NULL, prev_label.y1=NULL, prev_label.y2=NULL, prev_label.y3=NULL, prev_length.y1=NULL, prev_length.y2=NULL, prev_length.y3=NULL)]
sequences_with_latency[,method:=NULL]
sequences_with_latency[,pik:=NULL]

# Convert lengths to minutes
sequences_with_latency[,`:=`(start_labtime=sleep_data[start_position]$labtime, end_labtime=sleep_data[end_position]$labtime)]
sequences_with_latency[,length_in_epochs:=length]

# List columns in need of conversion
epoch_columns = c('length', 'prev_length', 'stage_2_latency', 'stage_3_latency', 'rem_latency', 'nrem_latency', 'wake_latency', 'next_rem_length', 'next_nrem_length', 'next_wake_length')
for (j in epoch_columns) set(sequences_with_latency,j=j,value=sequences_with_latency[[j]]*length_coefficient)

setcolorder(sequences_with_latency, c('subject_code', 'activity_or_bedrest_episode', 'label', 'length', 'start_labtime', 'end_labtime', 
                                      'rem_latency', 'nrem_latency', 'stage_2_latency', 'stage_3_latency', 'wake_latency', 'next_rem_length', 'next_nrem_length', 'next_wake_length',
                                      'prev_label', 'prev_length', 'episode_type', 'cycle_number', 'length_class',
                                      'wake_epochs', 'complete', 'next_rem_position', 'next_nrem_position', 'next_wake_position', 'next_stage_2', 'next_stage_3', 'start_position', 'end_position',  'length_in_epochs'))



# Inter State Intervals
iri[,type:="inter_REM"]
ini[,type:="inter_NREM"]
iwi[,type:="inter_WAKE"]

inter_state_intervals <- rbindlist(list(iri,iwi,ini), use.names=TRUE, fill=TRUE)
setnames(inter_state_intervals, c('i_length'), c('interval_length') )
inter_state_intervals[,pik:=NULL]
inter_state_intervals[,`:=`(start_labtime=sleep_data[start_position]$labtime, end_labtime=sleep_data[end_position]$labtime)]

# Convert lengths to minutes
inter_state_intervals[,interval_length_in_epochs:=interval_length]
inter_state_intervals[,interval_length:=interval_length*length_coefficient]
setcolorder(inter_state_intervals, c('subject_code', 'activity_or_bedrest_episode', 'type', 'interval_length', 'start_labtime', 'end_labtime', 'episode_type', 'cycle_number', 'start_position', 'end_position', 'interval_length_in_epochs'))


# Ouput to CSV
write.csv(sequences_with_latency, file='/home/pwm4/Desktop/beth_output/latencies_20151006.csv', row.names=FALSE, na=".")
write.csv(subjects[subject_code %in% allowed_subject_codes], file='/home/pwm4/Desktop/beth_output/subjects_20151006.csv', row.names=FALSE, na=".")
write.csv(inter_state_intervals, file='/home/pwm4/Desktop/beth_output/inter_state_intervals_20151006.csv', row.names=FALSE, na=".")


