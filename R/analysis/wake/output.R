# Latencies
common_columns <- c("subject_code", "activity_or_bedrest_episode", "label", "start_position", "end_position", "length")


setkeyv(rem_latencies, common_columns)
setkeyv(nrem_latencies, common_columns)
setkeyv(wake_latencies, common_columns)
setkeyv(sequences, common_columns)


sequences_with_latency <- merge(sequences, rem_latencies, all=TRUE, suffixes=c("", ".y1"))
sequences_with_latency <- merge(sequences_with_latency, nrem_latencies, all=TRUE, suffixes=c("", ".y2"))
sequences_with_latency <- merge(sequences_with_latency, wake_latencies, all=TRUE, suffixes=c("", ".y3"))
sequences_with_latency[,`:=`(length_class.y1=NULL, length_class.y2=NULL, length_class.y3=NULL, episode_type.y1=NULL, episode_type.y2=NULL, episode_type.y3=NULL, cycle_number.y1=NULL, cycle_number.y2=NULL, cycle_number.y3=NULL, prev_label.y1=NULL, prev_label.y2=NULL, prev_label.y3=NULL, prev_length.y1=NULL, prev_length.y2=NULL, prev_length.y3=NULL)]
sequences_with_latency[,method:=NULL]
sequences_with_latency[,pik:=NULL]

sequences_with_latency[,`:=`(start_labtime=sleep_data[start_position]$labtime, end_labtime=sleep_data[end_position]$labtime)]



# Inter State Intervals
iri[,type:="inter_REM"]
ini[,type:="inter_NREM"]
iwi[,type:="inter_WAKE"]

inter_state_intervals <- rbindlist(list(iri,iwi,ini), use.names=TRUE, fill=TRUE)
setnames(inter_state_intervals, c('sp', 'ep', 'i_length'), c('start_position', 'end_position', 'interval_length') )
inter_state_intervals[,pik:=NULL]
inter_state_intervals[,`:=`(start_labtime=sleep_data[start_position]$labtime, end_labtime=sleep_data[end_position]$labtime)]

# Ouput to CSV
write.csv(sequences_with_latency, file='/home/pwm4/Desktop/beth_output/latencies.csv', row.names=FALSE, na=".")
write.csv(subjects[subject_code %in% allowed_subject_codes], file='/home/pwm4/Desktop/beth_output/subjects.csv', row.names=FALSE, na=".")
write.csv(inter_state_intervals, file='/home/pwm4/Desktop/beth_output/inter_state_intervals.csv', row.names=FALSE, na=".")


