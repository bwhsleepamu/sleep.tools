# # Latencies
# common_columns <- c("subject_code", "activity_or_bedrest_episode", "label", "start_position", "end_position", "length")
# 
# 
# 
# # List columns in need of conversion
# # epoch_columns = c('length', 'prev_length', 'stage_2_latency', 'stage_3_latency', 'rem_latency', 'nrem_latency', 'wake_latency', 'next_rem_length', 'next_nrem_length', 'next_wake_length')
# # for (j in epoch_columns) set(sequences_with_latency,j=j,value=sequences_with_latency[[j]]*length_coefficient)
# # 
# # setcolorder(sequences_with_latency, c('subject_code', 'activity_or_bedrest_episode', 'label', 'length', 'start_labtime', 'end_labtime', 
# #                                       'rem_latency', 'nrem_latency', 'stage_2_latency', 'stage_3_latency', 'wake_latency', 'next_rem_length', 'next_nrem_length', 'next_wake_length',
# #                                       'prev_label', 'prev_length', 'episode_type', 'cycle_number', 'length_class',
# #                                       'wake_epochs', 'complete', 'next_rem_position', 'next_nrem_position', 'next_wake_position', 'next_stage_2', 'next_stage_3', 'start_position', 'end_position',  'length_in_epochs'))
# # 
# 
# 
# # Inter State Intervals
# 

# Ouput to CSV
write.csv(sequences_with_latency, file='/home/pwm4/Desktop/beth_output/latencies_20151006.csv', row.names=FALSE, na=".")
write.csv(subjects[subject_code %in% allowed_subject_codes], file='/home/pwm4/Desktop/beth_output/subjects_20151006.csv', row.names=FALSE, na=".")
write.csv(inter_state_intervals, file='/home/pwm4/Desktop/beth_output/inter_state_intervals_20151006.csv', row.names=FALSE, na=".")


