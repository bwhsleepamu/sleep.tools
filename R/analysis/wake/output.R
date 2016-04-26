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



# Paper output

write.csv(inter_state_intervals, file='/X/Manuscripts/Original Report/Inprep/Mankowski Sleep Episode Structure/Data/inter_state_intervals.csv', row.names=FALSE, na="")


# Matrices for heatmaps
write_heatmap_matrix <- function(d, type = "REM", bin_size=10, i_range=c(0,400)) {
  hd <- isi_heatmap(d, type, isi_range=i_range, bin_width=c(bin_size,bin_size), scale_cutoff = 1)
  print(hd$heatmap_data)
  print(length(hd$y_levels))
  print(length(hd$x_levels))
  m <- matrix(nrow=length(hd$y_levels), ncol=length(hd$x_levels), dimnames=list(hd$y_levels, hd$x_levels))
  hd$heatmap_data[,{ m[y_bin, x_bin] <<- .N }, by='y_bin,x_bin']
  
  write.csv(m, file=paste('/X/Manuscripts/Original Report/Inprep/Mankowski Sleep Episode Structure/Data/heatmap', type, bin_size, 'matrix.csv', sep="_"), na="")
}

target_d <- heatmap_data_list$REM

m <- matrix(nrow=length(target_d$x_levels), ncol=length(target_d$y_levels), dimnames=list(target_d$x_levels, target_d$y_levels))
target_d$heatmap_data[,{m[x_bin,y_bin]<<-.N},by='x_bin,y_bin']

heatmap_data_list$REM$heatmap_data



?matrix
matrix(data=heatmap_data_list$REM$heatmap_data, byrow=true, dimnames = c(levels(heatmap_data_list$REM$heatmap_data$ypa_bin), levels(heatmap_data_list$REM$heatmap_data$x_bin) 