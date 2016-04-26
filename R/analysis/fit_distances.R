good_sleep_data <- sleep_data[subject_code %in% subjects[include==TRUE]$subject_code]
good_sleep_data <- data.table(merge(good_sleep_data, subjects, by='subject_code'))
good_sleep_data <- good_sleep_data[labtime <= start_analysis]
good_sleep_data <- good_sleep_data[age_group == 'Y']
efficient_bedrest <- sleep_efficiency[se_label %in% c("100%")]
good_sleep_data <- data.table(merge(good_sleep_data, efficient_bedrest, by=c('subject_code','activity_or_bedrest_episode') ))
good_episodes <- setup_episodes(good_sleep_data, sleep_data)

# second pass
episodes
cycles

merged_episodes <- data.table(merge(episodes, subjects, by='subject_code'))
merged_cycles <- data.table(merge(cycles, subjects, by='subject_code'))

merged_episodes <- merge(merged_episodes, sleep_efficiency, by=c('subject_code', 'activity_or_bedrest_episode'))
merged_cycles <- merge(merged_cycles, sleep_efficiency, by=c('subject_code', 'activity_or_bedrest_episode'))

merged_episodes[start_labtime <= start_analysis & end_labtime <= end_analysis, protocol_section:='pre']
merged_episodes[start_labtime >= start_analysis & end_labtime <= end_analysis, protocol_section:='fd']
merged_episodes[start_labtime >= end_analysis & end_labtime >= end_analysis, protocol_section:='post']

merged_cycles[start_labtime <= start_analysis & end_labtime <= end_analysis, protocol_section:='pre']
merged_cycles[start_labtime >= start_analysis & end_labtime <= end_analysis, protocol_section:='fd']
merged_cycles[start_labtime >= end_analysis & end_labtime >= end_analysis, protocol_section:='post']


