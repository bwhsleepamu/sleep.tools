subjects[include == TRUE]
good_sleep_data <- sleep_data[subject_code %in% subjects[include==TRUE]$subject_code]
good_sleep_data <- data.table(merge(good_sleep_data, subjects, by='subject_code'))
good_sleep_data <- good_sleep_data[labtime <= start_analysis]
good_sleep_data <- good_sleep_data[age_group == 'Y']
efficient_bedrest <- sleep_efficiency[se_label %in% c("100%", "80%")]
good_sleep_data <- data.table(merge(good_sleep_data, efficient_bedrest, by=c('subject_code','activity_or_bedrest_episode') ))
good_episodes <- setup_episodes(good_sleep_data)
