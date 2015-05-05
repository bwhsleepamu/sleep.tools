source('R/analysis/agreement.R')
source('R/plotting/agreement_plot.R')


analysis_main <- function() {
  joint_episodes <- merge(episodes, subjects, by='subject_code')
  joint_episodes <- merge(joint_episodes, sleep_efficiency, by=c('subject_code', 'activity_or_bedrest_episode'))
  #joint_episodes[method=='iterative', episode_type:=label]
  
  joint_episodes <- joint_episodes[method %in% c('changepoint_compact', 'iterative')]
  
  set_agreement(joint_episodes, sleep_data)
  
  # Pre During Post
  
  # Male Female
  
  # Young Old
  
  # Habitual CSR
  
  # T Cycle
  
  ## I WANT TO SEE IF THERE ARE SIGNIFICANT DIFFERENCES BETWEEN THESE POPULATIONS
  
  
  joint_episodes[start_labtime <= start_analysis & end_labtime <= end_analysis, protocol_section:='pre']
  joint_episodes[start_labtime >= start_analysis & end_labtime <= end_analysis, protocol_section:='fd']
  joint_episodes[start_labtime >= end_analysis & end_labtime >= end_analysis, protocol_section:='post']
  
  
  episode_counts <- joint_episodes[,count_in_bedrest_episode(label),by='subject_code,method,activity_or_bedrest_episode']
  episode_counts <- episode_counts[method %in% c('changepoint_compact', 'iterative')]
  
  plot_agreement(joint_episodes)
  
  joint_episodes <- joint_episodes[method!='changepoint' & episode_type!='UNDEF']
  
  p <- ggplot(joint_episodes[label!="UNDEF"], aes(factor(label), agreement))
  p + geom_boxplot(aes(fill = factor(method))) + coord_flip()
  
  p <- ggplot(joint_episodes[label!="UNDEF"], aes(factor(label), length))
  p + geom_boxplot(aes(fill = factor(method))) + scale_y_log10()
  
  p <- ggplot(episode_counts, aes(factor(method), nrem_count))
  p + geom_boxplot(aes(fill = factor(method)))
  
  p <- ggplot(episode_counts, aes(factor(method), rem_count))
  p + geom_boxplot(aes(fill = factor(method)))
  
  p <- ggplot(episode_counts, aes(factor(method), wake_count))
  p + geom_boxplot(aes(fill = factor(method)))

  p <- ggplot(cycles[method != 'changepoint' & cycle_number < 10], aes(factor(method), length))  
  p <- p + facet_grid(type ~ cycle_number)
  p + geom_boxplot(aes(fill = factor(method))) + scale_y_log10()
  
}


count_in_bedrest_episode <- function(episode_types) {
  t <- table(episode_types)
  data.table(nrem_count=t["NREM"], rem_count=t["REM"], wake_count=t["WAKE"], undef_count=t["UNDEF"])
}



# 
# qplot(factor(episode_type), agreement, data = joint_episodes_fd, geom = "boxplot")
