joint_episodes <- merge(episodes, subjects, by='subject_code')
joint_episodes <- merge(joint_episodes, sleep_efficiency, by=c('subject_code', 'activity_or_bedrest_episode'))
joint_episodes[method=='iterative', episode_type:=label]
set_agreement(joint_episodes, sleep_data)




# Pre During Post

# Male Female

# Young Old

# Habitual CSR

# T Cycle

## I WANT TO SEE IF THERE ARE SIGNIFICANT DIFFERENCES BETWEEN THESE POPULATIONS


joint_episodes_pre <- joint_episodes[start_labtime <= start_analysis & end_labtime <= end_analysis]
joint_episodes_fd <- joint_episodes[start_labtime >= start_analysis & end_labtime <= end_analysis]
joint_episodes_post <- joint_episodes[start_labtime >= end_analysis & end_labtime >= end_analysis]

episode_counts <- joint_episodes_fd[,count_in_bedrest_episode(episode_type),by='subject_code,method,activity_or_bedrest_episode']


plot_agreement(joint_episodes_fd, facet_x_by=".")


p <- ggplot(joint_episodes_fd, aes(factor(episode_type), agreement))
p + geom_boxplot(aes(fill = factor(method)))

p <- ggplot(joint_episodes_fd, aes(factor(episode_type), length))
p + geom_boxplot(aes(fill = factor(method)))

p <- ggplot(episode_counts, aes(factor(method), nrem_count))
p + geom_boxplot(aes(fill = factor(method)))

p <- ggplot(episode_counts, aes(factor(method), rem_count))
p + geom_boxplot(aes(fill = factor(method)))

p <- ggplot(episode_counts, aes(factor(method), wake_count))
p + geom_boxplot(aes(fill = factor(method)))

count_in_bedrest_episode <- function(episode_types) {
  t <- table(episode_types)
  data.table(nrem_count=t["NREM"], rem_count=t["REM"], wake_count=t["WAKE"], undef_count=t["UNDEF"])
}




qplot(factor(episode_type), agreement, data = joint_episodes_fd, geom = "boxplot")
