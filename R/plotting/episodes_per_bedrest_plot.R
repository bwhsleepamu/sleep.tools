plot_episodes_per_bedrest <- function(data, compare_by="method", facet_x_by="label", facet_y_by="habitual_csr", include_flags=c(TRUE),
                                 labels=c("NREM", "REM", "WAKE"),
                                 methods=c("changepoint", "iterative","classic"),
                                 sleep_wake_labels=c("SLEEP"),
                                 sleep_efficiency_labels=c("100%", "80%", "60%", "40%", "20%"),
                                 age_groups = c("Y","O"),
                                 habitual_csr_groups = c("H", "R"),
                                 schedule_labels = c("baseline","fd","recovery"),
                                 sexes = c("M", "F"),
                                 t_cycles = c("20","28","42.85")) {
  
  
  data <- data[include %in% include_flags &
                 label %in% labels &
                 se_label %in% sleep_efficiency_labels &
                 age_group %in% age_groups &
                 habitual_csr %in% habitual_csr_groups &
                 schedule_label %in% schedule_labels &
                 sex %in% sexes &
                 t_cycle %in% t_cycles &
                 sleep_wake_label %in% sleep_wake_labels &
                 method %in% methods]  
  
  plot_data <- data[,list(episode_count=length(id)),by=c('subject_code','activity_or_bedrest_episode',compare_by,facet_y_by,facet_x_by)]
  
  plot <- ggplot(plot_data, aes_string(x="episode_count", fill=compare_by, color=compare_by))
  
  if(is.null(facet_x_by))
    facet_x_by = "."
  if(is.null(facet_y_by))
    facet_y_by = "."
  
  # Theme
  plot <- plot + theme_mine()
  plot <- plot + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)  
  
  plot <- plot + geom_freqpoly(binwidth=1, origin=-0.5)
  
  
  plot <- plot + facet_grid(paste(facet_y_by, ' ~ ', facet_x_by), scales="free")
  plot <- plot + ggtitle("Episodes Per Bedrest")
  
  plot
}
