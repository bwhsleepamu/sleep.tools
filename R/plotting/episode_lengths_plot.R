# Episode lengths by cycle #, method, etc

# p <- ggplot(episodes[sleep_wake_label == "SLEEP"])
# p <- p + geom_boxplot(aes_string(x="factor(method)", y="length"), outlier.shape= NA ) + scale_y_continuous(limits = quantile(episodes$length, c(0.1, 0.9)))
# p + facet_grid(. ~ label)

plot_episode_lengths <- function(data, compare_by="method", facet_x_by="label", facet_y_by="habitual_csr", color_by=NULL, include_flags=c(TRUE),
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
  
  plot <- ggplot(data, aes_string(x=paste("factor(",compare_by,")"), y="length"))
  
  # Theme
  plot <- plot + theme_mine()
  plot <- plot + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)  
  plot <- plot + geom_boxplot(aes_string(color="age_group"), outlier.shape=NA) + scale_y_continuous(limits = quantile(data$length, c(0.1, 0.9)))
  plot <- plot + facet_grid(paste(facet_y_by, ' ~ ', facet_x_by))
  plot <- plot + ggtitle("Episode Lengths")
  plot
}


