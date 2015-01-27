# Episode lengths by cycle #, method, etc

# p <- ggplot(episodes[sleep_wake_label == "SLEEP"])
# p <- p + geom_boxplot(aes_string(x="factor(method)", y="length"), outlier.shape= NA ) + scale_y_continuous(limits = quantile(episodes$length, c(0.1, 0.9)))
# p + facet_grid(. ~ label)

plot_cycle_lengths <- function(data, cycle_type="NREM", compare_by='method', facet_x_by=NULL, facet_y_by=NULL, color_by=NULL, include_flags=c(TRUE),
                           methods=c("changepoint", "iterative","classic"),
                           sleep_efficiency_labels=c("100%", "80%", "60%", "40%", "20%"),
                           age_groups = c("Y","O"),
                           habitual_csr_groups = c("H", "R"),
                           schedule_labels = c("baseline","fd","recovery"),
                           sexes = c("M", "F"),
                           t_cycles = c("20","28","42.85")) {
  
  
  data <- data[type == cycle_type &
               include %in% include_flags &
               se_label %in% sleep_efficiency_labels &
               age_group %in% age_groups &
               habitual_csr %in% habitual_csr_groups &
               schedule_label %in% schedule_labels &
               sex %in% sexes &
               t_cycle %in% t_cycles &
               method %in% methods]  
  
  plot <- ggplot(data, aes_string(x=paste("factor(",compare_by,")"), y="length"))
  
  
  
  # Theme
  plot <- plot + theme_mine()
  plot <- plot + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)  
  plot <- plot + geom_boxplot(aes_string(color=color_by), outlier.shape=NA) + scale_y_continuous(limits = quantile(data$length, c(0.1, 0.9)))
  
  if(!is.null(facet_x_by) | !is.null(facet_y_by)) {
    if(is.null(facet_x_by))
      facet_x_by = "."
    if(is.null(facet_y_by))
      facet_y_by = "."
  
    plot <- plot + facet_grid(paste(facet_y_by, ' ~ ', facet_x_by))    
  }
  plot <- plot + ggtitle(paste(cycle_type, "Cycle Lengths"))
  plot
}


