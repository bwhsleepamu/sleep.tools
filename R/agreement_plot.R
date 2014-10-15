
plot_agreement <- function(data, compare_by="method", facet_x_by="age_group", facet_y_by="label", include_flags=c(TRUE),
                           labels=c("NREM", "REM", "WAKE"),
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
               sleep_wake_label %in% sleep_wake_labels]  
  
  plot <- ggplot(data, aes_string(x="agreement", color=compare_by))
  plot <- plot + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette) + theme_mine()
  plot <- plot + ggtitle("Agreement within Periods by Method")
  plot <- plot + xlab("Agreement")
  plot <- plot + facet_grid(paste(facet_y_by, ' ~ ', facet_x_by), scales='free_y')
  plot <- plot + geom_density(alpha=.3)
  plot
}

# agreement_plot <- function() {
#   
#   agreement_episodes <<- episodes
#   agreement_episodes[,id:=.I]
#   agreement_episodes[,agreement:=(sum(sleep_data[start_position:end_position]$epoch_type == label)/length),by=id]
# 
# }
# 
#p <- ggplot(clean.periods[label %in% c('NREM', 'REM')], aes(x=agreement, color=method))

