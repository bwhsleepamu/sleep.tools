plot_binned_sleep <- function(data, cycle_type="NREM", cycle_method="changepoint", max_cycle=6, max_block=NULL, y_var="nrem_sleep", compare_by="habitual_csr", facet_by="age_group", include_flags=c(TRUE),
                              sleep_efficiency_labels=c("100%", "80%", "60%", "40%", "20%"),
                              age_groups = c("Y","O"),
                              habitual_csr_groups = c("H", "R"),
                              schedule_labels = c("baseline","fd","recovery"),
                              sexes = c("M", "F"),
                              t_cycles = c("20","28","42.85")) {
  
  
  plot_data <- data[include %in% include_flags
                    & se_label %in% sleep_efficiency_labels
                    & age_group %in% age_groups
                    & habitual_csr %in% habitual_csr_groups
                    & schedule_label %in% schedule_labels
                    & sex %in% sexes
                    & t_cycle %in% t_cycles]  
  if("cycle_number" %in% colnames(data)) {
    if(is.null(max_block))
      max_block <- 10
    plot_data <- plot_data[cycle_number <= max_cycle & method == cycle_method & type == cycle_type & block_number <= max_block]
    by_cycle = "cycle_number"
    cycle_facet = "cycle_number"
  } else {
    if(is.null(max_block))
      max_block <- 30
    plot_data <- plot_data[block_number <= max_block]
    by_cycle = NULL
    cycle_facet = NULL
  }
  
  cat(nrow(plot_data))
  
  
  trend_lines <- plot_data[, list(y=mean(get(y_var)), n=length(get(y_var))), by=c("block_number",by_cycle,compare_by,facet_by)]
  
  ## Actual plot
  plot <- ggplot(plot_data, aes_string(x="block_number", y=y_var))
  
  # Theme
  plot <- plot + theme_mine()
  plot <- plot + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  
  plot <- plot + geom_point(aes_string(color=compare_by), shape='.')
  
  plot <- plot + geom_line(aes_string(color=compare_by, y="y", x="block_number"), data=trend_lines)
  plot <- plot + geom_text(aes_string(color=compare_by, y="y", x="block_number", label="n"), size=3, vjust=2.5, data=trend_lines)
  
  if(!is.null(facet_by) | !is.null(cycle_facet)) {
    if(is.null(cycle_facet))
      cycle_facet <- "."
    if(is.null(facet_by))
      facet_by <- "."
    plot <- plot + facet_grid(paste(facet_by, " ~ ", cycle_facet), scales='free')
  }
  plot
}

# Workspace
binned_sleep_plot <- function() {
  to_plot_se <<- merge(collapsed_sleep_episode, subjects, all.x=TRUE, all.y=FALSE)
  to_plot_c <<- merge(collapsed_cycle, subjects, all.x=TRUE, all.y=FALSE)
  to_plot_be <<- merge(collapsed_bedrest_episode, subjects, all.x=TRUE, all.y=FALSE)
  
  
  plot_binned_sleep(to_plot_c, age_groups=c("Y"), max_cycle=5, max_block=10)
  
  plot_binned_sleep(to_plot_c, age_groups=c("Y"), sleep_efficiency_labels=c("20%", "100%"), facet_by=NULL, compare_by="se_label", max_cycle=5, max_block=10)
  
  plot_binned_sleep(to_plot_c, y_var="slow_wave_sleep", age_groups=c("Y"), sleep_efficiency_labels=c("20%", "100%"), facet_by=NULL, compare_by="se_label", max_cycle=5, max_block=10)
}