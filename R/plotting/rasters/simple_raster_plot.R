db_sleep_episodes <- fread("/home/pwm4/Desktop/rasters_andrew_m/sleep_episodes.csv")
db_light_episodes <- fread("/home/pwm4/Desktop/rasters_andrew_m/light_episodes.csv")

## Modify column names
setnames(db_sleep_episodes, names(db_sleep_episodes), tolower(names(db_sleep_episodes)))
setnames(db_sleep_episodes, c('first_decimal_labtime', 'second_decimal_labtime'), c('start_labtime', 'end_labtime'))
setnames(db_light_episodes, names(db_light_episodes), tolower(names(db_light_episodes)))
setnames(db_light_episodes, c('first_decimal_labtime', 'second_decimal_labtime'), c('start_labtime', 'end_labtime'))

## Set up for plotting
setup_raster_data <- function(db_sleep_episodes, db_light_episodes) {
  sleep_episodes.v <- copy(db_sleep_episodes)
  light_episodes.v <- copy(db_light_episodes)
  
  sleep_episodes.v[,c('start_day_number', 'start_day_labtime', 'end_day_number', 'end_day_labtime'):=c(set_days(start_labtime),set_days(end_labtime))]
  light_episodes.v[,c('start_day_number', 'start_day_labtime', 'end_day_number', 'end_day_labtime'):=c(set_days(start_labtime),set_days(end_labtime))]
  
  ## Deal with blocks that span multiple days
  sleep_episodes.v <- rbindlist(list(sleep_episodes.v[start_day_number==end_day_number], split_day_spanning_blocks(sleep_episodes.v[start_day_number!=end_day_number])))
  light_episodes.v <- rbindlist(list(light_episodes.v[start_day_number==end_day_number], split_day_spanning_blocks(light_episodes.v[start_day_number!=end_day_number])))
  
  ## Re-scale day numbers
  sleep_episodes.v[,day_number:=start_day_number]
  sleep_episodes.v[,`:=`(start_day_number=NULL, end_day_number=NULL)]
  light_episodes.v[,day_number:=start_day_number]
  light_episodes.v[,`:=`(start_day_number=NULL, end_day_number=NULL)]
  
  # TODO
  
  
  
  NULL
}


## Raster plots!
# Plotting
plot_raster <- function(subject_code, epoch_length=EPOCH_LENGTH, output_dir="/home/pwm4/Desktop/", l="",
                        number_of_days=NA, 
                        first_day=1
) {  
  # Limit by subject
  subject_list <- c(subject_code)
  
  # Limit by day
   days_to_graph <- unique(sleep_episodes.v[subject_code %in% subject_list]$day_number)
   if(!is.na(number_of_days))
     days_to_graph <- days_to_graph[first_day:(first_day+number_of_days-1)]
  
#   print(days_to_graph)
  
  graph_sleep_episodes <- copy(sleep_episodes.v[subject_code %in% subject_list])
  graph_light_episodes <- copy(light_episodes.v[subject_code %in% subject_list])

  # Draw
  .e <- environment()
  
  # Main Plot
  plot <- ggplot(environment=.e)
  
  # Labels and theming
  plot <- plot + ggtitle(subject_code)
  plot <- plot + theme(axis.title.y=element_blank(), legend.title=element_blank())
  plot <- plot + xlab("Time (hours)")
  
  # Faceting
  plot <- plot + facet_grid(day_number ~ .)
  
  # Scaling and Margins
  #plot <- plot + theme(panel.margin = unit(0, "npc"))
  #y_breaks <- c(-5,-3,-1,0,.5,1.5,2,2.5,3,4)
 
  plot <- plot + scale_x_continuous(limits=c(0 - epoch_length, 24 + epoch_length), expand=c(0,0), breaks=c(0,12,24), minor_breaks=c(3,6,9,15,18,21)) 
  #plot <- plot + scale_y_continuous(limits=c(-6, 4), breaks=y_breaks, labels=lapply(y_breaks,y_axis_formatter))
  
  # Colors
  plot <- plot + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  
  
  
  #plot <- plot + scale_fill_manual(values=alpha(c("blue", "red", "black", "purple", "green", "yellow"), 0.8))
  
  ## Episodes and Cycles
  #methods <- c('classic', 'iterative', 'changepoint')
#   r <- foreach(i=1:length(methods)) %do% {
#     end_pos <- i * -2    
#     text_y_pos <- end_pos + 0.5
#     
#     for_this_graph <- graph_cycles[method==methods[i]]
#     #print(nrow(for_this_graph))
#     #print(end_pos)
#     plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = end_pos+1, ymax = end_pos+2, data = graph_episodes[method==methods[i]])
#     plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), fill=NA, color='black', ymin = end_pos, ymax = end_pos+1, data=for_this_graph)    
#     plot <- plot + geom_text(aes(x=(start_day_labtime+end_day_labtime)/2, label=cycle_number), y=text_y_pos, data=for_this_graph)
#   }  
#   
  #  plot <- plot + geom_text(aes(x=(start_day_labtime+end_day_labtime)/2, y=-1.5, label=cycle_number), data=graph_cyles[method=='classic'])
  #   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -3, ymax = -2, data = graph_periods[method=="iterative"])
  #   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), ymin = -4, ymax = -3, data = graph_cyles[method=="iterative"])
  #   
  #   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -5, ymax = -4, data = graph_periods[method=="changepoint"])
  #   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), ymin = -6, ymax = -5, data = graph_cyles[method=="changepoint"])
  
  ## Sleep Episodes
  plot <- plot + geom_rect(aes(xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, ymin=0, ymax=1), alpha=.5, data=graph_sleep_episodes)
  
  ## Light Episodes
  plot <- plot + geom_rect(aes(xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, ymin = 1, ymax = 2), fill=NA, color='black', data=graph_light_episodes)    
  plot <- plot + geom_text(aes(x=(start_day_labtime+end_day_labtime)/2, label=light_level), y=1.5, data=graph_light_episodes)

  plot
  
  
  #plot <- plot + geom_point(shape='.', size=2)
  #plot <- plot + geom_line() #aes(colour=epoch_type)
  
  file_name = file.path(output_dir, paste(subject_code, "_", l, '.svg', sep=''))
  print(file_name)
  print(length(days_to_graph))
  ggsave(plot=plot, file=file_name, height=(length(days_to_graph)*1 + 0.5), width=7, scale=2.5, limitsize=FALSE)
  
  plot
  
}

