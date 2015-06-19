function(x) { 
  beth_data <- as.data.table(read.csv("data/beth_analysis.csv"))
  beth_data[,subject_code:=regmatches(myfilename, regexpr("\\S\\d\\d", myfilename, perl=TRUE))]
  beth_data[,labtime:=(1:.N)*.5/60.0,by='subject_code']
  beth_data[,pk:=.I]
  beth_data[,stage:=map_beth_stages(SlpState),by='pk']
  beth_data[,epoch_type:=map_epoch_type(stage),by='pk']
  beth_data[stage==2 | stage == 3,scale_factor:=4/boxplot.stats(delta_power)$stats[5],by='subject_code']
  setnames(beth_data, 'delta', 'delta_power')
  beth_data[stage==2 | stage == 3,scale_factor:=4/boxplot.stats(delta_power)$stats[5],by='subject_code']
  beth_data[stage==2 | stage == 3,scaled_delta_power:=delta_power * scale_factor]
  beth_data[scaled_delta_power>4, scaled_delta_power:=4]
  beth_data[,activity_or_bedrest_episode:=1]
  
  sleep_data <- copy(beth_data)
  setup_episodes(sleep_data, sleep_data)
  setup_cycles(sleep_data, episodes)
  setup_raster_data(sleep_data, episodes, cycles)

  plot_beth_raster(subject_list=NULL, first_day = 1)
  
  andrew_episodes <-generate_episodes.iterative(andrew_data, min_nrem_length=CLASSIC_MIN_NREM, min_rem_length=CLASSIC_MIN_REM, min_wake_length=CLASSIC_MIN_REM)
  andrew_episodes[,count:=1:.N,by='subject_code,activity_or_bedrest_episode,label']
  andrew_episodes[,episode_id:=paste(label, count, sep="_")]
  
  episode_ids <- rep.int(andrew_episodes$episode_id, andrew_episodes$length)
  andrew_data[,episode_id:=episode_ids]
  
  
  
  View(andrew_episodes)
}
####
####
map_beth_stages <- function(x) {
  if(x == 9) { res <- 5 }
  else if (x == 5) { res <- 6 }
  else { res <- x }
  as.integer(res)
}

map_beth_epoch_type <- function(x) {
  ## Possibly speed up if x is a factor??
  if (x >= 1 & x <=3) { res <- "NREM" }
  else if (x == 7) { res <- "WAKE" }
  else if (x == 5) { res <- "REM" }
  else { res <- "UNDEF" }
  
  res
}

plot_beth_raster <- function(subject_list = NULL, #, epoch_length=EPOCH_LENGTH, output_dir="/home/pwm4/Desktop/", l="",
                        number_of_days=NA, 
                        first_day=1,
                        cycle_types=c("NREM")
) {  
  
  epoch_length = EPOCH_LENGTH
  
  # Limit by subject
  if(is.null(subject_list))
    subject_list <- unique(sleep_data.v$subject_code)
  
  # Limit by day
  days_to_graph <- unique(sleep_data.v[subject_code %in% subject_list]$day_number)
  if(!is.na(number_of_days))
    days_to_graph <- days_to_graph[first_day:(first_day+number_of_days-1)]
  
  print(days_to_graph)
  
  graph_data <<- copy(sleep_data.v[subject_code %in% subject_list & day_number %in% days_to_graph])
  setkeyv(graph_data, c('subject_code', 'labtime'))
  graph_episodes <<- copy(episodes.v[subject_code %in% subject_list & day_number %in% days_to_graph & activity_or_bedrest_episode > 0])
  graph_cycles <- copy(cycles.v[subject_code %in% subject_list & day_number %in% days_to_graph & activity_or_bedrest_episode > 0 & type %in% cycle_types])
  #graph_bedrest_episodes <- copy(bedrest_episodes.v[subject_code %in% subject_list & day_number %in% days_to_graph])
  
  # Draw
  .e <- environment()
  
  # Main Plot
  plot <- ggplot(graph_data, aes(x=day_labtime, y=stage_for_raster, group=day_number), environment = .e)
  
  # Labels and theming
  plot <- plot + theme(axis.title.y=element_blank(), legend.title=element_blank(), axis.line = element_blank(),panel.grid.minor=element_blank())
  plot <- plot + xlab("Time (hours)")
  
  # Faceting
  plot <- plot + facet_grid(subject_code ~ .)
  
  # Scaling and Margins
  #plot <- plot + theme(panel.margin = unit(0, "npc"))
  y_breaks <- c(-3.5,-2.5,-1.5,-0.5,.5,2,3,3.5,4)
  
  plot <- plot + scale_x_continuous(limits=c(0 - epoch_length, 10 + epoch_length), expand=c(0,0)) 
  plot <- plot + scale_y_continuous(limits=c(-4, 4), breaks=y_breaks, labels=lapply(y_breaks,y_axis_andrew_formatter))
  
  # Colors
  plot <- plot + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  
  #plot <- plot + scale_fill_manual(values=alpha(c("blue", "red", "black", "purple", "green", "yellow"), 0.8))
  
  plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -.95, ymax = -0.05, data = graph_episodes[method=='raw'])# & keep==TRUE])
  
  
  ## Episodes and Cycles
  #   methods <- c('raw')#, 'classic')#, 'changepoint')
  #   r <- foreach(i=1:length(methods)) %do% {
  #     end_pos <- i * -2    
  #     text_y_pos <- end_pos + 0.5
  #     
  #     #for_this_graph <- graph_cycles[method==methods[i]]
  #     #print(nrow(for_this_graph))
  #     #print(end_pos)
  #     plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = end_pos+.2, ymax = end_pos+1.8, data = graph_episodes[method==methods[i]])
  #     #plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), fill=NA, color='black', ymin = end_pos, ymax = end_pos+1, data=for_this_graph)    
  #     #plot <- plot + geom_text(aes(x=(start_day_labtime+end_day_labtime)/2, label=cycle_number), y=text_y_pos, data=for_this_graph)
  #   }  
  #plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -1, ymax = -0.2, data = graph_episodes[method=='classic'])
  plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -1.95, ymax = -1.05, data = graph_episodes[method=='classic'])# & keep==TRUE])
  #plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), fill=NA, color='black', ymin = -1.45, ymax = -1.05, data=graph_cycles[method=="classic"])    
  #plot <- plot + geom_text(aes(x=(start_day_labtime+end_day_labtime)/2, label=cycle_number), y=-2.25, data=graph_cycles[method=="classic"])
  
  plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -2.95, ymax = -2.05, data = graph_episodes[method=='iterative'])
  #plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), fill=NA, color='black', ymin = -2.95, ymax = -2.55, data=graph_cycles[method=="iterative"])    
  #plot <- plot + geom_text(aes(x=(start_day_labtime+end_day_labtime)/2, label=cycle_number), y=-3.95, data=graph_cycles[method=="iterative"])
  
  
  plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -3.95, ymax = -3.05, data = graph_episodes[method=='changepoint_compact'])
  #plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -6, ymax = -5.2, data = graph_episodes[method=='changepoint_compact'])
  #plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), fill=NA, color='black', ymin = -4.45, ymax = -4.05, data=graph_cycles[method=="changepoint_compact"])    
  #plot <- plot + geom_text(aes(x=(start_day_labtime+end_day_labtime)/2, label=cycle_number), y=-6.45, data=graph_cycles[method=="changepoint"])
  
  
  #  plot <- plot + geom_text(aes(x=(start_day_labtime+end_day_labtime)/2, y=-1.5, label=cycle_number), data=graph_cyles[method=='classic'])
  #   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -3, ymax = -2, data = graph_periods[method=="iterative"])
  #   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), ymin = -4, ymax = -3, data = graph_cyles[method=="iterative"])
  #   
  #   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -5, ymax = -4, data = graph_periods[method=="changepoint"])
  #   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), ymin = -6, ymax = -5, data = graph_cyles[method=="changepoint"])
  
  ## Bedrest Episodes
  #  plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), ymin = 0, ymax = 4, alpha=.5, data = graph_bedrest_episodes)
  
  
  
  #plot <- plot + geom_point(shape='.', size=2)
  #plot <- plot + geom_line(data=graph_data[activity_or_bedrest_episode>0],mapping=(aes(group=activity_or_bedrest_episode))) #aes(colour=epoch_type)
  plot <- plot + geom_line(data=graph_data[activity_or_bedrest_episode>0],mapping=(aes(x=day_labtime,y=scaled_delta_power,group=activity_or_bedrest_episode))) #aes(colour=epoch_type)
  
  #file_name = file.path(output_dir, paste(subject_code, "_", l, '.svg', sep=''))
  #print(file_name)
  #print(length(days_to_graph))
  #ggsave(plot=plot, file=file_name, height=(length(days_to_graph)*1 + 0.5), width=7, scale=2.5, limitsize=FALSE)
  
  plot
  
}


y_axis_andrew_formatter <- function(x) {
  if (x == 0.5) { res <- "" }
  else if (x == 2) { res <- "" }
  else if (x == 3) { res <- "" }
  else if (x == 3.5) { res <- "" }
  else if (x == 4) { res <- "" }
  #else if (x == 3.5) { res <- "" }
  #else if (x == 0) { res <- ""}
  else if (x == -0.5) { res <- "Raw"}
  else if (x == -1.5) { res <- "Traditional"}
  else if (x == -2.5) { res <- "Extended"}
  else if (x == -3.5) { res <- "Changepoint"}
  else { res <- as.character(x) }
  
  res
}