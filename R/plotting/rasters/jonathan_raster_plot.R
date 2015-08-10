## Entering here is: sleep_data, subjects, periods, and nrem_cycles

## Set up for plotting
setup_raster_data <- function(sleep_data, episodes, cycles, jonathan_data) {
  # Sleep Data Setup
  sleep_data.v <<- copy(sleep_data)
  convert_stage_for_raster(sleep_data.v)
  sleep_data.v[,c('day_number','day_labtime'):=set_days(labtime)]
  
  # Episodes
  episodes.v <<- copy(episodes)
  episodes.v[,`:=`(length=convert_length_to_minutes(length))]
  episodes.v[,c('start_day_number', 'start_day_labtime', 'end_day_number', 'end_day_labtime'):=c(set_days(start_labtime),set_days(end_labtime))]
  episodes.v <<- data.table(rbindlist(list(episodes.v[start_day_number==end_day_number], split_day_spanning_blocks(episodes.v[start_day_number!=end_day_number]))))
  episodes.v[,day_number:=start_day_number]
  episodes.v[,`:=`(start_day_number=NULL, end_day_number=NULL)]
  
  # Cycles
  cycles.v <<- copy(cycles)
  cycles.v[,`:=`(length=convert_length_to_minutes(length))]
  cycles.v[,c('start_day_number', 'start_day_labtime', 'end_day_number', 'end_day_labtime'):=c(set_days(start_labtime),set_days(end_labtime))]
  cycles.v <<- data.table(rbindlist(list(cycles.v[start_day_number==end_day_number], split_day_spanning_blocks(cycles.v[start_day_number!=end_day_number]))))
  cycles.v[,day_number:=start_day_number]
  cycles.v[,`:=`(start_day_number=NULL, end_day_number=NULL)]
  
  # Jonathan Data
  jonathan_data.v <<- copy(jonathan_data)
  jonathan_data.v[,c('day_number', 'day_labtime'):=set_days(labtime)]
  jonathan_data.v <<- jonathan_data.v[!is.na(value)]
  
  
  
  jonathan_data.v[,value:=normalize_j(value),by='subject_code,activity_or_bedrest_episode,data_type']
  
#   max_v <- max(jonathan_data.v[data_type!="AUC"]$value)
#   min_v <- min(jonathan_data.v[data_type!="AUC"]$value)
#   jonathan_data.v[data_type!="AUC",value:=(value - min_v)*(10/(max_v-min_v))]
# 
#   max_v <- max(jonathan_data.v[data_type=="AUC"]$value)
#   min_v <- min(jonathan_data.v[data_type=="AUC"]$value)
#   jonathan_data.v[data_type=="AUC",value:=(value - min_v)*(10/(max_v-min_v))]
#   
    
  NULL
}

normalize_j <- function(values) {
  max_v <- max(values)
  min_v <- min(values)
  (values - min_v)*(10/(max_v-min_v))
}

## Raster plots!
# Plotting
plot_raster <- function(subject_code, number_of_days=NA, first_day=1, epoch_length = EPOCH_LENGTH) {  
  ## SETUPP
#   subject_code = '18B2XX'
#   number_of_days = 4
#   first_day = 14

  # Limit by subject
  subject_list <- c(subject_code)
  
  # Limit by day
  days_to_graph <- unique(sleep_data.v[subject_code %in% subject_list]$day_number)
  if(!is.na(number_of_days))
    days_to_graph <- days_to_graph[first_day:(first_day+number_of_days-1)]
  print(days_to_graph)
  
  # Get data subset
  graph_data <<- copy(sleep_data.v[subject_code %in% subject_list & day_number %in% days_to_graph])
  graph_episodes <<- copy(episodes.v[subject_code %in% subject_list & day_number %in% days_to_graph & activity_or_bedrest_episode > 0])
  graph_jdata <<- copy(jonathan_data.v[subject_code %in% subject_list & day_number %in% days_to_graph])
  
  # Draw
  .e <- environment()

  # Main Plot
  plot <- ggplot(graph_data, aes(x=day_labtime, y=stage_for_raster, group=day_number), environment = .e)
  
  # Labels and theming
  plot <- plot + ggtitle(subject_code)
  plot <- plot + theme(axis.title.y=element_blank(), legend.title=element_blank(), axis.line = element_blank(),panel.grid.minor=element_blank())
  plot <- plot + xlab("Time (hours)")
  
  # Faceting
  plot <- plot + facet_grid(day_number ~ .)
  
  # Scaling and Margins
  y_breaks <- c(-7,-6.5,-6,-4.5,-3, -2.5, -1.5,-.5, 5, 10)

  plot <- plot + scale_x_continuous(limits=c(0 - epoch_length, 24 + epoch_length), expand=c(0,0), breaks=c(0,12,24), minor_breaks=c(3,6,9,15,18,21)) 
  plot <- plot + scale_y_continuous(limits=c(-7, 10), breaks=y_breaks, labels=lapply(y_breaks,y_axis_formatter))
  
  # Colors
  plot <- plot + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)

  ## Episodes and Cycles
  plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -.95, ymax = -0.05, data = graph_episodes[method=='classic'])# & keep==TRUE])
  plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -1.95, ymax = -1.05, data = graph_episodes[method=='iterative'])
  plot <- plot + geom_line(data=graph_data[activity_or_bedrest_episode>0],mapping=(aes(group=activity_or_bedrest_episode))) #aes(colour=epoch_type)
  plot <- plot + geom_line(data=graph_jdata[data_type=="DELTA_POWER"], aes(group=interaction(data_type,activity_or_bedrest_episode), color=data_type, y=value))
  plot <- plot + geom_line(data=graph_jdata[data_type!="DELTA_POWER"], aes(group=interaction(data_type,activity_or_bedrest_episode), color=data_type, y=value))
  plot <- plot + geom_point(data=graph_jdata[data_type!="DELTA_POWER"], aes(group=interaction(data_type,activity_or_bedrest_episode), color=data_type, y=value))
  
  plot
}





## Helpers
split_day_spanning_blocks <- function(dt, t_cycle=T_CYCLE, epoch_length=EPOCH_LENGTH){
  first_division <- dt
  second_division <- copy(dt)
  
  new_end_day_labtime <- t_cycle-epoch_length
  
  first_division[,`:=`(end_day_number=start_day_number, end_day_labtime=new_end_day_labtime)]
  second_division[,`:=`(start_day_number=end_day_number, start_day_labtime=0)]
  
  rbindlist(list(first_division, second_division))
}


convert_length_to_minutes <- function(lengths, epoch_length=EPOCH_LENGTH) {
  lengths * epoch_length * 60
} 

convert_to_labtimes <- function(indeces, sleep_data) {
  sleep_data$labtime[indeces]
  
}

set_days <- function(labtimes, t_cycle=T_CYCLE) {
  day_numbers <- floor(labtimes / t_cycle)
  day_labtimes <- (labtimes - (day_numbers * t_cycle))

  list(day_numbers, day_labtimes)
}

convert_stage_for_raster <- function(d) {
  conv_map <- c(-6,-6.5,-7,-7,-3,-4.5)
  
  d[epoch_type!='UNDEF', stage_for_raster:=conv_map[stage]]
  d[epoch_type=='UNDEF', stage_for_raster:=-2.5]
}

y_axis_formatter <- function(x) {
  if (x == -3) { res <- "WAKE" }
  else if (x == -4.5) { res <- "REM" }
  else if (x == -6) { res <- "" }
  else if (x == -6.5) { res <- "NREM" }
  else if (x == -7) { res <- "" }
  #else if (x == 3.5) { res <- "" }
  else if (x == -2.5) { res <- ""}
  else if (x == -.5) { res <- "Traditional"}
    else if (x == -1.5) { res <- "Extended"}
#  else if (x == -4) { res <- "Changepoint"}
  else { res <- as.character(x) }
  
  res
}