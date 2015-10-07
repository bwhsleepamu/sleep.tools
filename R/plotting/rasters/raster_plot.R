## Entering here is: sleep_data, subjects, periods, and nrem_cycles


## Set up for plotting
setup_raster_data <- function(sleep_data, episodes, cycles) { #, cycles, bedrest_episodes) {
  # Sleep Data Setup
    sleep_data.v <<- copy(sleep_data)
    convert_stage_for_raster(sleep_data.v)
    sleep_data.v[,c('day_number','day_labtime'):=set_days(labtime)]
    sleep_data.v <<- double_plot(sleep_data.v,TRUE)
    
    # Sleep Episodes
    sleep_episodes.v <<- copy(sleep_episodes)
    sleep_episodes.v[,c('start_day_number', 'start_day_labtime', 'end_day_number', 'end_day_labtime'):=c(set_days(start_labtime),set_days(end_labtime))]
    sleep_episodes.v <<- data.table(rbindlist(list(sleep_episodes.v[start_day_number==end_day_number], split_day_spanning_blocks(sleep_episodes.v[start_day_number!=end_day_number]))))
    sleep_episodes.v[,length:=end_day_labtime-start_day_labtime]
    sleep_episodes.v <<- sleep_episodes.v[,select_longer_split(.SD),by='subject_code,activity_or_bedrest_episode']
    sleep_episodes.v[,day_number:=start_day_number]
    sleep_episodes.v[,`:=`(start_day_number=NULL, end_day_number=NULL)]
    sleep_episodes.v <<- double_plot(sleep_episodes.v,TRUE)
    
    # Episodes
    episodes.v <<- copy(episodes)
    episodes.v[,`:=`(length=convert_length_to_minutes(length))]
    episodes.v[,c('start_day_number', 'start_day_labtime', 'end_day_number', 'end_day_labtime'):=c(set_days(start_labtime),set_days(end_labtime))]
    episodes.v <<- data.table(rbindlist(list(episodes.v[start_day_number==end_day_number], split_day_spanning_blocks(episodes.v[start_day_number!=end_day_number]))))
    episodes.v[,day_number:=start_day_number]
    episodes.v[,`:=`(start_day_number=NULL, end_day_number=NULL)]
    episodes.v <<- double_plot(episodes.v,TRUE)
    
    
    # Cycles
    cycles.v <<- copy(cycles)
    cycles.v[,`:=`(length=convert_length_to_minutes(length))]
    cycles.v[,c('start_day_number', 'start_day_labtime', 'end_day_number', 'end_day_labtime'):=c(set_days(start_labtime),set_days(end_labtime))]
    cycles.v <<- data.table(rbindlist(list(cycles.v[start_day_number==end_day_number], split_day_spanning_blocks(cycles.v[start_day_number!=end_day_number]))))
    cycles.v[,day_number:=start_day_number]
    cycles.v[,`:=`(start_day_number=NULL, end_day_number=NULL)]
    cycles.v <<- double_plot(cycles.v,TRUE)
  
    # Mel Phase
    melatonin_phase.v <<- copy(melatonin_phase)
    melatonin_phase.v[,c('day_number','day_labtime'):=set_days(labtime)]
    
    melatonin_phase.v <- double_plot(melatonin_phase.v)
    
    NULL
}

## Raster plots!
# Plotting
plot_raster <- function(subject_code, number_of_days=NA, first_day=1, epoch_length = EPOCH_LENGTH) {  
  ## SETUPP
#   subject_code = '3450GX'
#   number_of_days = NA
#   first_day = 1
#   epoch_length = EPOCH_LENGTH
  
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
  graph_sleep_episodes <<- copy(sleep_episodes.v[subject_code %in% subject_list & day_number %in% days_to_graph])
  graph_mel_phase <<- copy(melatonin_phase.v[subject_code %in% subject_list & day_number %in% days_to_graph])
  
  # Draw
  .e <- environment()
  
  # Main Plot
  plot <- ggplot(graph_data, aes(x=day_labtime, y=stage_for_raster, group=day_number), environment = .e)
  
  # Labels and theming
  plot <- plot + ggtitle(subject_code)
  plot <- plot + theme(axis.title.y=element_blank(), legend.title=element_blank(), axis.line = element_blank(),panel.grid.minor=element_blank(),strip.text.x=element_blank())
  plot <- plot + xlab("Time (hours)")
  
  # Faceting
  plot <- plot + facet_grid(day_number ~ double_plot_pos)
  
  # Scaling and Margins
  y_breaks <- c(-8.5,-8, -7.5, -6, -4.5, -3.5, -2.5, -1.5,-.5, 0)
  
  plot <- plot + scale_x_continuous(limits=c(0 - epoch_length, 24 + epoch_length), expand=c(0,0), breaks=c(0,4,8,12,16,20)) 
  plot <- plot + scale_y_continuous(limits=c(-9, 0), breaks=y_breaks, labels=lapply(y_breaks,y_axis_formatter))
  
  plot <- plot + theme(panel.margin.x = unit(0.00, "npc"))
  
  # Colors
  plot <- plot + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  
  ## Episodes and Cycles
  plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -.95, ymax = -0.05, data = graph_episodes[label!="UNDEF" & method=='raw'])
  plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -1.95, ymax = -1.05, data = graph_episodes[method=='classic'])# & keep==TRUE])
  #plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -2.95, ymax = -2.05, data = graph_episodes[method=='iterative'])
  #plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -3.95, ymax = -3.05, data = graph_episodes[method=='changepoint_compact'])
  plot <- plot + geom_line(data=graph_data[activity_or_bedrest_episode>0],mapping=(aes(group=interaction(activity_or_bedrest_episode)))) #aes(colour=epoch_type)

  # Sleep Episode Numbers
  #plot <- plot + geom_rect(aes(NULL, NULL, xmin=start_day_labtime, xmax=end_day_labtime+epoch_length), ymin=-8, ymax=-7, fill=NA, color="black", data=graph_sleep_episodes)
  plot <- plot + geom_text(aes(x = (start_day_labtime+end_day_labtime)/2, label=activity_or_bedrest_episode), y=-8.7, size=2.5, data=graph_sleep_episodes)
  
  
  # Melatonin Phase
  plot <- plot + geom_vline(aes(xintercept = day_labtime), size=2,colour="red",data=graph_mel_phase)
  
  plot
}





## Helpers
select_longer_split <- function(d) {
  max_length <- max(d$length)
  d[length==max_length]
}

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
  conv_map <- c(-7.5,-8,-8.5,-8.5,-4.5,-6)
  
  d[epoch_type!='UNDEF', stage_for_raster:=conv_map[stage]]
  d[epoch_type=='UNDEF', stage_for_raster:=-4.0]
}

y_axis_formatter <- function(x) {
  if (x == -4.5) { res <- "WAKE" }
  else if (x == -6) { res <- "REM" }
  else if (x == -7.5) { res <- "" }
  else if (x == -8) { res <- "NREM" }
  else if (x == -8.5) { res <- "" }
  else if (x == -.5) { res <- "Raw"}
  else if (x == -1.5) { res <- "Traditional"}
  else if (x == -2.5) { res <- "Extended"}
  else if (x == -3.5) { res <- "Changepoint" }
  else { res <- as.character(x) }
  
  res
}


# Double-Plotting
double_plot <- function(dataset, plot_double=TRUE) {
  ### DANGER ###
  # This function changes the date of the right double-plot, causing the actual dates to not match up with the times. 
  # The correct date for a given set of times is (day + double_plot_pos)
  ###
  if(plot_double) { 
    right_side <- copy(dataset)
    left_side <- copy(dataset)
    
    left_side[,double_plot_pos:=0]
    right_side[,double_plot_pos:=1]
    right_side[,day_number:=day_number-1]
    
    #     if(!is.null(right_side$day_s))
    #       r_df$day_s <- format(r_df$day, format="%Y-%m-%d")
    return(rbind(left_side, right_side))
  } else {
    dataset[, double_plot_pos:=0]    
  }
}