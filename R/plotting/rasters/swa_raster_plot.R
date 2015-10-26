## Entering here is: sleep_data, subjects, periods, and nrem_cycles

## Set up for plotting
setup_raster_data <- function(sleep_data, episodes, total_delta_powers) {
  # Sleep Data Setup
  sleep_data.v <<- copy(sleep_data)
  convert_stage_for_raster(sleep_data.v)
  sleep_data.v[,c('day_number','day_labtime'):=set_days(labtime)]
  sleep_data.v[,delta_power:=normalize_a(delta_power,activity_or_bedrest_episode,cutoff=.975,target_sleep_episode=1),by='subject_code']

  
  # Episodes
  episodes.v <<- copy(episodes)
  episodes.v[,`:=`(length=convert_length_to_minutes(length))]
  episodes.v[,c('start_day_number', 'start_day_labtime', 'end_day_number', 'end_day_labtime'):=c(set_days(start_labtime),set_days(end_labtime))]
  episodes.v <<- data.table(rbindlist(list(episodes.v[start_day_number==end_day_number], split_day_spanning_blocks(episodes.v[start_day_number!=end_day_number]))))
  episodes.v[,day_number:=start_day_number]
  episodes.v[,`:=`(start_day_number=NULL, end_day_number=NULL)]
  
  # Delta Power
  total_delta_powers.v <<- copy(total_delta_powers)
  total_delta_powers.v[,c('day_number', 'day_labtime'):=set_days(labtime)]
  total_delta_powers.v[,total_delta_power:=normalize_j(total_delta_power),by='subject_code,activity_or_bedrest_episode']
  
  NULL
}


set_delta_power_groups <- function(labtimes) {
  n <- length(labtimes)
  i <- which(labtimes[-1L]-labtimes[-n] > EPOCH_LENGTH)
  label_lengths <- diff(c(0L,i))
  last_label <- (n-sum(label_lengths))
  label_lengths <- c(label_lengths,last_label)
  labels <- 1:length(label_lengths)
  r <- rep.int(labels,label_lengths)
}

fix_gaps_swa <- function(d, epoch_length = EPOCH_LENGTH, t_cycle = T_CYCLE) {
  n <- length(d$day_number)
  
  # One new point should have the following time: t_cycle - epoch_length
  # The other new point should have 0.0 start time. 
  
  # the value for both should be value_1 + (value_2 - value_1)/(time_2 - time_1) * value_1
  
  # so, we find the day diffs, take before/after indeces, and add the 2 new lines
  
  
  spanning_days <- which(d$day_number[-1L] != d$day_number[-n])
  
  new_rows <- lapply(spanning_days, function(position){
    time_1 <- d[position]$labtime
    time_2 <- d[position+1]$labtime
    time_diff_1 <- (t_cycle - epoch_length) - d[position]$day_labtime
    time_diff_2 <- time_diff_1 + epoch_length
    
    val_1 <- d[position]$total_delta_power
    val_2 <- d[position+1]$total_delta_power
    
    slope <- (val_2 - val_1)/(time_2 - time_1)
    
    new_val_1 <- val_1 + slope*time_diff_1
    new_val_2 <- val_1 + slope*time_diff_2
    
    data.table(labtime=c(time_1+time_diff_1, time_1+time_diff_2), total_delta_power=c(new_val_1, new_val_2), day_number=c(d[position]$day_number, d[position+1]$day_number), day_labtime=c(t_cycle-epoch_length, 0.0))
  })
  
  rbindlist(new_rows)
}

normalize_a <- function(values, sleep_episode, cutoff=1, target_sleep_episode=1) {
  value_subset <- values[sleep_episode==target_sleep_episode]
  
  max_v <- quantile(value_subset, cutoff)
  min_v <- min(values)
  (values - min_v)*(10/(max_v-min_v))
}

normalize_j <- function(values, cutoff=1) {
  
  
  max_v <- quantile(values,cutoff)
  min_v <- min(values)
  (values - min_v)*(10/(max_v-min_v))
}

## Raster plots!
# Plotting
plot_swa_raster <- function(subject_code, number_of_days=NA, first_day=1, activity_or_bedrest_episodes = c(1), epoch_length = EPOCH_LENGTH, doubleplot=FALSE, hour_range=c(0,12), label_sleep_episode=FALSE) {  
  ## SETUPP
#   subject_code = '103'
#   number_of_days = NA
#   first_day = 1
#   epoch_length = EPOCH_LENGTH
#   doubleplot = FALSE
#   hour_range=c(0,10)
#   label_sleep_episode = FALSE
  
  # Limit by subject
  subject_list <- c(subject_code)
  
  # Limit by day
  days_to_graph <- unique(sleep_data.v[subject_code %in% subject_list]$day_number)
  if(!is.na(number_of_days))
    days_to_graph <- days_to_graph[first_day:(first_day+number_of_days-1)]
  print(days_to_graph)
  
  # Get data subset
  graph_data <<- copy(sleep_data.v[subject_code %in% subject_list & day_number %in% days_to_graph & activity_or_bedrest_episode %in% activity_or_bedrest_episodes])
  graph_episodes <<- copy(episodes.v[subject_code %in% subject_list & day_number %in% days_to_graph & activity_or_bedrest_episode > 0 & activity_or_bedrest_episode %in% activity_or_bedrest_episodes])
  graph_delta <<- copy(total_delta_powers.v[subject_code %in% subject_list & day_number %in% days_to_graph & activity_or_bedrest_episode > 0 & activity_or_bedrest_episode %in% activity_or_bedrest_episodes])
  
  #   graph_jdata <<- copy(nrem_auc_fitted_data.v[subject_code %in% subject_list & day_number %in% days_to_graph])
#   graph_sleep_episodes <<- copy(sleep_episodes.v[subject_code %in% subject_list & day_number %in% days_to_graph])
#   
  # Draw
  .e <- environment()

  # Main Plot
  plot <- ggplot(graph_data, aes(x=day_labtime, y=stage_for_raster, group=day_number), environment = .e)
  
  # Labels and theming
  plot <- plot + ggtitle(subject_code)
  plot <- plot + theme(axis.title.y=element_blank(), legend.title=element_blank(), axis.line = element_blank(),panel.grid.minor=element_blank(),strip.text.x=element_blank())
  plot <- plot + xlab("Time (hours)")
  
  # Faceting
  if(doubleplot)
    plot <- plot + facet_grid(day_number ~ double_plot_pos)
  else
    plot <- plot + facet_grid(day_number ~ .)
  
  # Scaling and Margins
  y_breaks <- c(-7,-6.5,-6,-4.5,-3,-1.5, 0, 5, 10)

  plot <- plot + scale_x_continuous(limits=c(hour_range[1] - epoch_length, hour_range[2] + epoch_length), expand=c(0,0), breaks=c(0,4,8,12,16,20)) 
  plot <- plot + scale_y_continuous(limits=c(-7, 10), breaks=y_breaks, labels=lapply(y_breaks,y_axis_formatter))
  
  plot <- plot + theme(panel.margin.x = unit(0.00, "npc"))
  
  # Colors
  plot <- plot + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)

  ## Episodes and Cycles
  plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -1.95, ymax = -1.05, data = graph_episodes[method=='iterative'])
  plot <- plot + geom_line(data=graph_data[activity_or_bedrest_episode>0],mapping=(aes(group=activity_or_bedrest_episode))) #aes(colour=epoch_type)
  plot <- plot + geom_line(aes(y=delta_power), color="blue")
  plot <- plot + geom_line(data=graph_delta, aes(y=total_delta_power), color='red', size=1.2)
  plot <- plot + geom_point(data=graph_delta, aes(y=total_delta_power), color='red', size=3)
  
#   plot <- plot + geom_line(data=graph_jdata[data_type=="DELTA_POWER"], aes(group=interaction(data_type,activity_or_bedrest_episode,delta_power_group), color=data_type, y=value))
#   plot <- plot + geom_line(data=graph_jdata[data_type!="DELTA_POWER"], aes(group=interaction(data_type,activity_or_bedrest_episode), color=data_type, y=value))
  #plot <- plot + geom_point(data=graph_jdata[data_type!="DELTA_POWER"], aes(group=interaction(data_type,activity_or_bedrest_episode), color=data_type, y=value))
  
  # Sleep Episode Numbers
  #plot <- plot + geom_rect(aes(NULL, NULL, xmin=start_day_labtime, xmax=end_day_labtime+epoch_length), ymin=-8, ymax=-7, fill=NA, color="black", data=graph_sleep_episodes)
  if(label_sleep_episode)
    plot <- plot + geom_text(aes(x = (start_day_labtime+end_day_labtime)/2, label=activity_or_bedrest_episode), y=-7.7, size=2.5, data=graph_sleep_episodes)
  
  
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
#   else if (x == -2.5) { res <- ""}
#   else if (x == -.5) { res <- "Traditional"}
    else if (x == -1.5) { res <- "NREM-REM Episodes"}
#  else if (x == -4) { res <- "Changepoint"}
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