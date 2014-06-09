## Entering here is: sleep_data, subjects, periods, and nrem_cycles


## Set up for plotting
main_vis_setup <- function(sleep_data, periods, nrem_cycles) {
  
  sleep_data.v <- copy(sleep_data)
  convert_stage_for_raster(sleep_data.v)
  
  periods.v <- copy(periods)
  nrem_cycles.v <- copy(nrem_cycles)
  
  sleep_periods.v <- copy(sleep_periods)
  
  ## Get Labtimes
  periods.v[,`:=`(start_labtime=convert_to_labtimes(start_position, sleep_data), end_labtime=convert_to_labtimes(end_position, sleep_data), length=convert_length_to_minutes(length))]
  nrem_cycles.v[,`:=`(start_labtime=convert_to_labtimes(start_position, sleep_data), end_labtime=convert_to_labtimes(end_position, sleep_data), length=convert_length_to_minutes(length))]
  
  ## Set up Days and Day labtimes
  sleep_data.v[,c('day_number','day_labtime'):=set_days(labtime)]
  
  periods.v[,c('start_day_number', 'start_day_labtime', 'end_day_number', 'end_day_labtime'):=c(set_days(start_labtime),set_days(end_labtime))]
  nrem_cycles.v[,c('start_day_number', 'start_day_labtime', 'end_day_number', 'end_day_labtime'):=c(set_days(start_labtime),set_days(end_labtime))]
  sleep_periods.v[,c('start_day_number', 'start_day_labtime', 'end_day_number', 'end_day_labtime'):=c(set_days(start_labtime),set_days(end_labtime))]
  
  ## Deal with blocks that span multiple days
  periods.v <- rbindlist(list(periods.v[start_day_number==end_day_number], split_day_spanning_blocks(periods.v[start_day_number!=end_day_number])))
  nrem_cycles.v <- rbindlist(list(nrem_cycles.v[start_day_number==end_day_number], split_day_spanning_blocks(nrem_cycles.v[start_day_number!=end_day_number])))
  sleep_periods.v <- rbindlist(list(sleep_periods.v[start_day_number==end_day_number], split_day_spanning_blocks(sleep_periods.v[start_day_number!=end_day_number])))
  
  ## Re-scale day numbers
  periods.v[,day_number:=start_day_number]
  sleep_periods.v[,day_number:=start_day_number]
  nrem_cycles.v[,day_number:=start_day_number]
  periods.v[,`:=`(start_day_number=NULL, end_day_number=NULL)]
  sleep_periods.v[,`:=`(start_day_number=NULL, end_day_number=NULL)]
  nrem_cycles.v[,`:=`(start_day_number=NULL, end_day_number=NULL)]
  
  # TODO
  
  
  ## I THINK WE ARE READY TO PLOT
}

## Raster plots!
# Plotting
plot.raster <- function(sleep_data, periods, nrem_cycles, epoch_length=EPOCH_LENGTH) {  
  
  
  graph_data <- copy(sleep_data.v[subject_code == '3335GX' & day_number %in% c(175, 176,177)])
  graph_periods <- copy(periods.v[subject_code == '3335GX' & day_number %in% c(175, 176,177)])
  graph_cyles <- copy(nrem_cycles.v[subject_code == '3335GX' & day_number %in% c(175, 176,177)])
  graph_sleep_periods <- copy(sleep_periods.v[subject_code == '3335GX' & day_number %in% c(175, 176,177)])
  
  # Draw
  .e <- environment()

  # Main Plot
  plot <- ggplot(graph_data, aes(day_labtime, stage_for_raster, group=day_number), environment = .e)
  # Faceting
  plot <- plot + facet_grid(day_number ~ .)
  
  # Scaling and Margins
  #plot <- plot + theme(panel.margin = unit(0, "npc"))
  y_breaks <- c(-5,-3,-1,0,.5,1.5,2,2.5,3,4)

  plot <- plot + scale_x_continuous(limits=c(0 - epoch_length, 24 + epoch_length), expand=c(0,0)) 
  plot <- plot + scale_y_continuous(limits=c(-6.5, 4.5), breaks=y_breaks, labels=lapply(y_breaks,y_axis_formatter))
  
  # Colors
  #plot <- plot + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)

 
  
  #plot <- plot + scale_fill_manual(values=alpha(c("blue", "red", "black", "purple", "green", "yellow"), 0.8))
  
  ## Periods and Cycles
  methods <- c('classic', 'iterative', 'changepoint')
  r <- foreach(i=1:3) %do% {
    end_pos <- i * -2    
    
    plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = end_pos+1, ymax = end_pos+2, data = graph_periods[method==methods[i]])
    plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), fill=NA, color='black', ymin = end_pos, ymax = end_pos+1, data = graph_cyles[method==methods[i]])    
    #plot <- plot + geom_text(aes(x=(start_day_labtime+end_day_labtime)/2, y=end_pos + .5, label=cycle_number), data=graph_cyles[method==methods[i]])
  }  
  
  plot <- plot + geom_text(aes(x=(start_day_labtime+end_day_labtime)/2, y=-1.5, label=cycle_number), data=graph_cyles[method='classic'])
#   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -3, ymax = -2, data = graph_periods[method=="iterative"])
#   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), ymin = -4, ymax = -3, data = graph_cyles[method=="iterative"])
#   
#   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -5, ymax = -4, data = graph_periods[method=="changepoint"])
#   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), ymin = -6, ymax = -5, data = graph_cyles[method=="changepoint"])
  
  ## Sleep Periods
  plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, alpha=.5), ymin = 0, ymax = 4, data = graph_sleep_periods)
  
  
  
  #plot <- plot + geom_point(shape='.', size=2)
  plot <- plot + geom_line()
  
  
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
  conv_map <- c(1.5,2,2.5,3,.5,4)
  
  d[epoch_type!='UNDEF', stage_for_raster:=conv_map[stage]]
  d[epoch_type=='UNDEF', stage_for_raster:=0]
}

y_axis_formatter <- function(x) {
  if (x == .5) { res <- "WAKE" }
  else if (x == 1.5) { res <- "Stage 1" }
  else if (x == 2) { res <- "Stage 2" }
  else if (x == 2.5) { res <- "Stage 3" }
  else if (x == 3) { res <- "Stage 4" }
  else if (x == 4) { res <- "REM" }
  else if (x == 0) { res <- "UNDEF"}
  else if (x == -1) { res <- "Classic"}
  else if (x == -3) { res <- "Iterative"}
  else if (x == -5) { res <- "Changepoint"}
  else { res <- as.character(x) }
  
  res
}