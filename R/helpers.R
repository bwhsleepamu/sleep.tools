
plot_raster <- function(graph_subject = SUBJECT_CODE, 
                        number_of_days=NUMBER_OF_DAYS, 
                        first_day=FIRST_DAY, 
                        doubleplot=DOUBLE_PLOT, 
                        hour_range=c(0,24), 
                        label_sleep_episode=TRUE,
                        color_palette = COLOR_PALETTE) {  
#   graph_subject <- SUBJECT_CODE    
#   doubleplot = TRUE
#   first_day = 1
#   number_of_days = 5
#   hour_range=c(0,24)
#   color_palette <- COLOR_PALETTE

  
  # Limit by day
  days_to_graph <- unique(main_data.v[subject_code == graph_subject]$day_number)
  if(!is.na(number_of_days))
    days_to_graph <- days_to_graph[first_day:(first_day+number_of_days-1)]
  
  # Get data subset
  graph_data <<- copy(main_data.v[subject_code == graph_subject & day_number %in% days_to_graph])
  graph_sleep_episodes <<- copy(activity_or_bedrest_episodes.v[subject_code == graph_subject & day_number %in% days_to_graph])
  graph_cbt <<- copy(cbt_data.v[subject_code == graph_subject & day_number %in% days_to_graph])
  
  # Draw
  
  ## Main Plot
  plot <- ggplot(graph_data, aes(group=day_number))
  
  ## Labels and theming
  plot <- plot + theme(axis.title.y=element_blank(), legend.title=element_blank(), axis.line = element_blank(),panel.grid.minor=element_blank(),strip.text.x=element_blank())
  plot <- plot + xlab("Time (hours)")
  
  ## Faceting
  if(doubleplot) {
    plot <- plot + facet_grid(day_number ~ double_plot_pos)
  } else {
    plot <- plot + facet_grid(day_number ~ .)
  }
  
  
  # Scaling and Margins
  plot <- plot + scale_x_continuous(limits=c(hour_range[1] - EPOCH_LENGTH, hour_range[2] + EPOCH_LENGTH), expand=c(0,0), breaks=c(0,4,8,12,16,20)) 
  plot <- plot + scale_y_continuous(limits=c(0, 10.1))
  
  plot <- plot + theme(panel.margin.x = unit(0.00, "npc"))
  
  # Colors
  plot <- plot + scale_fill_manual(values=color_palette) + scale_colour_manual(values=color_palette)
  
  # Sleep Episodes
  plot <- plot + geom_rect(aes(xmin = start_day_labtime, xmax = end_day_labtime + EPOCH_LENGTH, ymin = 0, ymax = 10), alpha=.3, fill='black', data = graph_sleep_episodes[label==0])
  
  # Performance
  #plot <- plot + geom_line(aes(x=day_labtime, y=performance), colour='blue') 
  
  # Light
  plot <- plot + geom_line(aes(x=day_labtime, y=irradiance), colour='orange')
  
  # CBT Min
  plot <- plot + geom_vline(aes(xintercept = day_labtime), data=graph_cbt, colour='red', size=1)
  
  # Sleep Episode Numbers
  #plot <- plot + geom_rect(aes(NULL, NULL, xmin=start_day_labtime, xmax=end_day_labtime+epoch_length), ymin=-8, ymax=-7, fill=NA, color="black", data=graph_sleep_episodes)
  #   if(label_sleep_episode)
  #     plot <- plot + geom_text(aes(x = (start_day_labtime+end_day_labtime)/2, label=activity_or_bedrest_episode), y=-7.7, size=2.5, data=graph_sleep_episodes)
  
  plot
}



setup_raster_data <- function() {
  # Sleep Episodes
  activity_or_bedrest_episodes <<- main_data[,chunk(state,.I)]
  activity_or_bedrest_episodes[,`:=`(start_labtime=main_data[start_position]$labtime, end_labtime=main_data[end_position]$labtime)]
  
  # Set Subjects
  main_data[,subject_code:=SUBJECT_CODE]
  cbt_data[,subject_code:=SUBJECT_CODE]
  activity_or_bedrest_episodes[,subject_code:=SUBJECT_CODE]
  
  # Sleep Data Setup
  main_data.v <<- copy(main_data)
  main_data.v[,c('day_number','day_labtime'):=set_days(labtime)]
  
  # Log-transform and Scale irradiance
  main_data.v[,irradiance:=irradiance_raw]
  main_data.v[,irradiance:=as.double(irradiance)]
  main_data.v[irradiance!=0, irradiance:=log2(irradiance)]
  main_data.v[,irradiance:=normalize_cpss(irradiance, val_range = c(0,10))]
  
  # Normalize Performance
  #main_data.v[,performance:=normalize_cpss(performance, val_range=c(5,10))]
  
  # Sleep Episodes
  activity_or_bedrest_episodes.v <<- copy(activity_or_bedrest_episodes)
  activity_or_bedrest_episodes.v[,c('start_day_number', 'start_day_labtime', 'end_day_number', 'end_day_labtime'):=c(set_days(start_labtime),set_days(end_labtime))]
  activity_or_bedrest_episodes.v <<- data.table(rbindlist(list(activity_or_bedrest_episodes.v[start_day_number==end_day_number], split_day_spanning_blocks(activity_or_bedrest_episodes.v[start_day_number!=end_day_number]))))
  activity_or_bedrest_episodes.v[,length:=end_day_labtime-start_day_labtime]
  #activity_or_bedrest_episodes.v <<- activity_or_bedrest_episodes.v[,select_longer_split(.SD),by='subject_code,activity_or_bedrest_episode']
  activity_or_bedrest_episodes.v[,day_number:=start_day_number]
  activity_or_bedrest_episodes.v[,`:=`(start_day_number=NULL, end_day_number=NULL)]
  
  # CBT
  setnames(cbt_data, 'cbt_min_labtime', 'labtime')
  cbt_data.v <<- copy(cbt_data)
  cbt_data.v[,c('day_number','day_labtime'):=set_days(labtime)]
  
  if(DOUBLE_PLOT) {
    activity_or_bedrest_episodes.v <<- double_plot(activity_or_bedrest_episodes.v,TRUE)
    cbt_data.v <<-double_plot(cbt_data.v,TRUE)
    main_data.v <<- double_plot(main_data.v,TRUE)
  }
  
  
  NULL
}


chunk <- function(categories, indeces) {
  ## Might be possible to elaborate on this function using the code for rle
  
  reference = indeces[1] - 1
  rle_results <- rle(as.character(categories))
  
  # Get positions by cumulative sum of lengths
  positions <- cumsum(rle_results$lengths)
  
  # The ending positions for each chunk are represented by the cumsum of lengths.
  # The only correction we need is for the reference index
  
  # Since each following chunk starts one position after the end of the previous chunk
  # we calculate the start positions by adding 1 to each end position and artificially inserting
  # the start position of the first chunk==1. 
  
  # Again, we correct for the reference index, and also trim to get rid of the last value
  # (since it references a bout that does not exist). 
  
  start_positions <- (c(0, positions)+1)[1:length(positions)] + reference
  end_positions <- positions + reference
  
  #end_positions <- res$lengths
  
  data.table(label=rle_results$values, start_position=start_positions, end_position=end_positions, length=rle_results$lengths)
}


normalize_cpss <- function(values, cutoff=1, val_range=c(0,10)) {
  max_v <- quantile(values,cutoff)
  min_v <- min(values)
  r <- diff(val_range)
  
  ((values - min_v)*(r/(max_v-min_v)))+val_range[1]
}

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
  else if (x == -2.5) { res <- ""}
  else if (x == -.5) { res <- "Traditional"}
  else if (x == -1.5) { res <- "Extended"}
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
