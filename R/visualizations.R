## Entering here is: sleep_data, subjects, periods, and nrem_cycles


## Set up for plotting
main_vis_setup <- function(sleep_data, periods, nrem_cycles) {
  
  sleep_data.v <- copy(sleep_data)
  periods.v <- copy(periods)
  nrem_cycles.v <- copy(nrem_cycles)
  
  ## Get Labtimes
  periods.v[,`:=`(start_labtime=convert_to_labtimes(start_position, sleep_data), end_labtime=convert_to_labtimes(end_position, sleep_data), length=convert_length_to_minutes(length))]
  nrem_cycles.v[,`:=`(start_labtime=convert_to_labtimes(start_position, sleep_data), end_labtime=convert_to_labtimes(end_position, sleep_data), length=convert_length_to_minutes(length))]
  
  ## Set up Days and Day labtimes
  sleep_data.v[,c('day_number','day_labtime'):=set_days(labtime)]
  periods.v
  
  
  
  
  
  ## Split up blocks spanning two days
  
  
  
}



## Raster plots!
# Plotting
plot.raster <- function(sleep_data, periods, nrem_cycles) {  
  
  
  # Draw
  .e <- environment()

  # Main Plot
  plot <- ggplot(df, aes(day_labtime, stage, group=day_number), environment = .e)
  # Faceting
  plot <- plot + facet_grid(day_number ~ .)
  # Scaling and Margins
  #plot <- plot + theme(panel.margin = unit(0, "npc"))
  plot <- plot + scale_x_continuous(limits=c(0 - EPOCH_LENGTH, 24 + EPOCH_LENGTH), expand=c(0,0)) 
  plot <- plot + scale_y_continuous(limits=c(-2, 10))
  
  # Colors
  #plot <- plot + scale_fill_manual(values=alpha(c("blue", "red", "black", "purple", "green", "yellow"), 0.8))
  
  
  if(is.null(secondary_bouts))
    plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + EPOCH_LENGTH, fill = bout_type), ymin = 0, ymax = 10, data = primary_bouts)
  else {
    plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + EPOCH_LENGTH, fill = bout_type), ymin = 0, ymax = 4.5, data = primary_bouts)    
    plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + EPOCH_LENGTH, fill = bout_type), ymin = 5.5, ymax = 10, data = secondary_bouts)    
  }
  plot <- plot + geom_point(aes(group=day_number), shape='.')
  
  plot
  
}




## Helpers
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

setup_bouts_for_raster <- function(bouts, min_day_number, t_cycle) {
  ## TODO: REFACTOR
  # set day numbers + labtimes for bouts
  bouts$start_day_number <- floor(bouts$start_labtime / T_CYCLE)
  bouts$start_day_labtime <- (bouts$start_labtime - (bouts$start_day_number * T_CYCLE))
  bouts$start_day_number <- bouts$start_day_number - min_day_number
  bouts$end_day_number <- floor(bouts$end_labtime / T_CYCLE)
  bouts$end_day_labtime <- (bouts$end_labtime - (bouts$end_day_number * T_CYCLE))
  bouts$end_day_number <- bouts$end_day_number - min_day_number
  
  # Clean up bouts that span days
  
  # Nothing needs to be done to these:
  clean_bouts <- bouts[bouts$start_day_number == bouts$end_day_number,]
  
  # These bouts span days
  to_be_cleaned <- bouts[bouts$start_day_number != bouts$end_day_number,]
  
  first_cleaned <- ddply(to_be_cleaned, .(start_day_number), first_div)
  second_cleaned <- ddply(to_be_cleaned, .(start_day_number), second_div)
  bouts <- rbind(first_cleaned, second_cleaned, clean_bouts)
  
  bouts$day_number <- bouts$start_day_number
  
  bouts[which(bouts$sleep_wake_period > 0),]
}
