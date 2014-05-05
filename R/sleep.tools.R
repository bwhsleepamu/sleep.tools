# TODO
# 1. Speed up! (maybe using data tables??)
# Stats!

# Required Packages
library("cpm")
library("ggplot2")
library(scales)
#library(grid)
library(plyr)
library(iterators)


# My Sources
source("R/helpers.R")

# 1. Load the sleep files
# 2. Have 3 methods for analysis
# 3. Get stats on all three methods
T_CYCLE = 24.0
EPOCH_SECONDS <- 30
EPOCH_LENGTH <- (EPOCH_SECONDS / 3600)
REM_MIN_PERIOD_LENGTH = 10
NREM_MIN_PERIOD_LENGTH = 30

## Loader
load_sleep_file <- function(file_path) {
  df <- read.csv(file_path)
  colnames(df) <- c("subject_code", "sleep_wake_period", "labtime", "stage")
  min_day_number <- (min(floor(df$labtime / T_CYCLE)) - 1)
  res <- set_up_data_frame(df, T_CYCLE)
  
  df <- res$df
  min_day_number <- res$min_day_number
  
  list(df=df, min_day_number=min_day_number)
}

# Methods
## Method 1
bouts.changepoint <- function(df) {
  
  bouts <- ddply(df, .(sleep_wake_period), function(df) {
    changepoint_results <- processStream(df$epoch_type, cpmType="Mann-Whitney", ARL0=10000, startup=20)
    changepoint_rows <- df[changepoint_results$changePoints,]
    
    # Uses changepoint indeces to create two columns, with start and end indeces for each chunk
    chunks <- as.data.frame(cbind(c(1, changepoint_results$changePoints), c(changepoint_results$changePoints, nrow(df))))
    colnames(chunks) <- c("start_index", "end_index")
    
    # Uses the sleep data to determine the most frequent type of epoch in each chunk
    # Creates bouts with start and end labtimes
    bouts <- ddply(chunks, .(start_index, end_index), calculate_bouts, df=df)
    bouts$length <- bouts$end_index - bouts$start_index
    
    bouts
  })
  
  bouts[,c(1,4,5,6,7)]
}

# Definitions for Methods 2 and 3
### NREM Period
# >=15 minute bout of consecutive (stage 2,3,4?) sleep, 
### REM Period
# >=5 minute bout of consecutive REM sleep,  
### REM Cycle
### NREM Cycle
## Method 2
bouts.classic <- function(df) {
  bouts <- ddply(df, .(sleep_wake_period), function(df) {
    bouts <- chunk_epochs(df)
    bouts <- merge_undefined_bouts(bouts)
    bouts <- combine_identical_neighbors(bouts)
    bouts <- create_nrem_rem_periods(bouts)

    bouts
  })
  
  
}

## Method 3
bouts.improved <- function(df) {
  ## TODO
}

# Cycles

# Plotting
plot.bouts <- function(df, primary_bouts, secondary_bouts=NULL) {  
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
    plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + EPOCH_LENGTH, fill = bout_type), ymin = 0, ymax = 5, data = primary_bouts)    
    plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + EPOCH_LENGTH, fill = bout_type), ymin = 5, ymax = 10, data = secondary_bouts)    
  }
  plot <- plot + geom_point(aes(group=day_number), shape='.')
  
  plot
  
}


