# TODO
# 1. Speed up! (maybe using data tables??)
# Stats!

# Required Packages
library("cpm")
library("ggplot2")
library(scales)
library(gdata)
library(plyr)
library(iterators)
library(foreach)
library(parallel)
library(data.table)
#library(doMC)


# My Sources
source("R/helpers.R")
source("R/data.table.R")

# 1. Load the sleep files
# 2. Have 3 methods for analysis
# 3. Get stats on all three methods
T_CYCLE = 24.0
EPOCH_SECONDS <- 30
EPOCH_LENGTH <- (EPOCH_SECONDS / 3600)
REM_MIN_PERIOD_LENGTH = 10
NREM_MIN_PERIOD_LENGTH = 30



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
  }, .parallel=TRUE)
  
  bouts <- bouts[,c(1,4,5,6,7)]
  bouts$method <- 'changepoint'
  bouts
}

# Definitions for Methods 2 and 3
### NREM Period
# >=15 minute bout of consecutive (stage 2,3,4?) sleep, 
### REM Period
# >=5 minute bout of consecutive REM sleep,  
### REM Cycle
### NREM Cycle
## Method 2
bouts.classic <- function(df, subject_code=NULL) {
  cat(sprintf("1. subject_code: %s | dims: %s x %s | first: %s | last: %s \n", subject_code, dim(df)[[1]], dim(df)[[2]], df$labtime[[1]], df$labtime[[length(df$labtime)]]))
  
  bouts <- ddply(df, .(sleep_wake_period), function(df) {
    cat(sprintf("2. subject_code: %s | dims: %s x %s | first: %s | last: %s \n", subject_code, dim(df)[[1]], dim(df)[[2]], df$labtime[[1]], df$labtime[[length(df$labtime)]]))
   
    untransformed_bouts <- chunk_epochs(df)
    #cat(sprintf("3. dim: %s x %s\n", dim(untransformed_bouts)[[1]], dim(untransformed_bouts)[[2]]))
    untransformed_bouts$method <- 'untransformed'
    
    bouts <- merge_undefined_bouts(untransformed_bouts)
    #cat(sprintf("a. %s\n", nrow(bouts)))
    bouts <- combine_identical_neighbors(bouts)
    #cat(sprintf("b. %s\n", nrow(bouts)))
    bouts <- create_nrem_rem_periods(bouts)
    #cat(sprintf("c. %s\n", nrow(bouts)))
    #cat(sprintf("4. dim: %s x %s\n", dim(bouts)[[1]], dim(bouts)[[2]]))
    
    ## Should not make bouts dissapear
    if(nrow(bouts) == 0 & nrow(untransformed_bouts > 0))
    {
      print(untransformed_bouts)
      stop("Bouts are empty even though untransformed_bouts are not!")
    }
      
    bouts$method <- 'classic'
    
    rbind(untransformed_bouts, bouts)    
  }, .parallel=TRUE)
  
  bouts
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
    plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + EPOCH_LENGTH, fill = bout_type), ymin = 0, ymax = 4.5, data = primary_bouts)    
    plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + EPOCH_LENGTH, fill = bout_type), ymin = 5.5, ymax = 10, data = secondary_bouts)    
  }
  plot <- plot + geom_point(aes(group=day_number), shape='.')
  
  plot
  
}


# Determine how many types of epochs each period has
tabulate_periods <- function(periods, df) {
  res <- ddply(periods, .(bout_type, start_labtime, end_labtime), function(period, df) {  
    if(nrow(period) > 1)
      period <- period[1,]
    
    period_epochs <- df[which(df$labtime >= period$start_labtime & df$labtime < period$end_labtime),]
    period_length <- nrow(period_epochs)
    type_counts <- table(period_epochs$epoch_type)
    correct_number <- type_counts[[as.character(period$bout_type)]]
    
    
    proportion_correct <- type_counts[[as.character(period$bout_type)]]/period_length
    
    
    data.frame(length=nrow(period_epochs), NREM=type_counts['NREM'], REM=type_counts['REM'], WAKE=type_counts['WAKE'], UNDEF=type_counts['UNDEF'], proportion_correct=proportion_correct)
    # length, # REM, NREM, WAKE, UNDEF, % correct
  }, df)
  
  res <- res[order(res$start_labtime),]
  res
}

# Using the tabulated periods, calculate percentage of epochs in each period that agree with the period type
calculate_agreement_stats <- function(res) {
  agreement_stats <- list(NREM=list(), REM=list(), all=list(), REM_NREM=list())
  
  d <- res[which(res$bout_type == 'NREM' & !is.nan(res$proportion_correct)),]
  agreement_stats$NREM$proportion <- mean(d$proportion_correct)
  agreement_stats$NREM$n <- nrow(d)
  
  d <- res[which(res$bout_type == 'REM' & !is.nan(res$proportion_correct)),]
  agreement_stats$REM$proportion <- mean(d$proportion_correct)
  agreement_stats$REM$n <- nrow(d)
  
  
  d <- res[which(!is.nan(res$proportion_correct)),]
  agreement_stats$all$proportion <- mean(d$proportion_correct)
  agreement_stats$all$n <- nrow(d)
  
  d <- res[which((res$bout_type == 'REM' | res$bout_type == 'NREM') & !is.nan(res$proportion_correct)),]
  agreement_stats$REM_NREM$proportion <- mean(d$proportion_correct)
  agreement_stats$REM_NREM$n <- nrow(d)
  
  agreement_stats
}

# Calculates stats for each subject
calculate_subject_statistics <- function(subject_periods) {
  llply(subject_periods, function(subject) {
    #df <- subject$subject_df
    #changepoint <- subject$periods$changepoint
    tab_classic <- tabulate_periods(subject$periods$classic, subject$subject_df)
    tab_changepoint <- tabulate_periods(subject$periods$changepoint, subject$subject_df)
    tab_untransformed <- tabulate_periods(subject$periods$untransformed, subject$subject_df)
    
    stats_classic <- calculate_agreement_stats(tab_classic)
    stats_changepoint <- calculate_agreement_stats(tab_changepoint)
    stats_untransformed <- calculate_agreement_stats(tab_untransformed)
    
    list(classic=list(tabulated_periods=tab_classic, agreement_stats=stats_classic), changepoint=list(tabulated_periods=tab_changepoint, agreement_stats=stats_changepoint), untransformed=list(tabulated_periods=tab_untransformed, agreement_stats=stats_untransformed))
  })
}

# Present stats in a data frame
present_subject_statistics <- function(stats) {
  
  l <- ldply(stats, function(subject_stats) {
    data.frame(
      n_classic=subject_stats$classic$agreement_stats$all$n, 
      proportion_classic=subject_stats$classic$agreement_stats$all$proportion,
      n_changepoint=subject_stats$changepoint$agreement_stats$all$n, 
      proportion_changepoint=subject_stats$changepoint$agreement_stats$all$proportion,
      
      n_classic_rem_nrem=subject_stats$classic$agreement_stats$REM_NREM$n, 
      proportion_classic_rem_nrem=subject_stats$classic$agreement_stats$REM_NREM$proportion,
      n_changepoint_rem_nrem=subject_stats$changepoint$agreement_stats$REM_NREM$n, 
      proportion_changepoint_rem_nrem=subject_stats$changepoint$agreement_stats$REM_NREM$proportion,
      
      n_classic_rem=subject_stats$classic$agreement_stats$REM$n, 
      proportion_classic_rem=subject_stats$classic$agreement_stats$REM$proportion,
      n_changepoint_rem=subject_stats$changepoint$agreement_stats$REM$n, 
      proportion_changepoint_rem=subject_stats$changepoint$agreement_stats$REM$proportion,
      
      n_classic_nrem=subject_stats$classic$agreement_stats$NREM$n, 
      proportion_classic_nrem=subject_stats$classic$agreement_stats$NREM$proportion,
      n_changepoint_nrem=subject_stats$changepoint$agreement_stats$NREM$n, 
      proportion_changepoint_nrem=subject_stats$changepoint$agreement_stats$NREM$proportion
    )
  } ) 
  
  l
}