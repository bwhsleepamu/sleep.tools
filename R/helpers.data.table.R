## Read Subject Info
read.subject_info <- function(file_path) {
  subjects <- fread(file_path)
  subjects[,file_exists:=file.exists(file_path)]
  setkey(subjects, subject_code)

  subjects
}

## Load Sleep Stage Data
load_sleep_data.dt <- function(subjects) {
  sleep_data <- subjects[, load_sleep_file.dt(file_path), by=subject_code]
  sleep_data[,V1:=NULL]
  setnames(sleep_data, c('subject_code', 'sleep_wake_period', 'labtime', 'stage'))
  setkey(sleep_data, subject_code, labtime)
}

load_sleep_file.dt <- function(file_path) {  
  if(file.exists(file_path)) {
    fread(file_path)
  }
}

## Set up data for transformation
set_min_day_num <- function(subjects, sleep_data) {
  r <- sleep_data[,(min(floor(labtime / T_CYCLE)) - 1),by=subject_code]
  setnames(r, c('subject_code', 'min_day_number'))

  merge(subjects, r, all.x=TRUE)           
}

set_up_days <- function(labtime, min_day_number, t_cycle) {
  day_number <- floor(labtime / t_cycle)
  day_labtime <- labtime - (day_number * t_cycle)
  day_number <- day_number - min_day_number
  
  list(day_number, day_labtime)
}


set_up_data <- function(subjects, subject_data) {
  subjects
}


# min_day_number <- (min(floor(df$labtime / T_CYCLE)) - 1)
# # DAY NUMBER CALCULATION
# df$day_number <- floor(df$labtime / t_cycle)
# df$day_labtime <- (df$labtime - (df$day_number * t_cycle))
# min_day_number <- (min(df$day_number) - 1)
# df$day_number <- df$day_number - min_day_number
# 
# df$period_type <- factor(df$sleep_wake_period < 0, labels=c("sp", "wp")) 
# 
# # Label NREM, REM, WAKE, UNDEF
# df$epoch_type <- factor(sapply(df$stage, map_epoch_type))
# list(df=df, min_day_number=min_day_number)


### ACTUAL HELPERS!!
# Maps numerical values to types of epochs
map_epoch_type <- function(x) {
  ## Possibly speed up if x is a factor??
  if (x >= 1 & x <=4) { res <- "NREM" }
  else if (x == 5) { res <- "WAKE" }
  else if (x == 6) { res <- "REM" }
  else { res <- "UNDEF" }
  
  res
}


# Uses the sleep data to determine the most frequent type of epoch in each chunk
# Creates bouts with start and end labtimes
calculate_bouts <- function(chunks, epoch_type, labtime) {
  # get most frequent type
  data.frame(bout_type=names(sort(-table(epoch_type[chunks$start_index:chunks$end_index])))[1], start_labtime=labtime[chunks$start_index], end_labtime=labtime[chunks$end_index])
}
