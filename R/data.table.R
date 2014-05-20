## Read Subject Info
read.subject_info <- function(file_path) {
  subjects <- fread(file_path)
  subjects[,file_exists:=file.exists(file_path)]
  setkey(subjects, subject_code)

  subjects
}

## Load Sleep Stage Data
load_sleep_data.dt <- function(subjects) {
  #sleep_data <- subjects[, load_sleep_file.dt(file_path), by=subject_code]
  sleep_data <- rbindlist(lapply(subjects$file_path, load_sleep_file.dt))
  #sleep_data[,V1:=NULL]
  setnames(sleep_data, c('subject_code', 'sleep_wake_period', 'labtime', 'stage'))
  setkey(sleep_data, subject_code, labtime)
  sleep_data
}

load_sleep_file.dt <- function(file_path) {  
  if(file.exists(file_path)) {
    fread(file_path)
  }
}

# Strip all periods until first NREM
strip_until_sleep_onset <- function(dt) {
  #print(typeof(dt))
  min_labtime <- dt[bout_type=="NREM", min(start_labtime)]
    
  nrow(dt) - nrow(dt[start_labtime >= min_labtime])
      
}


## BOXPLOTS



## Every combo of subject_code and method needs to have: NREM, REM, WAKE, UNDEF
generate_missing_combinations <- function(bout_type, by) {
  if(TRUE %in% !(levels(bout_type) %in% unique(bout_type))) {
    # cat(sprintf("%s | %s\n", by[[1]], levels(bout_type)[!(levels(bout_type) %in% unique(bout_type))]))
    data.table(bout_type=levels(bout_type)[!(levels(bout_type) %in% unique(bout_type))],sleep_wake_period=c(0), start_labtime=c(-1), end_labtime=c(-1), length=c(-1))
#     lapply(, function(level) {
#       (bout_type=level, )
#     })
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