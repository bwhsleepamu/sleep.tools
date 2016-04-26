


# Get the period start times
# for each start time, group all labtimes within 15 minutes

# For each sleep episode/nrem episode/rem episode
# 
# 15 minute blocks
# 
# 1. Since sleep onset
# 
# 2. Since sleep opportunity
# 
# 3. Since each NREM episode
# 
# 4. Since each REM episode
# 
# % of NREM (2,3,4)
# % of REM 
# % of WAKE
# 
# mean, SD across all 

# What's my final result?
#   A list of (sleep/nrem/rem) episodes, with the % of rem, nrem (2,3,4), wake epochs in the 30 epochs following
#   We need a list of blocks, 

# Collapse by 30 epochs, with a certain starting point, right?

# we have start and end points
# first step, divide them into 30s

blocks <- function(start, end, stages, block_length=30, in_minutes=FALSE, epoch_length=.5) {
  print(cat("Running from ", start, " to ", end))      
  dt <- data.table(stages=stages)      
  starts <- seq(start, end, by=30)
  ids <- 1:length(starts)
  lengths <- rep(block_length, length(starts))
  blocks <- rep.int(ids, lengths)[1:nrow(dt)]
  dt[,block_number:=blocks]
  dt[,block_stats(stages, in_minutes=in_minutes, epoch_length=epoch_length), by=block_number]
}

block_stats <- function(stages, epoch_length=.5, in_minutes=FALSE) {
  len <- length(stages)
  counts <- count(stages)
  
  if(in_minutes) {
    block_length <- len*epoch_length
    denom <- 1/epoch_length  
  }    
  else {
    denom <- len
    block_length <- len
  }
    
  
  nrem_freq <- sum(counts$freq[which(counts$x %in% c(2,3,4))])/denom
  slow_wave_sleep_freq <- sum(counts$freq[which(counts$x %in% c(3,4))])/denom
  total_sleep_freq <- sum(counts$freq[which(counts$x %in% c(1,2,3,4,6))])/denom
  rem_freq <- sum(counts$freq[which(counts$x == 6)])/denom
  wake_freq <- sum(counts$freq[which(counts$x == 5)])/denom
  missing_freq <- sum(counts$freq[which(!(counts$x %in% c(1,2,3,4,5,6)))])/denom
  
  data.table(total_sleep=total_sleep_freq, nrem_sleep=nrem_freq, slow_wave_sleep=slow_wave_sleep_freq, rem_sleep=rem_freq, wake=wake_freq, missing=missing_freq, block_length=block_length)
}

collapse_blocks <- function(dt) {
  data.table(
    total_sleep=mean(dt$total_sleep),
    nrem_sleep=mean(dt$nrem),
    slow_wave_sleep=mean(dt$slow_wave_sleep),
    rem_sleep=mean(dt$rem),
    wake=mean(dt$wake)
  )
}


## Environment Setup
## Calculate Block Stats (SO SLOW!)
by_sleep_episode <- sleep_episodes[,blocks(start_position, end_position, sleep_data$stage[start_position:end_position], block_length), by='subject_code,activity_or_bedrest_episode,schedule_label,se_label']
by_cycle <- cycles[,blocks(start_position, end_position, sleep_data$stage[start_position:end_position], block_length), by='subject_code,activity_or_bedrest_episode,type,schedule_label,se_label,method,cycle_number']
by_bedrest_episode <- bedrest_episodes[,blocks(start_position, end_position, sleep_data$stage[start_position:end_position], block_length), by='subject_code,activity_or_bedrest_episode,schedule_label,se_label']

collapsed_sleep_episode <- by_sleep_episode[,collapse_blocks(.SD),by='subject_code,schedule_label,se_label,block_number']
collapsed_cycle <- by_cycle[,collapse_blocks(.SD),by='subject_code,type,schedule_label,se_label,method,cycle_number,block_number']
collapsed_bedrest_episode <- by_bedrest_episode[,collapse_blocks(.SD),by='subject_code,schedule_label,se_label,block_number']

## JOIN SUBJECT DATA!
setkey(collapsed_sleep_episode, subject_code)
setkey(collapsed_cycle, subject_code)
setkey(collapsed_bedrest_episode, subject_code)
setkey(subjects, subject_code)

