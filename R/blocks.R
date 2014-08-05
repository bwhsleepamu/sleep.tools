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

blocks <- function(start, end, stages, block_length=30) {
  dt <- data.table(stages=stages)
  starts <- seq(start, end, by=30)
  ids <- 1:length(starts)
  lengths <- rep(block_length, length(starts))
  blocks <- rep.int(ids, lengths)[1:nrow(dt)]
  dt[,block_number:=blocks]
  dt[,block_stats(stages), by=block_number]
}

block_stats <- function(stages) {
  len <- length(stages)
  counts <- count(stages)
  
  nrem_freq <- sum(counts$freq[which(counts$x %in% c(2,3,4))])/len
  slow_wave_sleep_freq <- sum(counts$freq[which(counts$x %in% c(3,4))])/len
  total_sleep_freq <- sum(counts$freq[which(counts$x %in% c(1,2,3,4,6))])/len
  rem_freq <- sum(counts$freq[which(counts$x == 6)])/len
  wake_freq <- sum(counts$freq[which(counts$x == 5)])/len
  missing_freq <- sum(counts$freq[which(!(counts$x %in% c(1,2,3,4,5,6)))])

  data.table(total_sleep=total_sleep_freq, nrem=nrem_freq, slow_wave_sleep=slow_wave_sleep_freq, rem=rem_freq, wake=wake_freq, missing=missing_freq, length=len)
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


