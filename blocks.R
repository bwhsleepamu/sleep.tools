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

sleep_data[c(21,50)]

# What's my final result?
#   A list of (sleep/nrem/rem) episodes, with the % of rem, nrem (2,3,4), wake epochs in the 30 epochs following
#   We need a list of blocks, 

# Collapse by 30 epochs, with a certain starting point, right?

# we have start and end points
# first step, divide them into 30s


seq(1,100, by=30)

rep.int(x,y)

View(sleep_periods[, rep.int(seq(start_position, end_position, by=30), rep(30, length(seq(start_position, end_position, by=30)))), by=i])


