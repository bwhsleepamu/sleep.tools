source('R/sleep.tools.R')
require(xlsx)

## Variables
mnl <- 30
mrl <- 10
mwl <- 10
cpmType <- "Mann-Whitney"
ARL0 <- 10000
startup <- 20
block_length <- 30
epoch_length <- .5


## Setup
subjects.local <- read.subject_info("data/local_subject_list.csv")
subjects.all <- read.subject_info("data/full_subject_list.csv")
subjects.all <- subjects.all[!(subject_code %in% c("??", ""))]
subjects.subset <- subjects.all[study %in% c('NIAPPG', 'T20CSR-Control', 'T20CSR-CSR')]

#subjects <- subjects.local
subjects <- subjects.subset
subjects <- subjects.all


# Load and set up data
sleep_data <- load_sleep_data.dt(subjects)

# Use different methods for period calculation

######## Classic
episodes.classic <- generate.episodes.classic.dt(sleep_data, wake=FALSE, min_nrem_length=mnl, min_rem_length=mrl, min_wake_length=mwl)

######## Iterative
episodes.iterative <- generate.episodes.iterative.dt(sleep_data, wake=TRUE, undef=FALSE, min_nrem_length=mnl, min_rem_length=mrl, min_wake_length=mwl)

######## Changepoint
episodes.changepoint <- generate.episodes.changepoint.dt(sleep_data, wake=TRUE, undef=FALSE, cpmType=cpmType, ARL0=ARL0, startup=startup)


# Merge methods into one table
episodes <- rbindlist(list(episodes.changepoint,episodes.classic,episodes.iterative))
episodes <- episodes[activity_or_bedrest_episode > 0]
#episodes <- episodes.iterative

# Get NREM Cycles
nrem_cycles <- find.cycles(episodes, sleep_data, type="NREM", start_fn=find_nrem_start, until_end=TRUE) 
rem_cycles <- find.cycles(episodes, sleep_data, type="REM", start_fn=find_nrem_start, until_end=TRUE) 

nrem_cycles[,`:=`(start_labtime=sleep_data[start_position]$labtime, end_labtime=sleep_data[end_position]$labtime)]
rem_cycles[,`:=`(start_labtime=sleep_data[start_position]$labtime, end_labtime=sleep_data[end_position]$labtime)]

# Get Sleep Episodes
bedrest_episodes <- sleep_data[activity_or_bedrest_episode>0,list(start_position=min(.I), end_position=max(.I), start_labtime=min(labtime), end_labtime=max(labtime)),by='subject_code,activity_or_bedrest_episode']
sleep_episodes <- copy(bedrest_episodes)
sleep_episodes[,sleep_onset:=get_first_stage(sleep_data$stage[start_position:end_position], start_position, 2, not_found=start_position), by='subject_code,activity_or_bedrest_episode']
sleep_episodes[,`:=`(start_position=sleep_onset, sleep_onset=NULL)]
#sleep_episodes <- sleep_episodes[!is.na(start_position)]

# Join subject data 
fd_times <- subjects[, list(subject_code, start_analysis, end_analysis)]
fd_times <- fd_times[!is.null(subject_code) & !is.na(start_analysis) & !is.na(end_analysis)]

setkey(fd_times, subject_code)
setkey(nrem_cycles, subject_code)
setkey(rem_cycles, subject_code)
setkey(bedrest_episodes, subject_code)
setkey(sleep_episodes, subject_code)

nrem_cycles <- nrem_cycles[fd_times]
rem_cycles <- rem_cycles[fd_times]
bedrest_episodes <- bedrest_episodes[fd_times]
sleep_episodes <- sleep_episodes[fd_times]

# ahahhaa
start_labtime < start_analysis | end_labtime > end_analysis
start_labtime >= start_analysis & end_labtime <= end_analysis



## TODO: put into main function!!
## Fix for 0 length cycles
nrem_cycles <- nrem_cycles[length > 0]
rem_cycles <- rem_cycles[length > 0]

# Limit to analysis times
sleep_episodes_a <- sleep_episodes[start_labtime >= start_analysis & end_labtime <= end_analysis]
nrem_cycles_a <- nrem_cycles[start_labtime >= start_analysis & end_labtime <= end_analysis]
rem_cycles_a <- rem_cycles[start_labtime >= start_analysis & end_labtime <= end_analysis]
bedrest_episodes_a <- bedrest_episodes[start_labtime >= start_analysis & end_labtime <= end_analysis]

# Calculate Block Stats


by_sleep_episode <- sleep_episodes_a[,blocks(start_position, end_position, sleep_data$stage[start_position:end_position], block_length), by='subject_code,activity_or_bedrest_episode']
by_nrem_cycle <- nrem_cycles_a[,blocks(start_position, end_position, sleep_data$stage[start_position:end_position], block_length), by='subject_code,activity_or_bedrest_episode,method,cycle_number']
by_rem_cycle <- rem_cycles_a[,blocks(start_position, end_position, sleep_data$stage[start_position:end_position], block_length), by='subject_code,activity_or_bedrest_episode,method,cycle_number']
by_bedrest_episode <- bedrest_episodes_a[,blocks(start_position, end_position, sleep_data$stage[start_position:end_position], block_length), by='subject_code,activity_or_bedrest_episode']

by_sleep_episode_recov <- sleep_episodes[start_labtime < start_analysis | end_labtime > end_analysis,blocks(start_position, end_position, sleep_data$stage[start_position:end_position], block_length), by='subject_code,activity_or_bedrest_episode']
by_nrem_cycle_recov <- nrem_cycles[start_labtime < start_analysis | end_labtime > end_analysis,blocks(start_position, end_position, sleep_data$stage[start_position:end_position], block_length), by='subject_code,activity_or_bedrest_episode,method,cycle_number']
by_rem_cycle_recov <- rem_cycles[start_labtime < start_analysis | end_labtime > end_analysis,blocks(start_position, end_position, sleep_data$stage[start_position:end_position], block_length), by='subject_code,activity_or_bedrest_episode,method,cycle_number']
by_bedrest_episode_recov <- bedrest_episodes[start_labtime < start_analysis | end_labtime > end_analysis,blocks(start_position, end_position, sleep_data$stage[start_position:end_position], block_length), by='subject_code,activity_or_bedrest_episode']



beth_sleep_episode <- sleep_episodes_fd[start_labtime >= start_analysis & end_labtime <= end_analysis,blocks(start_position, end_position, sleep_data$stage[start_position:end_position], block_length=block_length, in_minutes=TRUE, epoch_length=epoch_length), by='subject_code,activity_or_bedrest_episode']
beth_nrem_cycle <- nrem_cycles_fd[start_labtime >= start_analysis & end_labtime <= end_analysis,blocks(start_position, end_position, sleep_data$stage[start_position:end_position], block_length=block_length, in_minutes=TRUE, epoch_length=epoch_length), by='subject_code,activity_or_bedrest_episode,method,cycle_number']
beth_rem_cycle <- rem_cycles_fd[start_labtime >= start_analysis & end_labtime <= end_analysis,blocks(start_position, end_position, sleep_data$stage[start_position:end_position], block_length=block_length, in_minutes=TRUE, epoch_length=epoch_length), by='subject_code,activity_or_bedrest_episode,method,cycle_number']
beth_bedrest_episode <- bedrest_episodes_fd[start_labtime >= start_analysis & end_labtime <= end_analysis,blocks(start_position, end_position, sleep_data$stage[start_position:end_position], block_length=block_length, in_minutes=TRUE, epoch_length=epoch_length), by='subject_code,activity_or_bedrest_episode']


collapsed_sleep_episode <- by_sleep_episode[,collapse_blocks(.SD),by='subject_code,block_number']
collapsed_nrem_cycle <- by_nrem_cycle[,collapse_blocks(.SD),by='subject_code,method,cycle_number,block_number']
collapsed_rem_cycle <- by_rem_cycle[,collapse_blocks(.SD),by='subject_code,method,cycle_number,block_number']
collapsed_bedrest_episode <- by_bedrest_episode[,collapse_blocks(.SD),by='subject_code,block_number']

collapsed_sleep_episode_recov <- by_sleep_episode_recov[,collapse_blocks(.SD),by='subject_code,block_number']
collapsed_nrem_cycle_recov <- by_nrem_cycle_recov[,collapse_blocks(.SD),by='subject_code,method,cycle_number,block_number']
collapsed_rem_cycle_recov <- by_rem_cycle_recov[,collapse_blocks(.SD),by='subject_code,method,cycle_number,block_number']
collapsed_bedrest_episode_recov <- by_bedrest_episode_recov[,collapse_blocks(.SD),by='subject_code,block_number']




## JOIN SUBJECT DATA!
setkey(collapsed_sleep_episode, subject_code)
setkey(collapsed_nrem_cycle, subject_code)
setkey(collapsed_rem_cycle, subject_code)
setkey(collapsed_bedrest_episode, subject_code)
setkey(collapsed_sleep_episode_recov, subject_code)
setkey(collapsed_nrem_cycle_recov, subject_code)
setkey(collapsed_rem_cycle_recov, subject_code)
setkey(collapsed_bedrest_episode_recov, subject_code)
setkey(subjects, subject_code)


to_plot_se <- merge(collapsed_sleep_episode, subjects, all.x=TRUE, all.y=FALSE)
to_plot_nc <- merge(collapsed_nrem_cycle, subjects, all.x=TRUE, all.y=FALSE)
to_plot_rc <- merge(collapsed_rem_cycle, subjects, all.x=TRUE, all.y=FALSE)
to_plot_be <- merge(collapsed_bedrest_episode, subjects, all.x=TRUE, all.y=FALSE)

to_plot_se_r <- merge(collapsed_sleep_episode_recov, subjects, all.x=TRUE, all.y=FALSE)
to_plot_nc_r <- merge(collapsed_nrem_cycle_recov, subjects, all.x=TRUE, all.y=FALSE)
to_plot_rc_r <- merge(collapsed_rem_cycle_recov, subjects, all.x=TRUE, all.y=FALSE)
to_plot_be_r <- merge(collapsed_bedrest_episode_recov, subjects, all.x=TRUE, all.y=FALSE)


agg_nc <- to_plot_nc[,list(slow_wave_sleep=mean(slow_wave_sleep)), by='method,cycle_number,t_cycle,block_number,age_group']
agg_se <- to_plot_se[,list(slow_wave_sleep=mean(slow_wave_sleep)), by='block_number,age_group,t_cycle']


####### Plotting the blocks

qplot(block_number, total_sleep, data=to_plot_nc, facet=.~cycle_number)
qplot(block_number, total_sleep, data=to_plot_nc, facets=cycle_number~.)
qplot(block_number, total_sleep, data=to_plot_nc[cycle_number < 5], facets=cycle_number~., color=t_cycle)

plot <- ggplot(m[cycle_number < 6 & method=='changepoint' & block_number < 10 & include == TRUE & t_cycle %in% c(20, 28)], aes(x=block_number, y=nrem_sleep))
plot <- plot + geom_point(aes(color=type))
#plot <- plot + geom_line(aes(color=age_group, y=slow_wave_sleep), data=agg_nc[cycle_number < 6 & method=='changepoint' & block_number < 10])
plot <- plot + facet_grid(. ~ cycle_number, scales='free')
plot <- plot + geom_smooth(aes(group=type, color=type), method=loess)
plot



plot <- ggplot(to_plot_se, aes(x=block_number, y=slow_wave_sleep))
plot <- plot + geom_point(aes(color=age_group))
plot <- plot + geom_line(aes(color=age_group, y=slow_wave_sleep), data=agg_se)
plot <- plot + facet_grid(t_cycle ~ ., scales="free_y")
plot <- plot + geom_smooth(aes(group=age_group, color=age_group), method=loess)
plot










# What we're working with: periods, sleep_data, and subject_list
r <- setup.raster(sleep_data, periods, nrem_cycles)
p <- plot.raster(r$sleep_data, r$periods, r$nrem_cycles, r$sleep_periods, subject_code='1105X', number_of_days=2, first_day=45, epoch_length=EPOCH_LENGTH)
p <- plot.raster(r$sleep_data, r$periods, r$nrem_cycles, r$sleep_periods, subject_code='1106X', number_of_days=10, first_day=37, epoch_length=EPOCH_LENGTH)


# CSR vs. NOT
p <- plot.raster(r$sleep_data, r$periods, r$nrem_cycles, r$sleep_periods, subject_code='3319GX', epoch_length=EPOCH_LENGTH, l='full')
p <- plot.raster(r$sleep_data, r$periods, r$nrem_cycles, r$sleep_periods, subject_code='3319GX', number_of_days=10, first_day=3, epoch_length=EPOCH_LENGTH, l='short')
p <- plot.raster(r$sleep_data, r$periods, r$nrem_cycles, r$sleep_periods, subject_code='3315GX32', number_of_days=10, first_day=3, epoch_length=EPOCH_LENGTH)

# Young vs. Old
p <- plot.raster(r$sleep_data, r$periods, r$nrem_cycles, r$sleep_periods, subject_code='1257V', number_of_days=10, first_day=3, epoch_length=EPOCH_LENGTH)
p <- plot.raster(r$sleep_data, r$periods, r$nrem_cycles, r$sleep_periods, subject_code='1215H', number_of_days=10, first_day=3, epoch_length=EPOCH_LENGTH)


p



svg('/home/pwm4/Desktop/test.svg')
print(p)
dev.off()


subjects







## FAST UP TO HERE!!

### NOW, WE DIVERGE INTO DIFFERENT BOUT DTs
# Parallel plyr functions

# changepoint.dt <- sleep_data[, bouts.changepoint(.SD), by=subject_code]
# classic.dt <- sleep_data[, bouts.classic(.SD, subject_code=.BY), by=subject_code]
# 
# periods.dt <- rbind(changepoint.dt, classic.dt, use.names=TRUE)

setkey(periods, subject_code, method, start_labtime)

# Get rid of wake periods
clean.periods.dt <- periods.dt[activity_or_bedrest_episode > 0]

# Get rid of everything until sleep onset
clean.periods.dt <- clean.periods.dt[, onset:=start_labtime[match('NREM', bout_type)], by='subject_code,method,activity_or_bedrest_episode']
clean.periods.dt <- clean.periods.dt[start_labtime >= onset]
clean.periods.dt <- clean.periods.dt[, onset:=NULL]

## PLOTTING
# Add dummy data
csr.periods <- clean.periods.dt[subjects[study=='T20CSR-CSR']$subject_code]
#csr.periods[, group:='csr']
control.periods <- clean.periods.dt[subjects[study=='T20CSR-Control']$subject_code]
#control.periods[, group:='control']

old.periods <- clean.periods.dt[subjects[study=='NIAPPG' & age_group=='O']$subject_code]
young.periods <- clean.periods.dt[subjects[study=='NIAPPG' & age_group=='Y']$subject_code]


csr.plot <- plt(csr.periods, "CSR")
control.plot <- plt(control.periods, "CONTROL")
young.plot <- plt(young.periods, "YOUNG")
old.plot <- plt(old.periods, "OLD")

grid.arrange(csr.plot, control.plot, young.plot, old.plot, ncol=1)



plt <- function(dt, title) {
  missing <- dt[, generate_missing_combinations(bout_type, .BY), by='subject_code,method']
  
  plot <- ggplot(rbind(dt, missing, use.names=TRUE), aes(factor(method), length))
  
  plot <- plot + ggtitle(title)
  
  #plot <- plot + facet_grid(group ~ subject_code, drop=FALSE)
  plot <- plot + facet_wrap(~ subject_code, ncol = 7, drop=FALSE)
  
  # Scale for outliers
  #ylimits <- clean.periods.dt[, data.table(lower=boxplot.stats(length)$stats[1], upper=boxplot.stats(length)$stats[5]), by='subject_code,method']
  ylimits <- clean.periods.dt[, data.table(lower=boxplot.stats(length)$stats[1], upper=boxplot.stats(length)$stats[5]), by='subject_code,method,bout_type']  
  
  ylim <- c(0, 480)
  plot <- plot + coord_cartesian(ylim = ylim)
  plot <- plot + scale_y_continuous(breaks=seq(0, ylim[2], 60))
  
  plot <- plot + geom_boxplot(aes(fill=factor(bout_type)))
  #plot <- plot + geom_boxplot()
  
  #plot <- plot + geom_jitter()
  #plot <- plot + geom_jitter(aes(colour=factor(bout_type)))

  plot
}


subject_periods <- calculate_periods_for_subjects(subjects)

# Get specific results for a given subject
plot.bouts(subject_periods[["3335GX"]]$subject_df, subject_periods[["3335GX"]]$periods$changepoint, subject_periods[["3335GX"]]$periods$classic)  
df <- subject_periods[["3335GX"]]$subject_df
changepoint_periods <-subject_periods[["3335GX"]]$periods$changepoint 
classic_periods <-subject_periods[["3335GX"]]$periods$classic


stats <- calculate_subject_statistics(subject_periods)
stats_df <- present_subject_statistics(stats)



###
# MORE WORK
joined <- merge(clean.periods.dt, subjects)
table(joined$bout_type, joined$age_group, joined$method)

    
period.agreement <- function(subject_code, label, start_time, end_time, sleep_data) {
  
  t <- table(sleep_data[subject_code][labtime %between% c(start_time, end_time)]$epoch_type)
  t[[label]]/sum(t)
}

period.agreement <- function(df) {
  dlply(df, .(subject_code, method, start_labtime), function(d){
    t <- table(sleep_data[d$subject_code][labtime %between% c(d$start_labtime, d$end_labtime)]$epoch_type)
    t[[d$bout_type]]/sum(t)    
  })
}  


      


## Classic vs. Changepoint

### Stats per period:

# smallest - nrem, rem, wake, undef
# largest - nrem, rem, wake, undef

# % correct in total
#   nrem
#   rem

DT1a <- as.data.table(stats[["3335GX"]]$classic$tabulated_periods)
DT1a[,`:=`(subject_code="3335GX", method="classic")]
DT1b <- as.data.table(stats[["3335GX"]]$changepoint$tabulated_periods)
DT1b[,`:=`(subject_code="3335GX", method="changepoint")]
DT1c <- as.data.table(stats[["3335GX"]]$untransformed$tabulated_periods)
DT1c[,`:=`(subject_code="3335GX", method="untransformed")]


DT2a <- as.data.table(stats[["23D8HS"]]$classic$tabulated_periods)
DT2a[,`:=`(subject_code="23D8HS", method="classic")]
DT2b <- as.data.table(stats[["23D8HS"]]$changepoint$tabulated_periods)
DT2b[,`:=`(subject_code="23D8HS", method="changepoint")]
DT2c <- as.data.table(stats[["23D8HS"]]$untransformed$tabulated_periods)
DT2c[,`:=`(subject_code="23D8HS", method="untransformed")]


DT3a <- as.data.table(stats[["2632DX"]]$classic$tabulated_periods)
DT3a[,`:=`(subject_code="2632DX", method="classic")]
DT3b <- as.data.table(stats[["2632DX"]]$changepoint$tabulated_periods)
DT3b[,`:=`(subject_code="2632DX", method="changepoint")]
DT3c <- as.data.table(stats[["2632DX"]]$untransformed$tabulated_periods)
DT3c[,`:=`(subject_code="2632DX", method="untransformed")]

DT4a <- as.data.table(stats[["28J8X"]]$classic$tabulated_periods)
DT4a[,`:=`(subject_code="28J8X", method="classic")]
DT4b <- as.data.table(stats[["28J8X"]]$changepoint$tabulated_periods)
DT4b[,`:=`(subject_code="28J8X", method="changepoint")]
DT4c <- as.data.table(stats[["28J8X"]]$untransformed$tabulated_periods)
DT4c[,`:=`(subject_code="28J8X", method="untransformed")]

DT <- rbindlist(list(DT1a, DT1b, DT1c, DT2a, DT2b, DT2c, DT3a, DT3b, DT3c, DT4a, DT4b, DT4c))
setkey(DT, method)
plot <- ggplot(DT, aes(x=length, fill=bout_type)) + geom_histogram(binwidth=1)
plot <- plot + scale_y_continuous(limits=c(0, max(table(DT[c("classic", "changepoint"),length]))))
plot <- plot + facet_grid(method ~ subject_code, scales="free") 
plot
#

# In general, We can bring the comparisons together.

# Determining cycles:
#  