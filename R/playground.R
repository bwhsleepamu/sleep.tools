source("R/main/libraries.R")
source("R/main/variables.R")
source("R/main/helper_methods.R")

source("R/data/load_data.R")
source("R/episodes/episodes.R")

source("R/cycles/cycles.R")
source("R/plotting/rasters/raster_plot.R")

source("R/analysis/analysis.R")

load_data(local=FALSE)
setup_episodes()


setup_cycles()
setup_raster_data(sleep_data, episodes, cycles)







###
plot_raster("3335GX", number_of_days=8, first_day=25)
plot_raster("3353GX", number_of_days=5, first_day=1)
plot_raster("3335GX", number_of_days=5, first_day=2)

plot_raster("23CEHM", number_of_days=10, first_day=3)
plot_raster("23CEHM", number_of_days=5, first_day=5)

#####

source("R/cycles/cycles.R")


load_data(TRUE)
setup_episodes()
setup_cycles()






qplot




# ahahhaa
#start_labtime < start_analysis | end_labtime > end_analysis
#start_labtime >= start_analysis & end_labtime <= end_analysis







































## Create trend lines across subjects (might need to be done on a graph by graph basis, or in bulk)
agg_nc <- to_plot_c[,list(slow_wave_sleep=mean(slow_wave_sleep)), by='sujbect_code,type,schedule_label,method,cycle_number,t_cycle,block_number,age_group']
agg_se <- to_plot_se[,list(slow_wave_sleep=mean(slow_wave_sleep)), by='block_number,age_group,t_cycle']
agg_se <- to_plot_be[,list(slow_wave_sleep=mean(slow_wave_sleep)), by='block_number,age_group,t_cycle']


####### Plotting the blocks


plot_survival.cycles <- function(data, type="NREM", method="changepoint", max_cycle=6, max_block=10, y_var="nrem_sleep", compare_by="se_label", facet_by="age_group", sleep_efficiency_labels=c("100%", "80%", "60%", "40%", "20%"), include=c(TRUE, FALSE)) {
  plot_data <- data[include %in% include & type == type & method == method & cycle_number <= max_cycle & block_number <= max_block & se_label %in% sleep_efficiency_labels]
  trend_lines <- plot_data[, list(y=mean(get(y_var))), by=c("block_number","cycle_number",compare_by,facet_by)]
  
  plot <- ggplot(plot_data, aes_string(x="block_number", y=y_var))
  plot <- plot + geom_point(aes_string(color=compare_by))

  plot <- plot + geom_line(aes_string(color=compare_by, y="y", x="block_number"), data=trend_lines)
  plot <- plot + facet_grid(paste(facet_by, " ~ cycle_number"), scales='free')
  
  plot
}
  
plot_survival.episodes <- function(dt, )



dt <- to_plot_se[include == TRUE & block_number < 30 & se_label %in% c("100%", "20%")]


plot <- ggplot(dt, aes_string(x="block_number", y="nrem_sleep"))
plot <- plot + geom_point(aes_string(color="se_label"))
plot <- plot + facet_grid(paste("age_group", " ~ cycle_number"), scales='free')
plot <- plot + geom_smooth(aes_string(group="se_label", color="se_label"), method=loess)
plot

qplot(block_number, total_sleep, data=to_plot_nc, facet=.~cycle_number)
qplot(block_number, total_sleep, data=to_plot_nc, facets=cycle_number~.)
qplot(block_number, total_sleep, data=to_plot_nc[cycle_number < 5], facets=cycle_number~., color=t_cycle)

plot <- ggplot(to_plot_nc[cycle_number < 6 & method=='changepoint' & block_number < 10], aes(x=block_number, y=nrem_sleep))
plot <- plot + geom_point(aes(color=age_group))
plot <- plot + geom_line(aes(color=age_group, y=slow_wave_sleep), data=agg_nc[cycle_number < 6 & method=='changepoint' & block_number < 10])
plot <- plot + facet_grid(t_cycle ~ cycle_number, scales='free')
plot <- plot + geom_smooth(aes(group=age_group, color=age_group), method=loess)
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