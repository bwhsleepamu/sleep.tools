### ALL THIS IS SETUP, AN WORKS!!!

source('R/sleep.tools.R')

subjects.local <- read.subject_info("data/local_subject_list.csv")
subjects.all <- read.subject_info("data/full_subject_list.csv")
subjects.subset <- subjects.all[study %in% c('NIAPPG', 'T20CSR-Control', 'T20CSR-CSR')]

subjects <- subjects.subset

sleep_data <- load_sleep_data.dt(subjects)
# Generate row indeces
sleep_data[, pk:=.I]
# Map stages to epoch types
sleep_data[,epoch_type:=as.vector(lapply(stage, map_epoch_type)),]

# Generate Chunks!
chunks <- sleep_data[, chunk(epoch_type, pk), by='subject_code,sleep_wake_period']

######## Classic
chunks.classic <- chunks

# label all rows to keep
chunks.classic[,keep:=TRUE]
chunks.classic[,new_label:=relabel_to_biggest_neighbor(label,length,"UNDEF"),by='subject_code,sleep_wake_period']

# Now, to merge same bouts again, with length consideration...



chunks.classic[,merge_label(label, length, "UNDEF"),by='subject_code,sleep_wake_period']



chunks.tmp <- chunks.classic[subject_code=='1105X' & sleep_wake_period==1]

#m <- matrix(c(i-1,i+1), ncol=2, nrow=length(i))
#mapply(determine_merge_direction, i-1, i+1, MoreArgs=list(chunks.tmp$label, chunks.tmp$length))



chunks.tmp[,new_label:=testf(label,length,"UNDEF")]
chunks.tmp[,temp:=NULL]


i <- chunks.tmp[label=="UNDEF", which=TRUE]

r <- merge_label(chunks.tmp$label, chunks.tmp$length, "UNDEF")

f <- i + r

chunks.tmp$label[f]
chunks.tmp[i, new_label:=chunks.tmp$label[f]]


i
f
r
c <- c(f[which(r!=0)], c(i[which(r==0)]+1,i[which(r==0)]-1))

ddd <- data.table(dir=r,my_start=chunks.tmp$start_position[i], f_start=chunks.tmp$start_position[f], my_end=chunks.tmp$end_position[i], f_end=chunks.tmp$end_position[f], my_lab=chunks.tmp$label[i],f_lab=chunks.tmp$label[f])


chunks.tmp[i, new_length:=length]
chunks.tmp[i, ]
chunks.tmp[f]


mapply(function(i,f) {
  
})

v <- chunks.tmp$length
len <- apply(m, c(1,2), function(x){chunks.tmp$length[x]})
lab <- apply(m, c(1,2), function(x){chunks.tmp$label[x]})

len <- apply(m, c(1,2), function(x){v[x]})
lab <- apply(m, c(1,2), function(x){chunks.tmp$label[x]})

apply(m, c(1), function(x){
  
})

## OK SO DEALING WITH UNDEF FIRST
# To compare label to earlier:
# To compare label to later:
# Edge case: if i == first or last of list

## HERE WE CAN DO A LOT WITH CHUNKS

## HERE WE ALSO CAN DO CHANGEPOINT ANALYSIS

# Convert chunk positions to labtimes
chunks[,start_labtime:=sleep_data$labtime[start_position]]

## THIS IS FOR GRAPHING!!!
subjects <- set_min_day_num(subjects, sleep_data)
sleep_data[,c('day_number', 'day_labtime'):=set_up_days(labtime, subjects[subject_code]$min_day_number, T_CYCLE),by=subject_code]
#sleep_data[,epoch_type:=as.factor(as.character(epoch_type))]

## FAST UP TO HERE!!

### NOW, WE DIVERGE INTO DIFFERENT BOUT DTs
# Parallel plyr functions
registerDoMC(4)

changepoint.dt <- sleep_data[, bouts.changepoint(.SD), by=subject_code]
classic.dt <- sleep_data[, bouts.classic(.SD, subject_code=.BY), by=subject_code]

periods.dt <- rbind(changepoint.dt, classic.dt, use.names=TRUE)
setkey(periods.dt, subject_code, method, start_labtime)

# Get rid of wake periods
clean.periods.dt <- periods.dt[sleep_wake_period > 0]

# Get rid of everything until sleep onset
clean.periods.dt <- clean.periods.dt[, onset:=start_labtime[match('NREM', bout_type)], by='subject_code,method,sleep_wake_period']
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