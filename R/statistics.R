## For periods:
### NEED: 
#DONE #### Agreement stats by subject and or subject group FOR EACH METHOD 
#### Number of periods per sleep episode for each period type by each method
#### Distribution of period lengths for each type by subject and or subject group for each method
#### OTHER COMPARISONS??

## For cycles:
### NEED: distribution of lengths and counts for 1st, 2nd...cycles by subject and/or subject group FOR EACH METHOD
### Possible others: start times by cycle, 





## Agreement
function() {
  # Cleaning:
  ## No sleep periods
  clean.periods <- copy(periods)
  clean.periods <- clean.periods[sleep_wake_period>0]
  ## Strip wake at beginning and end of sleep periods
  clean.periods <- clean.periods[,strip_wake(.SD),by='subject_code,sleep_wake_period']
  clean.periods[,pik:=.I]
  
  clean.periods[,agreement:=sum(sleep_data[start_position:end_position]$epoch_type == label)/length,by='pik']
  
  ## Agreement graph
  p <- ggplot(clean.periods, aes(x=agreement, color=method))
  p <- p + facet_grid(label ~ ., scales='free_y')
  p + geom_density(alpha=.3)
  
}

function() {
  # period stats
  #period_counts <- clean.periods[,list(nrem_count=sum(label=='NREM'),rem_count=sum(label=='REM'),wake_count=sum(label=='WAKE')),by='subject_code,sleep_wake_period,method']
  period_counts <- copy(clean.periods)
  period_counts <- period_counts[,list(count=.N),by='subject_code,sleep_wake_period,method,label']
  
  ## Number of periods of each type per sleep episode
  p <- ggplot(period_counts, aes(x=count, fill=method, color=method))
  p <- p + facet_grid(label ~ ., scales='free')
  p <- p + scale_x_discrete()
  p + geom_freqpoly(binwidth=1)
  p + geom_histogram(binwidth=1)
  
  ## Length distribution of periods
  period_lengths <- copy(clean.periods)
  period_lengths[,length:=length*epoch_length*60]
  period_lengths[,period_number:=seq(.N),by='subject_code,sleep_wake_period,method,label']
  
  p <- ggplot(period_lengths, aes(factor(period_number), length, color=method))
  p <- p + facet_grid(. ~ label)
  p + geom_boxplot()

}

## Cycles
function() {
  nrem_cycles.s <- copy(nrem_cycles)
  nrem_cycles.s[,length:=length*epoch_length*60]
  
  p <- ggplot(nrem_cycles.s, aes(factor(cycle_number), length, color=method))
  p + geom_boxplot()

  cycle_counts <- nrem_cycles[,list(count=.N),by='subject_code,sleep_wake_period,method']
  
  p <- ggplot(cycle_counts, aes(x=count, fill=method, color=method))
  #p <- p + facet_grid(label ~ ., scales='free')
  #p <- p + scale_x_discrete()
  p + geom_freqpoly(binwidth=1)
  
}

#periods
function(){
  wd <- periods[subject_code=="3335GX" & sleep_wake_period==2]
  
  tst <- wd[method=='changepoint']
  
  # Cleaning:
  ## No sleep periods
  clean.periods <- periods[sleep_wake_period>0]
  ## Strip wake at beginning and end of sleep periods
  clean.periods <- clean.periods[,strip_wake(.SD),by='subject_code,sleep_wake_period']
  
  psts <- clean.periods[,list(count=length(length)),by='subject_code,sleep_wake_period,method,label']
  
  psts[, mean(NREM_count), by='method']
  
  
  
  plot <- ggplot(psts, aes(method, count))
  plot <- plot + facet_wrap(~ label)
  plot + geom_boxplot()
  
  
  plot <- ggplot(psts, aes(x=count))
  plot <- plot + facet_grid(method ~ label)
  plot <- plot + geom_histogram()
  
  plot
  
  #clean.periods[,table(label,method), by='subject_code,sleep_wake_period']
  
  #summary(table(clean.periods$method,clean.periods$label,clean.periods$subject_code,clean.periods$sleep_wake_period))
  
}

## Stats for number of periods
period_stats <- function(labels) {
  t <- table(labels)
  list(NREM_count=t[names(t)=='NREM'], REM_count=t[names(t)=='REM'],WAKE_count=t[names(t)=='WAKE'], total=length(labels))  
}


## Strips start and end wake off
strip_wake <- function(dt) {
  
  sleep_onset <- min(match(c("NREM", "REM"), dt$label, nomatch=1L))
  sleep_end <- (length(dt$label) - min(match(c("NREM", "REM"), rev(dt$label), nomatch=1L))) + 1L
  
  dt[sleep_onset:sleep_end]
}