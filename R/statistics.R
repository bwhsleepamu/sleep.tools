## Agreement
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