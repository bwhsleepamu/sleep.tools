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
  
  
  clean.periods[,period_stats(label),by='subject_code,sleep_wake_period,method']
  
  clean.periods[,table(label,method), by='subject_code,sleep_wake_period']
  
  summary(table(clean.periods$method,clean.periods$label,clean.periods$subject_code,clean.periods$sleep_wake_period))
  
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