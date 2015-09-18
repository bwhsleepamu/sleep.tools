# Helper Methods
find_patterns <- function(labels, lengths, pattern, plen, no_wake_pattern, nw_plen, wake_pos, set_zeros=TRUE) {
  wake_lengths <- integer(0)
  pattern_lengths <- integer(0)
  
  simple_labels <- paste(substring(labels, 1, 1), collapse='')
  start_positions <- as.integer(gregexpr(pattern, simple_labels)[[1]])
  if(start_positions[1] != -1) {
    all_positions <- sapply(start_positions, function(x, l) {seq.int(x, x+l) }, plen-1, simplify=TRUE)
    
    pattern_lengths <- apply(all_positions,2,function(x, lengths,wake_pos){sum(lengths[x[-wake_pos]])},lengths,wake_pos)
    wake_lengths <- apply(all_positions,2,function(x, lengths, wake_pos){ lengths[x[wake_pos]] },lengths,wake_pos)
  }
  
  start_positions <- as.integer(gregexpr(no_wake_pattern, simple_labels)[[1]])
  if(start_positions[1]!=-1 & set_zeros==TRUE) {
    all_positions <- sapply(start_positions, function(x, l) {seq.int(x, x+l) }, nw_plen-1, simplify=TRUE)
    
    if(nw_plen == 1)
      pattern_lengths <- c(pattern_lengths, sapply(all_positions,function(x, lengths){sum(lengths[x])},lengths))
    else
      pattern_lengths <- c(pattern_lengths, apply(all_positions,2,function(x, lengths){sum(lengths[x])},lengths))
    wake_lengths <- c(wake_lengths, rep.int(0, length(start_positions)))    
  }
  
  data.table(wake_length=as.integer(wake_lengths),pattern_length=as.integer(pattern_lengths))
}


plot_wake_graph <- function(patterns, episodes) {
  
  wds <- rbindlist(lapply(patterns,function(x) episodes[method=='changepoint_compact',find_wake_in_sleep_patterns(label,length,wake_epochs,x,nchar(x))] ))
  qplot(wake_length,pattern_length, data=wds, color=pattern)
}


find_wake_in_sleep_patterns <- function(labels, lengths, wake_epochs, pattern, pattern_length) {
  simple_labels <- paste(substring(labels, 1, 1), collapse='')
  start_positions <- as.integer(gregexpr(pattern, simple_labels)[[1]])
  
  if(start_positions[1] != -1) {
    all_positions <- sapply(start_positions, function(x, l) {seq.int(x, x+l) }, pattern_length-1, simplify=TRUE)
    
    if(pattern_length == 1) {
      wake_lengths <- sapply(all_positions,function(x, wake){ sum(wake[x]) },wake_epochs)
      pattern_lengths <- sapply(all_positions,function(x, lengths){sum(lengths[x])},lengths) - wake_lengths
    }
    else {
      wake_lengths <- apply(all_positions,2,function(x, wake){ sum(wake[x]) },wake_epochs)
      pattern_lengths <- apply(all_positions,2,function(x, lengths){sum(lengths[x])},lengths) - wake_lengths
    }
  }
  data.table(wake_length=as.integer(wake_lengths),pattern_length=as.integer(pattern_lengths), pattern=pattern)
}



## Second pass
# 1. find places within each activ.bedrest.episode where sequence of labels is of a given pattern
# 2. get length of pattern, and length of wake. For variation of patern w/o wake, wake == 0
# 3. scatterplot wake length vs. pattern length for different patterns

# so, in each episode we search for label patterns
# we return a row for each pattern containing wake length and total length
# 

wds <- episodes[method=='iterative', find_patterns(label,length,"NWNR", 4, "NR", 2, 2, FALSE), by='subject_code,activity_or_bedrest_episode']
wds <- episodes[method=='iterative', find_patterns(label,length,"RWRN", 4, "RN", 2, 2, FALSE), by='subject_code,activity_or_bedrest_episode']
wds <- episodes[method=='iterative', find_patterns(label,length,"NRWNR", 5, "NRNR", 4, 3, FALSE), by='subject_code,activity_or_bedrest_episode']
wds <- episodes[method=='iterative', find_patterns(label,length,"RNWRN", 5, "NRNR", 4, 3, FALSE), by='subject_code,activity_or_bedrest_episode']
wds <- episodes[method=='iterative', find_patterns(label,length,"NWR", 3, "NR", 2, 2, FALSE), by='subject_code,activity_or_bedrest_episode']
wds <- episodes[method=='iterative', find_patterns(label,length,"NWN", 3, "N", 1, 2, TRUE), by='subject_code,activity_or_bedrest_episode']
wds <- episodes[method=='iterative', find_patterns(label,length,"NWN", 3, "N", 1, 2, FALSE), by='subject_code,activity_or_bedrest_episode']
wds <- episodes[method=='iterative', find_patterns(label,length,"RWR", 3, "R", 1, 2, FALSE), by='subject_code,activity_or_bedrest_episode']

wds <- episodes.raw[label!="UNDEF",find_patterns(label,length,"NWNR", 4, "NR", 2, 2, FALSE), by='subject_code,activity_or_bedrest_episode']
wds <- episodes[label!='UNDEF', find_patterns(label,length,"NRWNR", 5, "NRNR", 4, 3, FALSE), by='subject_code,activity_or_bedrest_episode']
wds <- episodes[method=='iterative', find_patterns(label,length,"NWR", 3, "NR", 2, 2, FALSE), by='subject_code,activity_or_bedrest_episode']
wds <- episodes[method=='iterative', find_patterns(label,length,"NWN", 3, "N", 1, 2, TRUE), by='subject_code,activity_or_bedrest_episode']
wds <- episodes[method=='iterative', find_patterns(label,length,"NWN", 3, "N", 1, 2, FALSE), by='subject_code,activity_or_bedrest_episode']
wds <- episodes.raw[label!='UNDEF', find_patterns(label,length,"NWN", 3, "N", 1, 2, FALSE), by='subject_code,activity_or_bedrest_episode']
wds <- episodes.raw[label!='UNDEF', find_patterns(label,length,"RWR", 3, "R", 1, 2, FALSE), by='subject_code,activity_or_bedrest_episode']

lmout <- lm(pattern_length~wake_length,wds)
cor(wds$pattern_length,wds$wake_length)
qplot(wake_length, pattern_length, data=wds, main=paste(list(lmout$coefficients)), sep="; ") + geom_abline(slope=lmout$coefficients[2], intercept=lmout$coefficients[1], color='red')
qplot(wake_length, pattern_length, data=wds, main=paste(list(lmout$coefficients)), sep="; ") + geom_abline(slope=lmout_a$coefficients[2], intercept=lmout_a$coefficients[1], color='red') + geom_abline(slope=lmout_b$coefficients[2], intercept=lmout_b$coefficients[1], color='blue') 

lmout_a <- lm(pattern_length~wake_length,wds[wake_length <= 50])
cor(wds[wake_length<=50]$pattern_length,wds[wake_length<=50]$wake_length)
qplot(wake_length, pattern_length, data=wds[wake_length <= 50], main=paste(list(lmout_a$coefficients)), sep="; ") + geom_abline(slope=lmout_a$coefficients[2], intercept=lmout_a$coefficients[1], color='red')


lmout_b <- lm(pattern_length~wake_length,wds[wake_length >= 50])
cor(wds[wake_length>=50]$pattern_length,wds[wake_length>=50]$wake_length)
qplot(wake_length, pattern_length, data=wds[wake_length >= 50], main=paste(list(lmout_b$coefficients)), sep="; ") + geom_abline(slope=lmout_b$coefficients[2], intercept=lmout_b$coefficients[1], color='red')


o <- episodes[method=='changepoint_compact' & subject_code=='1105X' & activity_or_bedrest_episode==5]

as.integer(gregexpr("NR", ss)[[1]])
gsub("W","","NRWNWNWNWRN")

paste(substring(o$label, 1, 1), collapse='')


# New angle
# 

episodes[method!="raw",wake_epochs:=sum(sleep_data[start_position:end_position]$epoch_type=="WAKE"),by='start_position,end_position']
cycles[method!="raw",wake_epochs:=sum(sleep_data[start_position:end_position]$epoch_type=="WAKE"),by='start_position,end_position']
