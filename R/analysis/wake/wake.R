## Second pass
# 1. find places within each activ.bedrest.episode where sequence of labels is of a given pattern
# 2. get length of pattern, and length of wake. For variation of patern w/o wake, wake == 0
# 3. scatterplot wake length vs. pattern length for different patterns

# so, in each episode we search for label patterns
# we return a row for each pattern containing wake length and total length
# 

wds <- episodes[method=='iterative', find_patterns(label,length,"NWNR", 4, "NR", 2, 2, FALSE), by='subject_code,activity_or_bedrest_episode']
wds <- episodes[method=='iterative', find_patterns(label,length,"NRWNR", 5, "NRNR", 4, 3, FALSE), by='subject_code,activity_or_bedrest_episode']
wds <- episodes[method=='iterative', find_patterns(label,length,"NWR", 3, "NR", 2, 2, FALSE), by='subject_code,activity_or_bedrest_episode']
wds <- episodes[method=='iterative', find_patterns(label,length,"NWN", 3, "N", 1, 2, TRUE), by='subject_code,activity_or_bedrest_episode']
wds <- episodes[method=='iterative', find_patterns(label,length,"NWN", 3, "N", 1, 2, FALSE), by='subject_code,activity_or_bedrest_episode']
wds <- episodes[method=='iterative', find_patterns(label,length,"NWN", 3, "N", 1, 2, FALSE), by='subject_code,activity_or_bedrest_episode']
wds <- episodes[method=='iterative', find_patterns(label,length,"RWR", 3, "R", 1, 2, FALSE), by='subject_code,activity_or_bedrest_episode']


qplot(wake_length, pattern_length, data=wds, log="xy") + stat_smooth()

o <- episodes[method=='changepoint_compact' & subject_code=='1105X' & activity_or_bedrest_episode==5]

as.integer(gregexpr("NR", ss)[[1]])
gsub("W","","NRWNWNWNWRN")

paste(substring(o$label, 1, 1), collapse='')

find_patterns <- function(labels, lengths, pattern, plen, no_wake_pattern, nw_plen, wake_pos, set_zeros=TRUE) {
  wake_lengths <- integer(0)
  pattern_lengths <- integer(0)
  
  simple_labels <- paste(substring(labels, 1, 1), collapse='')
  start_positions <- as.integer(gregexpr(pattern, simple_labels)[[1]])
  if(start_positions[1] != -1) {
    all_positions <- sapply(start_positions, function(x, l) {seq.int(x, x+l) }, plen-1, simplify=TRUE)
    
    pattern_lengths <- apply(all_positions,2,function(x, lengths){sum(lengths[x])},lengths)
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
# FIrst pass



e <- analysis_episodes[typ=='all' & protocol_section!='fd' & method=='changepoint_compact']

wake_i <- which(e$label=="WAKE")
ws <- wake_sheet(wake_i, e, nrow(e))

bc <- ws[!is.na(before_length)]
ac <- ws[!is.na(after_length)]
tc <- ws[!is.na(total_length)]

cor(bc$length,bc$before_length)
p <- ggplot(bc, aes(bc$length, bc$before_length))
p + geom_point(alpha=1/10)

cor(tc$length,tc$total_length)
p <- ggplot(tc, aes(tc$length, tc$total_length))
p + geom_point(alpha=1/10)

cor(ac$length,ac$after_length)
p <- ggplot(ac, aes(ac$length, ac$after_length))
p + geom_point(alpha=1/10)

bc[,`:=`(t='bc',l=before_length)]
tc[,`:=`(t='tc',l=total_length)]
ac[,`:=`(t='ac',l=after_length)]
allc <- rbindlist(list(bc,ac,tc))

medians <- allc[,data.table(med=median(l)),by='t']
p <- ggplot(allc, aes(factor(t), l)) 
p + geom_boxplot(aes(fill = factor(t))) + geom_text(data=medians, aes(x=factor(t), y=med, label = med)) + scale_y_log10()


table(ws$before_label)
table(ws$after_label)

# Compare lengths to NREM with no WAKE
# Get dist of lengths based on wake

we <- copy(episodes[method == "changepoint"])
f <- copy(episodes[method == "iterative"])

qplot(f[label=="WAKE"]$length, binwidth=.5)

e[label=="WAKE"]

hist(e[label=="WAKE" & length > 10 & length < 200]$length)



short_wake_i <- which(e$label == "WAKE" & e$length <= 10)
medium_wake_i <- which(e$label == "WAKE" & e$length > 10 & e$length <= 30)
long_wake_i <- which(e$label == "WAKE" & e$length > 30 & e$length <= 60)
very_long_wake_i <- which(e$label == "WAKE" & e$length > 60)

wake_sheet <- function(i, e, n) {
  sheet <- data.table(wake=i)
  sheet[,before:=wake - 1]
  sheet[,after:=wake+1]
  
  sheet[before < 1, before:=NA]
  sheet[after > n, after:=NA]
  
  sheet[e[after]$activity_or_bedrest_episode != e[wake]$activity_or_bedrest_episode, after:=NA]  
  sheet[e[before]$activity_or_bedrest_episode != e[wake]$activity_or_bedrest_episode, before:=NA]
  
  sheet[e[after]$subject_code != e[wake]$subject_code, after:=NA]  
  sheet[e[before]$subject_code != e[wake]$subject_code, before:=NA]
  
  sheet[,before_label:=e[before]$label]
  sheet[,after_label:=e[after]$label]
  
  sheet[,length:=e[wake]$length]
  sheet[,before_length:=e[before]$length]
  sheet[,after_length:=e[after]$length]

  sheet[!is.na(after_label) & !is.na(before_label), total_length:=before_length + after_length]
  sheet
}

short <- wake_sheet(short_wake_i, e, nrow(e))
medium <- wake_sheet(medium_wake_i, e, nrow(e))
long <- wake_sheet(long_wake_i, e, nrow(e))
very_long <- wake_sheet(very_long_wake_i, e, nrow(e))

short[,wake_length:="1_short"]
medium[,wake_length:="2_medium"]
long[,wake_length:="3_long"]
very_long[,wake_length:="4_very_long"]

all <- rbind(short,medium,long,very_long)

View(very_long)

summary(short$total_length)
summary(medium$total_length)
summary(long$total_length)
summary(very_long$total_length)

restricted <- all[!is.na(total_length)]
describeBy(restricted$total_length, group=list(restricted$wake_length, restricted$after_label), mat=TRUE)

table(restricted$before_label, restricted$wake_length)
table(restricted$after_label, restricted$wake_length)

