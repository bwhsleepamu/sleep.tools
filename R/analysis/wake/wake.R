e <- copy(episodes[method == "changepoint"])
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
  
  sheet[,before_label:=e[before]$label]
  sheet[,after_label:=e[after]$label]
  
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

