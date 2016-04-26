library(minpack.lm)

lcount <- function(labels) {
  data.table(
    nrem_count=sum(labels == "NREM"),
    rem_count=sum(labels=="REM"),
    wake_count=sum(labels=="WAKE")
  )
}


subset <- analysis_episodes[method=='iterative' & typ =='all' & protocol_section == 'pre']
ts <- analysis_episodes[method=='changepoint_compact' & typ =='all' & protocol_section == 'pre']
candidate_abes <- subset[,c(agreement=mean(agreement), raw_se=max(raw_se), lcount(label)), by='subject_code,activity_or_bedrest_episode']
#candidate_abes <- candidate_abes[nrem_count >= 4 & nrem_count <= 6 & rem_count >= 4 & rem_count <= 6 & wake_count <= 1, list(subject_code,activity_or_bedrest_episode)]
candidate_abes <- candidate_abes[nrem_count == rem_count & agreement > .90 & nrem_count >= 4 & nrem_count <= 6 & raw_se > .90]

setkeyv(sleep_data, c("subject_code","activity_or_bedrest_episode"))
setkeyv(subset, c("subject_code","activity_or_bedrest_episode"))
setkeyv(ts, c("subject_code","activity_or_bedrest_episode"))

training_data <- sleep_data[candidate_abes]
comparison_episodes <- subset[candidate_abes]
t_eps <- ts[candidate_abes]

comparison_episodes[,mpos:=(end_position+start_position)/2.0]
t_eps[,mpos:=(end_position+start_position)/2.0]
#cpeps[,mpos:=(end_position+start_position)/2.0]

as <- seq(from=-1.2,to=-.4,by=.1)
bs <- seq(from=-1.0,to=-1.5,by=-.1)
cs <- seq(from=-.9,to=-0.3,by=.1)
param_pairs <- as.data.table(expand.grid(a=as,b=bs,c=cs))
param_pairs <- param_pairs[!a==b & !a==c & !b==c]

res <- param_pairs[,residFun(.BY, training_data, comparison_episodes),by='a,b,c']

p <- list(a=-0.5, b=-1.2, c=0.5)
nls.out <- nls.lm(par=parStart, fn = residFun, dt = training_data, comparison_episodes = comparison_episodes, control = nls.lm.control(nprint=1))

residFun <- function(p, dt, comparison_episodes) {
  t_eps <- compact.changepoint(generate_episodes.changepoint(dt, distances=list(wake=p$a, rem=p$b, stage1=p$c, stage2=0, stage3=1), stage1 = TRUE))
  t_eps[,mpos:=(end_position+start_position)/2.0]
  distances <- comparison_episodes[,distance_f(mpos, start_position, end_position, .BY, t_eps), by='subject_code,activity_or_bedrest_episode,label']
  res <- mean(c(sum(distances[label=='REM']$distance),sum(distances[label=='NREM']$distance),sum(distances[label=='WAKE']$distance)))
  print(paste(p))
  print(res)
  print("---")
  res
}

distance_f <- function(midpoints, sps, eps, by_vals, comparison_episodes) {
  comparison_midpoints <- comparison_episodes[subject_code == by_vals$subject_code & activity_or_bedrest_episode == by_vals$activity_or_bedrest_episode & label == by_vals$label]$mpos

  if(length(comparison_midpoints) == 0) {
    comparison_midpoints <- sps[1]
  }
  
  if(length(midpoints) > length(comparison_midpoints)) {
    x <- midpoints
    y <- comparison_midpoints
  } else {
    x <- comparison_midpoints
    y <- midpoints
  }
  
  t <- 0
  for(i in y) {
    diffs <- abs(x - i)
    d <- min(diffs,na.rm=TRUE)
    t <- t + d
    x <- x[!d == diffs]
  }
  
  for(j in x) {
    diffs <- abs(y - j)
    t <- t + min(diffs,na.rm=TRUE)
  }
  list(distance=t)  
}

  
  
