##### NEWWW #######

### Distances:

# 3/4 --> 2 109
# 2 --> 1 97
# 2 --> REM 95
# 2 --> Wake 150

# 145
# 200
# 245
# 300
# 375
# 
# 
# tr_dt <- copy(sleep_data)
# 
# 
# sleep_data
# 
# sequences <- tr_dt[, chunk(stage, pk), by='subject_code,activity_or_bedrest_episode']
# sequences[,label:=relabel_to_biggest_neighbor(label, length, '7'),by='subject_code,activity_or_bedrest_episode']
# sequences[,label:=relabel_to_biggest_neighbor(label, length, '0'),by='subject_code,activity_or_bedrest_episode']
# sequences[,label:=relabel_to_biggest_neighbor(label, length, '9'),by='subject_code,activity_or_bedrest_episode']
# sequences[,label:=relabel_to_biggest_neighbor(label, length, '8'),by='subject_code,activity_or_bedrest_episode']
# sequences
# 
# length(as.numeric(rep(sequences$label, sequences$length)))
# dim(tr_dt)
# tr_dt[,stage:=as.numeric(rep(sequences$label, sequences$length))]
# tr_dt
# 
# tr_dt[stage == 5, dist:=5]
# tr_dt[stage == 1, dist:=6.1]
# tr_dt[stage == 6, dist:=7]
# tr_dt[stage == 2, dist:=8.1]
# tr_dt[stage == 3 | stage == 4, dist:=9.6]
# 
# 
# test <- tr_dt[activity_or_bedrest_episode ==10 & subject_code=='3335GX']
# 
# test
# 
# res <- cpt.mean(test$dist, method = "PELT", penalty="AIC1", pen.value=0.001)
# plot(res, type = 'l', cpt.col = "blue")

####################
## Changepoint Bouts
####################
generate_episodes.changepoint <- function(dt, distances=list(wake=5.5, rem=6.5, stage1=6.1, stage2=8.3, stage3=9.6), stage1=FALSE, clean=TRUE, ic="SIC") {
  
  # Re-label undefined epochs
  dt <- copy(dt)
  sequences <- dt[, chunk(stage, pk), by='subject_code,activity_or_bedrest_episode']
  
  if(clean) {
    sequences <- remove.target.label(sequences, target_label='7')
    sequences <- remove.target.label(sequences, target_label='0')
    sequences <- remove.target.label(sequences, target_label='9')
    sequences <- remove.target.label(sequences, target_label='8')    
  }
  if(!stage1)
    sequences <- remove.target.label(sequences, target_label='1')
  
  
  #print(sequences[label%in%c('7','0','9','8')]$label)
  dt[,stage:=as.numeric(rep(sequences$label, sequences$length))]
  
  #print(dt[stage%in%c(7,0,9,8)])
  
  dt[,group:=set_changepoint_group(as.data.table(stage), distances, stage1, ic),by='subject_code,activity_or_bedrest_episode']
  episodes <- dt[,merge_epochs(pk,stage),by='subject_code,activity_or_bedrest_episode,group']
  episodes[,group:=NULL]
  
  
  episodes[, method:='changepoint']
  episodes[,pk:=.I]
  episodes[,episode_type:=map_epoch_type(label),by='pk']
  episodes[label=="3" | label=="4",label:='3/4']
  episodes[label=="5", label:="WAKE"]
  episodes[label=="6" | label==6, label:="REM"]
  episodes[,pk:=NULL]
  episodes
  
}

## used in episodes (changepoint)
set_changepoint_group <- function(stages, distances, stage1, ic) {
  # Set distances for cpm.mean
  stages[,dist:=0]
  stages[stage == 5, dist:=distances$wake]
  if(stage1)
    stages[stage == 1, dist:=distances$stage1]
  stages[stage == 6, dist:=distances$rem]
  stages[stage == 2, dist:=distances$stage2]
  stages[stage == 3 | stage == 4, dist:=distances$stage3]
  
  #print(stages)
  changepoints <- cpt.mean(stages$dist, method="PELT", penalty=ic)@cpts
  #print(changepoints)
  # Add end of last group
  if(!nrow(stages)%in%changepoints)
    changepoints <- c(changepoints, length(stages))
  
  # Get lengths of each group
  lengths <- diff(c(0L, changepoints))
  
  # Label groups
  rep.int(seq(1,length(lengths)), lengths)
}


