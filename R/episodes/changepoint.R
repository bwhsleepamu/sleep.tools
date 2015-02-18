####################
## Changepoint Bouts
####################
generate_episodes.changepoint <- function(dt, wake=TRUE, undef=FALSE, cpmType="Mann-Whitney", ARL0=10000, startup=20) {
  dt[,group:=set_changepoint_group(epoch_type,cpmType=cpmType,ARL0=ARL0,startup=startup),by='subject_code,activity_or_bedrest_episode']
  episodes <- dt[,merge_epochs(pk,epoch_type),by='subject_code,activity_or_bedrest_episode,group']
  episodes[,group:=NULL]
  
  if(!undef)
    episodes <- remove.target.label(episodes, target_label="UNDEF")  
  if(!wake)
    episodes <- remove.target.label(episodes, target_label="WAKE")
  
  episodes[, method:='changepoint']
  episodes
}

## used in episodes (changepoint)
set_changepoint_group <- function(epoch_type, cpmType="Mann-Whitney", ARL0=10000, startup=20) {
  changepoints <- processStream(epoch_type, cpmType=cpmType, ARL0=ARL0, startup=startup)$changePoints
  
  # Add end of last group
  if(!length(epoch_type)%in%changepoints)
    changepoints <- c(changepoints, length(epoch_type))
  
  # Get lengths of each group
  lengths <- diff(c(0L, changepoints))
  
  # Label groups
  rep.int(seq(1,length(lengths)), lengths)
}


