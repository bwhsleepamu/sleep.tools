####################
## Changepoint Bouts
####################
generate.episodes.changepoint <- function(dt, wake=TRUE, undef=FALSE, cpmType="Mann-Whitney", ARL0=10000, startup=20) {
  dt[,group:=set_changepoint_group(epoch_type,cpmType=cpmType,ARL0=ARL0,startup=startup),by='subject_code,activity_or_bedrest_episode']
  episodes <- dt[,merge_epochs(pk,epoch_type),by='subject_code,activity_or_bedrest_episode,group']
  episodes[,group:=NULL]
  
  if(!undef)
    episodes <- remove.target.label.dt(episodes, target_label="UNDEF")  
  if(!wake)
    episodes <- remove.target.label.dt(episodes, target_label="WAKE")
  
  episodes[, method:='changepoint']
  episodes
}
