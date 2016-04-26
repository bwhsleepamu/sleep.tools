set_agreement <- function(episodes, sleep_data) {
  ae <- episodes
  sd <- copy(sleep_data)
  
  sd[,label_a:=map_epoch_type(stage),by='pk']
  sd[,label_b:="OTHER"]
  sd[stage==3 | stage==4,label_b:='3/4']
  sd[stage==5, label_b:="WAKE"]
  sd[stage==6, label_b:="REM"]
  sd[stage==2, label_b:='2']
  
  ae[,pik:=.I]
  ae[, agreement:=sum(sd[start_position:end_position]$label_a == label)/length,by='pik']
  #ae[method=='iterative', agreement:=sum(sd[start_position:end_position]$label_a == label)/length,by='pik']
}


