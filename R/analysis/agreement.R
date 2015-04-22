ae <- copy(episodes)
sd <- copy(sleep_data)

sd[,label_a:=map_epoch_type(stage),by='pk']
sd[,label_b:="OTHER"]
sd[stage==3 | stage==4,label_b:='3/4']
sd[stage==5, label_b:="WAKE"]
sd[stage==6, label_b:="REM"]
sd[stage==2, label_b:='2']

ae[,pik:=.I]
ae[method=='changepoint',agreement:=sum(sd[start_position:end_position]$label_b == label)/length,by='pik']


unique(a_e$label)
sleep_data[]

qplot(agreement, data=ae[method!='classic'], geom='density', facets=. ~ method, color=method)