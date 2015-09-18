# Helper Methods

inter_intervals <- function(starts,ends) {
  n <- length(starts)
  data.table(sp=ends[-n], ep=starts[-1L], i_length=starts[-1L] - ends[-n])
}


# Latencies for REM, NREM (1,2,3/4), WAKE
# Latency by: 
# - previous state (REM, NREM 1,2,3/4)
# - length
# - time of night (REM_EPISODE/NREM_EPISODE)


e <- episodes.raw
e <- e[activity_or_bedrest_episode > 0]
e[,pik:=.I]
setnames(e, c('start_position','end_position'), c('sp', 'ep'))

cs <- cycles[method=='classic' & type == "NREM"]

e[,episode_type:=episodes.classic[sp >= start_position & ep <= end_position]$label, by='pik']
e[,cycle_number:=cs[sp >= start_position & ep <= end_position]$cycle_number, by='pik']
e[,prev_label:=c(NA,label[-.N]),by='subject_code,activity_or_bedrest_episode']
e[,prev_length:=c(NA,length[-.N]),by='subject_code,activity_or_bedrest_episode']

rem_sequences <- e[label=="REM"]
nrem_sequences <- e[label=="NREM"]
wake_sequences <- e[label=="WAKE"]

sd <- copy(sleep_data)
setnames(sd, c('subject_code', 'activity_or_bedrest_episode'), c('sc','abe'))
sd[,position:=.I]
stage_2s <- sd[stage==2]
stage_3s <- sd[stage==3 | stage==4]

# setnames(rem_sequences,c("sp","ep","subject_code","activity_or_bedrest_episode"),c("start_pos","end_pos","sc","abe"))
# setnames(nrem_sequences,c("sp","ep","subject_code","activity_or_bedrest_episode"),c("start_pos","end_pos","sc","abe"))
# setnames(wake_sequences,c("sp","ep","subject_code","activity_or_bedrest_episode"),c("start_pos","end_pos","sc","abe"))

# e[,next_rem_start:=rem_sequences[sc==subject_code & activity_or_bedrest_episode==abe & start_pos > ep]$start_pos[1],by='pik']
# e[,next_nrem_start:=nrem_sequences[sc==subject_code & activity_or_bedrest_episode==abe & start_pos > ep]$start_pos[1],by='pik']
# e[,next_wake_start:=wake_sequences[sc==subject_code & activity_or_bedrest_episode==abe & start_pos > ep]$start_pos[1],by='pik']



# e[,rem_latency:=next_rem_start-ep]
# e[,nrem_latency:=next_nrem_start-ep]
# e[,wake_latency:=next_wake_start-ep]


#####
#####
e[,next_stage_3:=NULL]
tl <<- list()
stage_3s[,{tl[[paste(sc,abe,sep="_")]]<<-c(position);0},by='sc,abe']
e[,next_stage_3:=tl[[paste(subject_code,activity_or_bedrest_episode,sep='_')]][(which.max(tl[[paste(subject_code,activity_or_bedrest_episode,sep='_')]] > ep))],by='subject_code,activity_or_bedrest_episode,ep']

e[,next_stage_2:=NULL]
tl <<- list()
stage_2s[,tl[[paste(sc,abe,sep="_")]]<<-c(position),by='sc,abe']
e[,next_stage_2:=tl[[paste(subject_code,activity_or_bedrest_episode,sep='_')]][(which.max(tl[[paste(subject_code,activity_or_bedrest_episode,sep='_')]] > ep))],by='subject_code,activity_or_bedrest_episode,ep']

e[,stage_2_latency:=next_stage_2-ep]
e[,stage_3_latency:=next_stage_3-ep]
e[stage_2_latency < 0, stage_2_latency:=NA]
e[stage_3_latency < 0, stage_3_latency:=NA]
#####


rem_lat_merged <- merge(e,rem_sequences,by=c('subject_code','activity_or_bedrest_episode'),all.x=TRUE,suffixes=c("",".rem"),allow.cartesian=TRUE)
rem_latencies <- rem_lat_merged[sp.rem>ep,data.table(next_rem_position=min(sp.rem),rem_latency=min(sp.rem-ep),next_rem_length=length.rem[1]),by='subject_code,activity_or_bedrest_episode,label,sp,ep,length,episode_type,cycle_number,prev_label,prev_length']
rm(rem_lat_merged)

nrem_lat_merged <- merge(e,nrem_sequences,by=c('subject_code','activity_or_bedrest_episode'),all.x=TRUE,suffixes=c("",".nrem"),allow.cartesian=TRUE)
nrem_latencies <- nrem_lat_merged[sp.nrem>ep,data.table(next_nrem_position=min(sp.nrem),nrem_latency=min(sp.nrem-ep),next_nrem_length=length.nrem[1]),by='subject_code,activity_or_bedrest_episode,label,sp,ep,length,episode_type,cycle_number,prev_label,prev_length']
rm(nrem_lat_merged)


wake_lat_merged <- merge(e,wake_sequences,by=c('subject_code','activity_or_bedrest_episode'),all.x=TRUE,suffixes=c("",".wake"),allow.cartesian=TRUE)
wake_latencies <- wake_lat_merged[sp.wake>ep,data.table(next_wake_position=min(sp.wake),wake_latency=min(sp.wake-ep),next_wake_length=length.wake[1]),by='subject_code,activity_or_bedrest_episode,label,sp,ep,length,episode_type,cycle_number,prev_label,prev_length']
rm(wake_lat_merged)





qplot(data=rem_latencies[prev_label=='REM' & prev_length > 10 & label=='WAKE' & length > 10],rem_latency,geom='density',log='x')


length_breaks <- c(0, 1, 2, 5,10,30,100,1000)

rem_latencies[,length_class:=cut(length, length_breaks,include.lowest = TRUE)]
nrem_latencies[,length_class:=cut(length, length_breaks,include.lowest = TRUE)]
wake_latencies[,length_class:=cut(length, length_breaks,include.lowest = TRUE)]
e[,length_class:=cut(length, length_breaks, include.lowest=TRUE)]

rem_p <- ggplot(data=rem_latencies[label=="WAKE" & prev_label %in% c("REM", "NREM")], aes(x=rem_latency))
#nrem_p <- ggplot(data=nrem_latencies[label=="WAKE" & night_pos %in% c("NREM1", "NREM2", "NREM3", "NREM4", "NREM5") & next_nrem_length > 30], aes(x=nrem_latency))
nrem_p <- ggplot(data=nrem_latencies[label=="WAKE" & prev_label %in% c("REM", "NREM")], aes(x=nrem_latency))
wake_p <- ggplot(data=wake_latencies[label=="WAKE" & prev_label %in% c("REM", "NREM")], aes(x=wake_latency))
s2_p <- ggplot(data=e[label=="WAKE" & prev_label %in% c("REM", "NREM")], aes(x=stage_2_latency))
s3_p <- ggplot(data=e[label=="WAKE" & prev_label %in% c("REM", "NREM")], aes(x=stage_3_latency))



rem_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ .) + scale_x_log10()
nrem_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ .) + scale_x_log10()
wake_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ .) + scale_x_log10()
s2_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ .) + scale_x_log10()
s3_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ .) + scale_x_log10()




iri <- e[label=="REM",inter_intervals(sp,ep),by='subject_code,activity_or_bedrest_episode']
ini <- e[label=="NREM",inter_intervals(sp,ep),by='subject_code,activity_or_bedrest_episode']
iwi <- e[label=="WAKE",inter_intervals(sp,ep),by='subject_code,activity_or_bedrest_episode']

iri[,pik:=.I]
iri[,episode_type:=episodes.classic[(sp + i_length/2) >= start_position & (sp + i_length/2) <= end_position]$label, by='pik']
iri[,cycle_number:=cs[(sp + i_length/2) >= start_position & (sp + i_length/2) <= end_position]$cycle_number, by='pik']

ini[,pik:=.I]
ini[,episode_type:=episodes.classic[(sp + i_length/2) >= start_position & (sp + i_length/2) <= end_position]$label, by='pik']
ini[,cycle_number:=cs[(sp + i_length/2) >= start_position & (sp + i_length/2) <= end_position]$cycle_number, by='pik']

iwi[,pik:=.I]
iwi[,episode_type:=episodes.classic[(sp + i_length/2) >= start_position & (sp + i_length/2) <= end_position]$label, by='pik']
iwi[,cycle_number:=cs[(sp + i_length/2) >= start_position & (sp + i_length/2) <= end_position]$cycle_number, by='pik']




