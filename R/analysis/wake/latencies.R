# Helper Methods

inter_intervals <- function(starts,ends) {
  n <- length(starts)
  data.table(start_position=ends[-n], end_position=starts[-1L], i_length=starts[-1L] - ends[-n])
}

length_coefficient <- EPOCH_SECONDS/60.0 

# Latencies for REM, NREM (1,2,3/4), WAKE
# Latency by: 
# - previous state (REM, NREM 1,2,3/4)
# - length
# - time of night (REM_EPISODE/NREM_EPISODE)

# Get all sequences
sequences <- episodes[activity_or_bedrest_episode > 0 & method == 'raw']
sequences[,pik:=.I]
setkey(sequences, pik)

# Determine what cycle each sequence is in
cs <- copy(cycles[method=='classic' & type == "NREM"])
setnames(cs, c('start_position', 'end_position'), c('sp', 'ep'))
sequences[,cycle_number:=cs[start_position >= sp & end_position <= ep]$cycle_number, by='pik']

# Determine what type of (traditional) episode each sequence is in
ep <- copy(episodes.classic)
setnames(ep, c('start_position', 'end_position'), c('sp', 'ep'))
sequences[,episode_type:=ep[start_position >= sp & end_position <= ep]$label, by='pik']


# Determine information about the previous sequence
sequences[,prev_label:=c(NA,label[-.N]),by='subject_code,activity_or_bedrest_episode']
sequences[,prev_length:=c(NA,length[-.N]),by='subject_code,activity_or_bedrest_episode']

# Seperate into 3 types of sequences
rem_sequences <- sequences[label=="REM"]
nrem_sequences <- sequences[label=="NREM"]
wake_sequences <- sequences[label=="WAKE"]


# Use sleep data to find stage 2,3 latencies
sd <- copy(sleep_data)
setnames(sd, c('subject_code', 'activity_or_bedrest_episode'), c('sc','abe'))
sd[,position:=.I]
stage_2s <- sd[stage==2]
stage_3s <- sd[stage==3 | stage==4]

tl <<- list()
stage_3s[,{tl[[paste(sc,abe,sep="_")]] <<- c(position);0},by='sc,abe']
sequences[,next_stage_3:=tl[[paste(subject_code,activity_or_bedrest_episode,sep='_')]][(which.max(tl[[paste(subject_code,activity_or_bedrest_episode,sep='_')]] > end_position))],by='subject_code,activity_or_bedrest_episode,end_position']

tl <<- list()
stage_2s[,tl[[paste(sc,abe,sep="_")]]<<-c(position),by='sc,abe']
sequences[,next_stage_2:=tl[[paste(subject_code,activity_or_bedrest_episode,sep='_')]][(which.max(tl[[paste(subject_code,activity_or_bedrest_episode,sep='_')]] > end_position))],by='subject_code,activity_or_bedrest_episode,end_position']

sequences[,stage_2_latency:=next_stage_2-end_position]
sequences[,stage_3_latency:=next_stage_3-end_position]
sequences[stage_2_latency < 0, stage_2_latency:=NA]
sequences[stage_3_latency < 0, stage_3_latency:=NA]


# Find REM, NREM, and WAKE latencies
rem_lat_merged <- merge(sequences,rem_sequences,by=c('subject_code','activity_or_bedrest_episode'),all.x=TRUE,suffixes=c("",".rem"),allow.cartesian=TRUE)
rem_latencies <- rem_lat_merged[start_position.rem>end_position,data.table(next_rem_position=min(start_position.rem),rem_latency=min(start_position.rem-end_position),next_rem_length=length.rem[1]),by='subject_code,activity_or_bedrest_episode,label,start_position,end_position,length,episode_type,cycle_number,prev_label,prev_length']
rm(rem_lat_merged)

nrem_lat_merged <- merge(sequences,nrem_sequences,by=c('subject_code','activity_or_bedrest_episode'),all.x=TRUE,suffixes=c("",".nrem"),allow.cartesian=TRUE)
nrem_latencies <- nrem_lat_merged[start_position.nrem>end_position,data.table(next_nrem_position=min(start_position.nrem),nrem_latency=min(start_position.nrem-end_position),next_nrem_length=length.nrem[1]),by='subject_code,activity_or_bedrest_episode,label,start_position,end_position,length,episode_type,cycle_number,prev_label,prev_length']
rm(nrem_lat_merged)


wake_lat_merged <- merge(sequences,wake_sequences,by=c('subject_code','activity_or_bedrest_episode'),all.x=TRUE,suffixes=c("",".wake"),allow.cartesian=TRUE)
wake_latencies <- wake_lat_merged[start_position.wake>end_position,data.table(next_wake_position=min(start_position.wake),wake_latency=min(start_position.wake-end_position),next_wake_length=length.wake[1]),by='subject_code,activity_or_bedrest_episode,label,start_position,end_position,length,episode_type,cycle_number,prev_label,prev_length']
rm(wake_lat_merged)





#qplot(data=rem_latencies[prev_label=='REM' & prev_length > 10 & label=='WAKE' & length > 10],rem_latency,geom='density',log='x')


length_breaks_in_epochs <- c(0, 1, 2, 5,10,30,100,1000)
length_breaks <- length_breaks_in_epochs * length_coefficient

rem_latencies[,length_class:=cut(length * length_coefficient, length_breaks,include.lowest = TRUE)]
nrem_latencies[,length_class:=cut(length * length_coefficient, length_breaks,include.lowest = TRUE)]
wake_latencies[,length_class:=cut(length * length_coefficient, length_breaks,include.lowest = TRUE)]
sequences[,length_class:=cut(length * length_coefficient, length_breaks, include.lowest=TRUE)]

# rem_p <- ggplot(data=rem_latencies[label=="WAKE" & prev_label %in% c("REM", "NREM")], aes(x=rem_latency))
# #nrem_p <- ggplot(data=nrem_latencies[label=="WAKE" & night_pos %in% c("NREM1", "NREM2", "NREM3", "NREM4", "NREM5") & next_nrem_length > 30], aes(x=nrem_latency))
# nrem_p <- ggplot(data=nrem_latencies[label=="WAKE" & prev_label %in% c("REM", "NREM")], aes(x=nrem_latency))
# wake_p <- ggplot(data=wake_latencies[label=="WAKE" & prev_label %in% c("REM", "NREM")], aes(x=wake_latency))
# s2_p <- ggplot(data=e[label=="WAKE" & prev_label %in% c("REM", "NREM")], aes(x=stage_2_latency))
# s3_p <- ggplot(data=e[label=="WAKE" & prev_label %in% c("REM", "NREM")], aes(x=stage_3_latency))
# 
# 
# 
# rem_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ .) + scale_x_log10()
# nrem_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ .) + scale_x_log10()
# wake_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ .) + scale_x_log10()
# s2_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ .) + scale_x_log10()
# s3_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ .) + scale_x_log10()
# 






iri <- sequences[label=="REM",inter_intervals(start_position,end_position),by='subject_code,activity_or_bedrest_episode']
ini <- sequences[label=="NREM",inter_intervals(start_position,end_position),by='subject_code,activity_or_bedrest_episode']
iwi <- sequences[label=="WAKE",inter_intervals(start_position,end_position),by='subject_code,activity_or_bedrest_episode']

iri[,pik:=.I]
iri[,episode_type:=episodes.classic[(start_position + i_length/2) >= start_position & (start_position + i_length/2) <= end_position]$label, by='pik']
iri[,cycle_number:=cs[(start_position + i_length/2) >= start_position & (start_position + i_length/2) <= end_position]$cycle_number, by='pik']

ini[,pik:=.I]
ini[,episode_type:=episodes.classic[(start_position + i_length/2) >= start_position & (start_position + i_length/2) <= end_position]$label, by='pik']
ini[,cycle_number:=cs[(start_position + i_length/2) >= start_position & (start_position + i_length/2) <= end_position]$cycle_number, by='pik']

iwi[,pik:=.I]
iwi[,episode_type:=episodes.classic[(start_position + i_length/2) >= start_position & (start_position + i_length/2) <= end_position]$label, by='pik']
iwi[,cycle_number:=cs[(start_position + i_length/2) >= start_position & (start_position + i_length/2) <= end_position]$cycle_number, by='pik']




