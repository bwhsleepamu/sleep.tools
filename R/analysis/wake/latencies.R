# Helper Methods

inter_intervals <- function(starts,ends,t) {
  n <- length(starts)
  return_dt <- data.table(start_position=ends[-n], end_position=starts[-1L], i_length=starts[-1L] - ends[-n])
  return_dt[,type:=t]
}

length_coefficient <- EPOCH_SECONDS/60.0 

# Latencies for REM, NREM (1,2,3/4), WAKE
# Latency by: 
# - previous state (REM, NREM 1,2,3/4)
# - length
# - time of night (REM_EPISODE/NREM_EPISODE)

# Get all sequences
sleep_data[,high_res_epoch_type:=as.factor(as.character(lapply(stage, map_high_res_epoch_type))),]
high_res_sequences <- sleep_data[, chunk(high_res_epoch_type, pk), by='subject_code,activity_or_bedrest_episode']
sequences <- sleep_data[, chunk(epoch_type, pk), by='subject_code,activity_or_bedrest_episode']
high_res_sequences[,tag:='high_res']
sequences[,tag:='normal']
sequences <- rbindlist(list(high_res_sequences, sequences))
sequences[,pik:=.I]
setkey(sequences, pik)

# Determine what cycle each sequence is in
cs <- copy(cycles[method=='classic' & type == "NREM"])
setnames(cs, c('start_position', 'end_position'), c('sp', 'ep'))
sequences[,cycle_number:=cs[start_position >= sp & end_position <= ep]$cycle_number, by='pik']

# Determine what type of (traditional) episode each sequence is in
# ep <- copy(episodes.classic)
# setnames(ep, c('start_position', 'end_position'), c('sp', 'ep'))
# sequences[,episode_type:=ep[start_position >= sp & end_position <= ep]$label, by='pik']


# Determine information about the previous sequence
sequences[,prev_label:=c(NA,label[-.N]),by='subject_code,activity_or_bedrest_episode,tag']
sequences[,prev_length:=c(NA,length[-.N]),by='subject_code,activity_or_bedrest_episode,tag']

# Seperate into 3 types of sequences
# rem_sequences <- sequences[label=="REM"]
# nrem_sequences <- sequences[label=="NREM"]
# wake_sequences <- sequences[label=="WAKE"]


# Latencies: rem, nrem, stage 2, stage 3, wake

# Use sleep data to find stage 2,3 latencies
sd <- copy(sleep_data)
setnames(sd, c('subject_code', 'activity_or_bedrest_episode'), c('sc','abe'))
sd[,position:=.I]

lapply(list("NREM", "N2", "SWS", "REM", "WAKE"), function(e){
  seq_subset <- sd[high_res_epoch_type == e | epoch_type == e]
  temp_list <<- list()
  col_name <- paste("next", e, sep="_")
  seq_subset[,{temp_list[[paste(sc,abe,sep="_")]] <<- c(position)}, by='sc,abe']
  sequences[,new_next_col:=temp_list[[paste(subject_code,activity_or_bedrest_episode,sep='_')]][(which.max(temp_list[[paste(subject_code,activity_or_bedrest_episode,sep='_')]] > end_position))],by='tag,subject_code,activity_or_bedrest_episode,end_position']
  sequences[,new_latency_col:=new_next_col-end_position]
  sequences[new_latency_col < 0,new_latency_col:=NA]
  setnames(sequences, c("new_next_col", "new_latency_col"), c(col_name, paste(e,'latency',sep="_")))
})


length_breaks <- c(0, 2, 5, 15, 60, 3000)
length_labels <- c("very_short", "short", "medium", "long", "very_long")
sequences[,length_class:=cut(length * length_coefficient, length_breaks, include.lowest=TRUE, right=TRUE, labels = length_labels)]

# Convert lengths to minutes
sequences[,`:=`(start_labtime=sleep_data[start_position]$labtime, end_labtime=sleep_data[end_position]$labtime)]
sequences[,length_in_epochs:=length]
sequences[,length:=length_in_epochs*length_coefficient]




# Inter-State Intervals
inter_state_interval_list <- lapply(list("N1", "N2", "SWS", "REM", "WAKE"), function(e){
  sequences[label==e & tag == "high_res", inter_intervals(start_position,end_position,e),by='subject_code,activity_or_bedrest_episode']
})
  
ini <-   sequences[label=="NREM" & tag == "normal", inter_intervals(start_position,end_position,"NREM"),by='subject_code,activity_or_bedrest_episode']

inter_state_intervals <- rbindlist(inter_state_interval_list)
inter_state_intervals <- rbindlist(list(inter_state_intervals, ini))
  
iwi[,episode_type:=episodes.classic[(start_position + i_length/2) >= start_position & (start_position + i_length/2) <= end_position]$label, by='pik']

setnames(inter_state_intervals, c('i_length'), c('interval_length') )
inter_state_intervals[,`:=`(start_labtime=sleep_data[start_position]$labtime, end_labtime=sleep_data[end_position]$labtime)]

# Convert lengths to minutes
inter_state_intervals[,interval_length_in_epochs:=interval_length]
inter_state_intervals[,interval_length:=interval_length*length_coefficient]

setcolorder(inter_state_intervals, c('subject_code', 'activity_or_bedrest_episode', 'type', 'interval_length', 'start_labtime', 'end_labtime', 'episode_type', 'cycle_number', 'start_position', 'end_position', 'interval_length_in_epochs'))


# Add phase information
setkey(sleep_episodes, subject_code, activity_or_bedrest_episode)
sequences[,phase_label:=sleep_episodes[list(sequences$subject_code, sequences$activity_or_bedrest_episode)]$phase_label]
inter_state_intervals[,phase_label:=sleep_episodes[list(inter_state_intervals$subject_code, inter_state_intervals$activity_or_bedrest_episode)]$phase_label]
inter_state_intervals[,pik:=.I]
inter_state_intervals[,cycle_number:=cs[(start_position + interval_length/2) >= start_position & (start_position + interval_length/2) <= end_position]$cycle_number, by='pik']


isi_stats <- function(start, end, sleep_data) {
    as.list(table(sleep_data[start:end]$high_res_epoch_type))
}

inter_state_intervals[,c("N1", "N2", "REM", "SWS", "UNDEF", "WAKE"):=isi_stats(start_position,end_position,sleep_data),by='pik']

sequences



