source("R/sources.R")
source("R/plotting/rasters/sequence_raster_plot.R")
library(gridExtra)

# Helper Methods

inter_intervals <- function(starts,ends,t) {
  n <- length(starts)
  return_dt <- data.table(start_position=ends[-n]+1, end_position=starts[-1L]-1, i_length=starts[-1L] - ends[-n]-1)
  return_dt[,type:=t]
}


isi_stats <- function(start, end, sleep_data) {
  as.list(table(sleep_data[start:end]$high_res_epoch_type))
}

isi_heatmap <- function(isi_data, isi_type, isi_range = c(0,9000), bin_widths=c(10,10), scale_cutoff=c(0,1), axis_ranges=c(300,400)) {
  y_data <- isi_data[type==isi_type & interval_length <= isi_range[2] & interval_length > isi_range[1]]$interval_length_wake
  x_data <- isi_data[type==isi_type & interval_length <= isi_range[2] & interval_length > isi_range[1]]$interval_length_without_wake
  
  # print(length(y_data))
  # print(length(x_data))
  
  y_bin_width <- bin_widths[1]
  x_bin_width <- bin_widths[2]
  
  y_breaks <- c(seq(from=0, to=axis_ranges[1], by=y_bin_width))
  y_bins <- cut(y_data, y_breaks,include.lowest = TRUE, right=FALSE, ordered_result = TRUE)
  
  x_breaks <- c(seq(from=0, to=axis_ranges[2], by=x_bin_width))
  x_bins <- cut(x_data, x_breaks,include.lowest = TRUE, right=FALSE, ordered_result = TRUE)
  
  #print(length(y_breaks))
  #print(length(x_breaks))
  
    
  #print(length(y_bins))
  #print(length(x_bins))
  
  #print(length(y_data))
  #print(length(x_data))
  
  heat_dt <- data.table(y=y_data, x=x_data, y_bin=y_bins, x_bin=x_bins)


  
  heat_dt[,row_count:=.N,by='y_bin']
  
  heat_dt[,val:=.N/row_count,by='y_bin,x_bin']
  
  heat_dt[val > scale_cutoff[2], val:=scale_cutoff[2]]
  heat_dt[val < scale_cutoff[1], val:=0]
  
  x_lab_count <- seq(from=1, to=length(levels(x_bins)), by=5)
  x_labels <- rep("", length(levels(x_bins)))
  x_labels[x_lab_count] <-  levels(x_bins)[x_lab_count]
  
  y_lab_count <- seq(from=1, to=length(levels(y_bins)), by=5)
  y_labels <- rep("", length(levels(y_bins)))
  y_labels[y_lab_count] <-  levels(y_bins)[y_lab_count]
  
  #print(heat_dt)
  
  list(heatmap_data=copy(heat_dt), type=isi_type, x_labs=x_labels, y_labs=y_labels, x_levels=levels(x_bins), y_levels=levels(y_bins))
}
  
function() {
  
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
  sequences <- sequences[activity_or_bedrest_episode > 0]
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
  length_labels <- c("< 2 minutes", "2 - 5 minutes", "5-15 minutes", "15 - 60 minutes", "60+ minutes")
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
    
  #iwi[,episode_type:=episodes.classic[(start_position + i_length/2) >= start_position & (start_position + i_length/2) <= end_position]$label, by='pik']
  inter_state_intervals <- inter_state_intervals[activity_or_bedrest_episode > 0]
  
  setnames(inter_state_intervals, c('i_length'), c('interval_length') )
  inter_state_intervals[,`:=`(start_labtime=sleep_data[start_position]$labtime, end_labtime=sleep_data[end_position]$labtime+EPOCH_LENGTH)]
  
  # Convert lengths to minutes
  inter_state_intervals[,interval_length_in_epochs:=interval_length]
  inter_state_intervals[,interval_length:=interval_length*length_coefficient]
  
  setcolorder(inter_state_intervals, c('subject_code', 'activity_or_bedrest_episode', 'type', 'interval_length', 'start_labtime', 'end_labtime', 'start_position', 'end_position', 'interval_length_in_epochs'))
  
  
  # Add phase information
  setkey(sleep_episodes, subject_code, activity_or_bedrest_episode)
  sequences[,phase_label:=sleep_episodes[list(sequences$subject_code, sequences$activity_or_bedrest_episode)]$phase_label]
  inter_state_intervals[,phase_label:=sleep_episodes[list(inter_state_intervals$subject_code, inter_state_intervals$activity_or_bedrest_episode)]$phase_label]
  inter_state_intervals[,pik:=.I]
  inter_state_intervals[,cycle_number:=cs[(start_position + interval_length/2) >= start_position & (start_position + interval_length/2) <= end_position]$cycle_number, by='pik']
  
  inter_state_intervals[,c("N1", "N2", "REM", "SWS", "UNDEF", "WAKE"):=isi_stats(start_position,end_position,sleep_data),by='pik']
  inter_state_intervals[,wake_percentage:=WAKE/interval_length_in_epochs]
  
  inter_state_intervals[wake_percentage <= .05, wake_level:="0% - 5%"]
  inter_state_intervals[wake_percentage > .05 & wake_percentage <=.2, wake_level:="5% - 20%"]
  inter_state_intervals[wake_percentage > .2, wake_level:="20% - 100%"]
  
  inter_state_intervals$wake_level <- factor(inter_state_intervals$wake_level, levels(factor(inter_state_intervals$wake_level))[c(2,3,1)])
  
  
  inter_state_intervals[,interval_length_without_wake:=(interval_length_in_epochs-WAKE)*length_coefficient]
  inter_state_intervals[,interval_length_wake:=(WAKE)*length_coefficient]
  
  
  # function(isi_data, isi_type, isi_range = c(1,9000), bin_widths=c(10,10), scale_cutoff=1)
    
  heatmap_data_list <- list(
    SWS=isi_heatmap(inter_state_intervals, "SWS", isi_range=c(0,400), bin_width=c(1,1), scale_cutoff = c(.01, .3), axis_ranges = c(120,180)),
    REM=isi_heatmap(inter_state_intervals, "REM", isi_range=c(0,400), bin_width=c(5,5), scale_cutoff = c(.01, .3), axis_ranges = c(120,180)),
    WAKE=isi_heatmap(inter_state_intervals, "WAKE", isi_range=c(0,400), bin_width=c(10,10)),
    NREM=isi_heatmap(inter_state_intervals, "NREM", isi_range=c(0,1000), bin_width=c(10,10)),
    N1=isi_heatmap(inter_state_intervals, "N1", isi_range=c(0,400), bin_width=c(10,10)),
    N2=isi_heatmap(inter_state_intervals, "N2", isi_range=c(0,400), bin_width=c(10,10))
  )
  
}
