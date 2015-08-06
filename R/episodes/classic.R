##################
## Classic V1
##################

# generate_episodes.classic <- function(dt, wake=FALSE, undef=FALSE, min_nrem_length=NULL, min_rem_length=NULL, min_wake_length=NULL) {
#   # Take series of epochs and collapse them into sequences of the same type  
#   sequences <- dt[, chunk(epoch_type, pk), by='subject_code,activity_or_bedrest_episode']
#   
#   # Re-label undefined sequences as their biggest neighbor
#   ## TODO: NAs show up!
#   if(!undef)
#     sequences <- remove.target.label(sequences, target_label="UNDEF")    
#   
#   # Merge around seeds by setting a label and group number for each sequence.
#   sequences[,c('label', 'group'):=merge_around_seeds(label, length, wake=wake, min_nrem_length=min_nrem_length, min_rem_length=min_rem_length, min_wake_length=min_wake_length), by='subject_code,activity_or_bedrest_episode']
#   
#   # Collapse all sequences within a group number into one episode
#   episodes <- sequences[,merge_group(start_position, end_position, label, length), by='subject_code,activity_or_bedrest_episode,group']
#   episodes[,`:=`(group=NULL, method='classic')]
#   
#   episodes
# }

##################
## Classic V2
##################

### THIS IS A METHOD THAT JUMPS STRAIGHT TO CYCLES
generate_episodes.raw <- function(dt) {
  episodes <- dt[, chunk(epoch_type, pk), by='subject_code,activity_or_bedrest_episode']
  episodes[,method:="raw"]
}

generate_episodes.classic <- function(dt, min_nrem_length=15, min_rem_length=5, burst_length = 15, completion_cutoff=5, e_length = .5) {
  ## SETUPOP
  dt <- copy(sleep_data[subject_code=='18B2XX'])
  min_nrem_length=15
  min_rem_length=5
  burst_length = 15
  completion_cutoff=5
  e_length = .5
   
  
  ## Take series of epochs and collapse them into sequences of the same type  
  sequences <- dt[, chunk(epoch_type, pk), by='subject_code,activity_or_bedrest_episode']
  sequences <- remove.target.label(sequences, target_label="UNDEF")  
  sequences <- sequences[activity_or_bedrest_episode > 0]
  sequences[,pik:=seq(1,.N),by='subject_code,activity_or_bedrest_episode']
  
  ## Find NREM Bursts
  sequences[, nrem_burst:=FALSE]
  sequences[label == 'NREM' & (length * e_length) >= burst_length, nrem_burst:=TRUE]
  sequences[nrem_burst==TRUE, burst_id:=seq(1,.N), by='subject_code,activity_or_bedrest_episode']
    
  ## Find first REM Episodes
  
  # Number REM Sequences 
  sequences[label == "REM", rem_id:=seq(1,.N),by='subject_code,activity_or_bedrest_episode']
  
  # Label first REM
  sequences[, first_rem:=FALSE]
  sequences[label == "REM" & rem_id == 1, first_rem:=TRUE]

  ## Find inter-Burst cumulative REM
  # Label inter-Burst interval
  sequences[,inter_burst_id:=set_inter_burst_intervals(.BY,.SD),by='subject_code,activity_or_bedrest_episode']
  sequences[,total_ibi:=max(inter_burst_id),by='subject_code,activity_or_bedrest_episode']
  
  # Get info on the intervals
  all_inter_burst_intervals <- sequences[,data.table(inter_burst_id=unique(inter_burst_id)),by='subject_code,activity_or_bedrest_episode']
  
  # Cumulative rem amounts
  cum_rem <- sequences[label=="REM", data.table(cumulative_rem=sum(length),first_rem_position=min(start_position),last_rem_position=max(end_position)),by='subject_code,activity_or_bedrest_episode,inter_burst_id']
  cum_rem[cumulative_rem>=(min_rem_length/e_length),above_threshold:=TRUE]
  
  # First REM of sleep episode
  has_first_rem <- sequences[label=="REM", data.table(has_first_rem=any(first_rem)),by='subject_code,activity_or_bedrest_episode,inter_burst_id']
  
  # Merging and cleanup
  all_inter_burst_intervals <- merge(all_inter_burst_intervals,cum_rem,by=c('subject_code','activity_or_bedrest_episode','inter_burst_id'),all.x=TRUE)
  all_inter_burst_intervals <- merge(all_inter_burst_intervals,has_first_rem,by=c('subject_code','activity_or_bedrest_episode','inter_burst_id'),all.x=TRUE)
  all_inter_burst_intervals[is.na(cumulative_rem),cumulative_rem:=0]
  all_inter_burst_intervals[is.na(above_threshold),above_threshold:=FALSE]
  all_inter_burst_intervals[is.na(has_first_rem),has_first_rem:=FALSE]
  
  sequences <- merge(sequences,all_inter_burst_intervals,by=c("subject_code","activity_or_bedrest_episode","inter_burst_id"),all.x=TRUE)
  
  # Total: 2143
  ## Outside the tails (first/last ibi excluded):
  
  collapsed <- list()
  # 1. Collapse those without enough REM into NREM burst
  # 105
  to_collapse <- sequences[!above_threshold & !has_first_rem & inter_burst_id!=0 & inter_burst_id!=total_ibi]
  collapsed$below_threshold <- to_collapse[,data.table(start_position=min(start_position),end_position=max(end_position),label="NREM",length=sum(length),complete=TRUE),by='subject_code,activity_or_bedrest_episode,inter_burst_id'] 

  # 2. Collapse all sequences from first rem pos to last rem pos where threshold reached into rem episodes
  # 1298
  to_collapse <- sequences[(above_threshold | has_first_rem) & inter_burst_id!=0 & inter_burst_id!=total_ibi & start_position >= first_rem_position & end_position <= last_rem_position]
  collapsed$rem <-to_collapse[,data.table(start_position=min(start_position),end_position=max(end_position),label="REM",length=sum(length),complete=TRUE),by='subject_code,activity_or_bedrest_episode,inter_burst_id'] 
  
  # 3. Collapse all sequences in inter-burst but outside of REM episodes 
  # a. Before REM episode (200)
  to_collapse <- sequences[inter_burst_id!=0 & inter_burst_id != total_ibi & (above_threshold | has_first_rem) & (end_position < first_rem_position)]
  collapsed$pre_rem <- to_collapse[,data.table(start_position=min(start_position),end_position=max(end_position),label="NREM",length=sum(length),complete=TRUE),by='subject_code,activity_or_bedrest_episode,inter_burst_id'] 
  
  # b. After REM episode (260)
  to_collapse <- sequences[inter_burst_id!=0 & inter_burst_id != total_ibi & (above_threshold | has_first_rem) & (start_position > last_rem_position)]
  collapsed$post_rem <- to_collapse[,data.table(start_position=min(start_position),end_position=max(end_position),label="NREM",length=sum(length),complete=TRUE),by='subject_code,activity_or_bedrest_episode,inter_burst_id'] 
  
  ## The head:
  # has 1st REM:
  to_collapse <- sequences[inter_burst_id==0 & has_first_rem & start_position >= first_rem_position & end_position <= last_rem_position]
  collapsed$head_rem <- to_collapse[, data.table(start_position=min(start_position),end_position=max(end_position),label="REM",length=sum(length),complete=TRUE),by='subject_code,activity_or_bedrest_episode,inter_burst_id'] 
  
  
  # post-REM
  to_collapse <- sequences[inter_burst_id==0 & has_first_rem & (start_position > last_rem_position)]
  collapsed$head_post_rem <- to_collapse[,data.table(start_position=min(start_position),end_position=max(end_position),label="NREM",length=sum(length),complete=TRUE),by='subject_code,activity_or_bedrest_episode,inter_burst_id'] 
  
  # Has no 1st REM:
  to_collapse <- sequences[inter_burst_id==0 & !has_first_rem]
  first_nrem_lookup <- to_collapse[label=="NREM",data.table(first_nrem_position=min(start_position)),by='subject_code,activity_or_bedrest_episode,inter_burst_id']
  to_collapse <- merge(to_collapse,first_nrem_lookup,by=c('subject_code','activity_or_bedrest_episode','inter_burst_id'),all.x=TRUE)
  
  collapsed$head_nrem <- to_collapse[!is.na(first_nrem_position),data.table(start_position=min(first_nrem_position),end_position=max(end_position),label="NREM",length=(max(end_position)-min(first_nrem_position)+1),complete=TRUE),by='subject_code,activity_or_bedrest_episode,inter_burst_id'] 
  
  
  ## The tail:
  # 247
  # Below Threshold w/ no REM
  to_collapse <- sequences[inter_burst_id==total_ibi & cumulative_rem == 0]
  collapsed$tail_no_rem <- to_collapse[label == 'NREM',data.table(start_position=min(start_position),end_position=max(end_position),label="NREM",length=(max(end_position)-min(start_position)+1),complete=FALSE),by='subject_code,activity_or_bedrest_episode,inter_burst_id']
  
  # Below Threshold w/ REM
  to_collapse <- sequences[inter_burst_id==total_ibi & cumulative_rem > 0 & !above_threshold]
  collapsed$tail_some_rem <- to_collapse[label == 'NREM',data.table(start_position=min(start_position),end_position=min(first_rem_position-1),label="NREM",length=(min(first_rem_position)-min(start_position)),complete=FALSE),by='subject_code,activity_or_bedrest_episode,inter_burst_id']
  
  # Above Threshold
  to_collapse <- sequences[inter_burst_id==total_ibi & above_threshold & (end_position < first_rem_position)]
  collapsed$tail_pre_rem <- to_collapse[label == 'NREM',data.table(start_position=min(start_position),end_position=max(first_rem_position-1),label="NREM",length=(max(end_position)-min(start_position)+1),complete=TRUE),by='subject_code,activity_or_bedrest_episode,inter_burst_id']
  
  to_collapse <- sequences[inter_burst_id==total_ibi & above_threshold & start_position >= first_rem_position & end_position <= last_rem_position]
  collapsed$tail_rem <-to_collapse[,data.table(start_position=min(start_position),end_position=max(end_position),label="REM",length=sum(length),complete=NA),by='subject_code,activity_or_bedrest_episode,inter_burst_id'] 
  
  to_collapse <- sequences[inter_burst_id==total_ibi & above_threshold & (start_position > last_rem_position) & label == "NREM"]
  collapsed$tail_post_rem <- to_collapse[label == 'NREM',data.table(start_position=min(last_rem_position+1),end_position=max(end_position),label="NREM",length=(max(end_position)-min(start_position)+1),complete=FALSE,cumulative_nrem=sum(length)),by='subject_code,activity_or_bedrest_episode,inter_burst_id']
  
  
    
  episodes <- rbindlist(collapsed, use.names=TRUE, fill=TRUE)
  setkey(episodes,subject_code,activity_or_bedrest_episode,start_position)
  
  # Clean up NA complete variable
  rem_complete_lookup <- episodes[is.na(complete) | !is.na(cumulative_nrem),data.table(complete=(.N != 1) | (max(cumulative_nrem,na.rm=TRUE) > (completion_cutoff/e_length))),by='subject_code,activity_or_bedrest_episode,inter_burst_id']
  episodes[is.na(complete),complete:=rem_complete_lookup$complete]
  
  # Merge same label neighbors
  episodes <- episodes[,merge_same_neighbors(.SD),by='subject_code,activity_or_bedrest_episode']
  episodes[,method:="classic"]
  episodes
}

merge_same_neighbors <- function(dt) {
  
  
  n <- nrow(dt)
  y <- (dt$label[-1L] != dt$label[-n])
  i <- c(which(y | is.na(y)), n)
  
  diffs <- diff(c(0L, i))
  values <- dt$label[i]
  
  result <- copy(dt)
  result[,group:=rep(seq(1,length(values)), diffs)]
  result <- result[,list(start_position=min(start_position), end_position=max(end_position), length=sum(length),complete=any(complete)), by='group']
  result[,label:=values]
  result[,group:=NULL]
  
  result
}

set_inter_burst_intervals <- function(by, sd) {
  burst_positions <- sd[(nrem_burst)]$pik
  burst_ids <- sd[(nrem_burst)]$burst_id
  last_pik <- max(sd$pik)
  
  if(length(burst_positions)==0) {
    burst_ids <- c(0L)
    burst_positions <- c(1L, as.integer(last_pik+1))
  } else {
    # Correction for pre-first burst
    if(burst_positions[[1]] > 1) {
      burst_positions <- c(1L,burst_positions)
      burst_ids <- c(0L, burst_ids)
    }
    
    # Correction for post-last burst
    burst_positions <- c(burst_positions,as.integer(last_pik+1))
  }
  
  rep.int(burst_ids,diff(burst_positions))
}
