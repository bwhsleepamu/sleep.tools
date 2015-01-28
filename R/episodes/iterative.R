##################
## Iterative
##################
generate.episodes.iterative <- function(dt, wake=TRUE, undef=FALSE, min_nrem_length=NULL, min_rem_length=NULL, min_wake_length=NULL) {
  # Take series of epochs and collapse them into sequences of the same type  
  sequences <- dt[, chunk(epoch_type, pk), by='subject_code,activity_or_bedrest_episode']
  
  if(!undef)
    sequences <- remove.target.label.dt(sequences, target_label="UNDEF")  
  if(!wake)
    sequences <- remove.target.label.dt(sequences, target_label="WAKE")
  
  episodes <- iterative_merge(sequences)
  
  episodes[,method:='iterative']
  
  episodes
}
