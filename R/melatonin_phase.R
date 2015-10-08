melatonin_phase <- subjects[,list(subject_code, labtime=Mel.Comp.Max, tau=Mel.Tau, start_analysis=Start.analysis, end_analysis=End.Analysis)]

melatonin_phase <- melatonin_phase[,list(labtime=predict_phase(labtime, tau, end_analysis)),by='subject_code']

# For each sleep episode of a subject, find closest melatonin maximum
sleep_episodes[,midpoint:=((end_labtime-start_labtime)/2.0)+start_labtime]
sleep_episodes

setkey(melatonin_phase, subject_code)

sleep_episodes[,closest_phase_estimate:=find_closest_phase_estimate(midpoint, melatonin_phase[subject_code]$labtime), by='subject_code,midpoint']


find_closest_phase_estimate <- function(midpoint, melatonin_labtimes) {
  melatonin_labtimes[which.min(abs(melatonin_labtimes-midpoint))]
  
}
predict_phase <- function(start_phase, tau, max_labtime) {
  #print(paste(start_phase,tau,max_labtime, sep=" | "))
  if(!is.na(start_phase) & !is.na(tau) & !is.na(max_labtime) & max_labtime > start_phase) {
    seq(from=start_phase, to=max_labtime, by=tau)
  } else {
    double(0L)
  }
    
}