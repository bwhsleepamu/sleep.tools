melatonin_phase <- subjects[,list(subject_code, labtime=Mel.Comp.Max, tau=Mel.Tau, start_analysis=Start.analysis, end_analysis=End.Analysis)]
melatonin_phase <- melatonin_phase[,list(labtime=predict_phase(labtime, tau, end_analysis)),by='subject_code']

# For each sleep episode of a subject, find closest melatonin maximum
sleep_episodes[,midpoint:=((end_labtime-start_labtime)/2.0)+start_labtime]
sleep_episodes

setkey(melatonin_phase, subject_code)

sleep_episodes[,closest_phase_estimate:=find_closest_phase_estimate(midpoint, melatonin_phase[subject_code]$labtime), by='subject_code,midpoint']
sleep_episodes[,phase_diff_midpoint:=midpoint-closest_phase_estimate]
sleep_episodes[,phase_diff_start:=start_labtime-closest_phase_estimate]
sleep_episodes[,phase_diff_end:=end_labtime-closest_phase_estimate]

sleep_episodes[abs(phase_diff_midpoint) <= 24, phase_label:="neither"]
sleep_episodes[abs(phase_diff_midpoint) > 24 | is.na(phase_diff_midpoint), phase_label:=NA]
sleep_episodes[abs(phase_diff_midpoint) <= 14 & abs(phase_diff_midpoint) >= 10, phase_label:="out_of_phase"]
sleep_episodes[abs(phase_diff_midpoint) <= 2, phase_label:="in_phase"]

View(sleep_episodes[!is.na(phase_label)])

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

# Assume mel max is middle of night
# This means in phase: <2 hours
# Out of phase from 10 to 14 hours