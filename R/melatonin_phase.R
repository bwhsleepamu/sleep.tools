melatonin_phase <- subjects[,list(subject_code, labtime=Mel.Comp.Max, tau=Mel.Tau, start_analysis=Start.analysis, end_analysis=End.Analysis)]

melatonin_phase <- melatonin_phase[,list(labtime=predict_phase(labtime, tau, end_analysis)),by='subject_code']

# 

predict_phase <- function(start_phase, tau, max_labtime) {
  #print(paste(start_phase,tau,max_labtime, sep=" | "))
  if(!is.na(start_phase) & !is.na(tau) & !is.na(max_labtime) & max_labtime > start_phase) {
    seq(from=start_phase, to=max_labtime, by=tau)
  } else {
    double(0L)
  }
    
}