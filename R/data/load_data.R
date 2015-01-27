## Methods
# Load epochs for a given subject list
load_sleep_data <- function(subjects) {
  sleep_data <- rbindlist(lapply(subjects$file_path, load_sleep_file.dt))
  setnames(sleep_data, c('subject_code', 'activity_or_bedrest_episode', 'labtime', 'stage'))
  setkey(sleep_data, subject_code, labtime)
  # Generate row indeces
  sleep_data[, pk:=.I]
  # Map stages to epoch types
  sleep_data[,epoch_type:=as.factor(as.character(lapply(stage, map_epoch_type))),]
  sleep_data
}

# Read Subject Info
read.subject_info <- function(file_path) {
  subjects <- fread(file_path)
  subjects[,file_exists:=file.exists(file_path)]
  setkey(subjects, subject_code)
  
  subjects
}

# Load a single sleep file
load_sleep_file.dt <- function(file_path) {  
  use <- TRUE
  results <- data.table(character())
  
  if(file.exists(file_path)) {
    tryCatch(
      results<-fread(file_path), 
      error = function(e) { print(paste("ERROR!", file_path)); use <- FALSE }, 
      warning = function(w) {print(paste("WARNING!", file_path)); use <- FALSE}, 
      finally = {
        if(ncol(results) == 4)
          print(paste(file_path, ncol(results)))
        else
          use <- FALSE
      }
    )
    
  }
  
  if(use) {
    results
  } else {
    NULL
  }
  
}


## Environment Setup
# Load Subject Groups
subjects.local <- read.subject_info(subject_fp.local)
subjects.all <- read.subject_info(subject_fp.all)
subjects.subset <- subjects.all[study %in% c('NIAPPG', 'T20CSR-Control', 'T20CSR-CSR')]

# Select main subject group
subjects <- subjects.all

# Load and set up data for subject group
sleep_data <- load_sleep_data(subjects)

# Load and set up sleep stats
sleep_stats <- as.data.table(read.csv("/home/pwm4/Desktop/sleep_stats.csv"))
sleep_stats[,Sleep.Efficiency:=as.numeric(as.character(Sleep.Efficiency))]
setnames(sleep_stats, c("Subject", "SPn"), c("subject_code", "activity_or_bedrest_episode"))
setkey(sleep_stats, subject_code, activity_or_bedrest_episode)

sleep_efficiency_cutoffs <- quantile(sleep_stats$Sleep.Efficiency, c(.20, .40, .60, .80), na.rm=TRUE)
labs <- c(names(sleep_efficiency_cutoffs), "100%")
sleep_efficiency_cutoffs <- c(0.00, sleep_efficiency_cutoffs, 100.00)
sleep_efficiency_cutoffs <- as.data.table(list(label=labs, start=sleep_efficiency_cutoffs[-length(sleep_efficiency_cutoffs)], end=sleep_efficiency_cutoffs[-1L]))
label_indeces <- sleep_efficiency_cutoffs[,which(sleep_stats$Sleep.Efficiency >= start & sleep_stats$Sleep.Efficiency < end), by=label]
sleep_stats[label_indeces$V1, se_label:=label_indeces$label]#$se_label <- 
sleep_efficiency <- sleep_stats[,list(subject_code, activity_or_bedrest_episode,se_label)]
sleep_efficiency <- sleep_efficiency[!is.na(se_label)]

sleep_efficiency <- sleep_efficiency[!J('1215H', 19)]
sleep_efficiency <- sleep_efficiency[!J('2823GX', 31)]

setkey(sleep_efficiency, subject_code, activity_or_bedrest_episode)


# Get FD times
fd_times <- subjects[, list(subject_code, start_analysis, end_analysis)]
fd_times <- fd_times[!is.null(subject_code) & !is.na(start_analysis) & !is.na(end_analysis)]

setkey(fd_times, subject_code)
