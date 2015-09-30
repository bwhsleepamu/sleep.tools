source("R/sources.R")
#source("R/plotting/rasters/raster_plot.R")
#source("R/analysis/analysis.R")


subjects <- as.data.table(read.xls("/I/Projects/Forced Desynchrony data projects/FD-info 2015a.xls"))
setnames(subjects, c("Subject", "Age.Group", "Study"), c("subject_code", "age_group", "study"))
subjects[,file_path:=paste("/I/AMU Cleaned Data Sets/", subject_code, "/Sleep/", subject_code, "Slp.01.csv", sep="")]
subjects[,X:=NULL]
allowed_subject_codes <- as.character(subjects[grep('Y', get('Permission.'))]$subject_code)

load_data(subjects = subjects)

setup_episodes(sleep_data, sleep_data, types=c("raw", "trad"))
setup_cycles(sleep_data, episodes)
setup_raster_data(sleep_data, episodes, cycles)

plot_raster("20A4DX", number_of_days = 6)

subjects
View(subjects)
