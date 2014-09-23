# Load Subject Groups
subjects.local <- read.subject_info(subject_fp.local)
subjects.all <- read.subject_info(subject_fp.all)
subjects.subset <- subjects.all[study %in% c('NIAPPG', 'T20CSR-Control', 'T20CSR-CSR')]

# Select main subject group
subjects <- subjects.all

# Load and set up data for subject group
sleep_data <- load_sleep_data.dt(subjects)
