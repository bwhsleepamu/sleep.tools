

load_sleep_files.dt <- function(subjects) {
  subjects[, file_path], .aaplpply
  dt <- fread(file_path)
  setnames(dt, c("subject_code", "sleep_wake_period", "labtime", "stage"))
  setkey(dt, sleep_wake_period, labtime)
  dt <- set_up_data_table(dt, T_CYCLE)
  
  min_day_number <- min(dt[,original_day_number])
  
  list(dt=dt, min_day_number=min_day_number)
}