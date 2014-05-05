source('R/sleep.tools.R')

s23D8HS <- load_sleep_file("data/AMU/23D8H/Sleep/23D8HSSlp.01.csv")
s2632DX <- load_sleep_file("data/AMU/2632DX/Sleep/2632DXSlp.01.csv")
s28J8X <- load_sleep_file("data/AMU/28J8X/Sleep/28J8XSlp.01.csv")
s3335GX <- load_sleep_file("data/AMU/3335GX/Sleep/3335GXSlp.01.csv")

main_f <- function(sf) {
  df <- sf$df
  min_day_number <- sf$min_day_number
  
  changepoint_bouts <- bouts.changepoint(df)
  classic_bouts <- bouts.classic(df)
  
  classic_bouts <- setup_bouts_for_raster(classic_bouts, min_day_number, T_CYCLE)
  changepoint_bouts <- setup_bouts_for_raster(changepoint_bouts, min_day_number, T_CYCLE)
  
  
  plot.bouts(df, changepoint_bouts, classic_bouts)  
}



