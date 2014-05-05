source('R/sleep.tools.R')



df <- load_sleep_file("data/AMU/28J8XSlp.01.csv")
changepoint_bouts <- bouts.changepoint(df)
classic_bouts <- bouts.classic(df)

