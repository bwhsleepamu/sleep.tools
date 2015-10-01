source("R/main/libraries.R")
source("R/helpers.R")

EPOCH_LENGTH <- 1/60
EPOCH_SECONDS <- 60
T_CYCLE <- 24.0

DOUBLE_PLOT <- TRUE
SUBJECT_CODE <- "TEST0"
NUMBER_OF_DAYS <- NA
FIRST_DAY <- 1
COLOR_PALETTE <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#F0E442", "#888888", "#000000", "#D55E00",  "#CC79A7", "#555555")

main_data <<- as.data.table(read.csv("~/Desktop/cpss/main_raster_data_23.90.csv"))
cbt_data <<- as.data.table(read.csv("~/Desktop/cpss/cbt_min_23.90.csv"))

setup_raster_data()
plot_raster()
