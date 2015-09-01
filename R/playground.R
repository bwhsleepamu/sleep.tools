source("R/sources.R")
source("R/plotting/rasters/raster_plot.R")
source("R/analysis/analysis.R")


fd_info_2015a <- as.data.table(read.xls("/I/Projects/Forced Desynchrony data projects/FD-info 2015a.xls"))



subjects <- read_subject_info(subject_fp.all)
setup_episodes(sleep_data, sleep_data)
setup_cycles(sleep_data, episodes)
setup_raster_data(sleep_data, episodes, cycles)

source("R/plotting/rasters/raster_plot.R")
plot_raster("F03102000", first_day = 1)



