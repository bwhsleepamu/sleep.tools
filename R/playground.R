source("R/sources.R")
source("R/plotting/rasters/raster_plot.R")
source("R/analysis/analysis.R")

load_data(local=FALSE)
setup_episodes(sleep_data, sleep_data)
setup_cycles(sleep_data, episodes)
setup_raster_data(sleep_data, episodes, cycles)

source("R/plotting/rasters/raster_plot.R")
plot_raster("F03102000disrupt", first_day = 1)



