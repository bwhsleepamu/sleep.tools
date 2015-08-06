source("R/sources.R")
source("R/plotting/rasters/jonathan_raster_plot.R")
source("R/analysis/analysis.R")

jonathan_subject_codes <- fread("data/jonathan_subject_list.csv")$Subject
jonathan_data <- fread("data/18b2xx_NREM_AUC_Fitted.csv")
setnames(jonathan_data, c("subject_code","activity_or_bedrest_episode", "data_type", "labtime", "value"))
jonathan_data[,subject_code:='18B2XX']

load_data(local=FALSE, subject_list=jonathan_subject_codes)


setup_episodes(sleep_data, sleep_data)
setup_cycles(sleep_data, episodes)
setup_raster_data(sleep_data, episodes, cycles, jonathan_data)

p <- plot_raster('20A4DX')
ggsave(plot=r[[1]], file="/home/pwm4/Desktop/test.svg", height=20, width=6, scale=2, limitsize=FALSE)

r <- lapply(jonathan_subject_codes, function(x) {
  p <-plot_raster(x, first_day=1)
  #ggsave(plot=p, file=paste("/home/pwm4/Desktop/jonathan_rasters/", x, '.svg', sep=''), height=30, width=10, scale=2, limitsize=FALSE)
  p
})

for(p in r) {
  ggsave(plot=p, file=paste("/home/pwm4/Desktop/", p$data$subject_code[[1]], ".svg", sep=''), height=20, width=6, scale=2, limitsize=FALSE)
}

for_jon <- copy(episodes[method=='iterative' & label=='NREM'])
for_jon[,`:=`(label=NULL, start_position=NULL, end_position=NULL, method=NULL)]
for_jon[,nrem_episode_number:=1:.N,by='subject_code,activity_or_bedrest_episode']

