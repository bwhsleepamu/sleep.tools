source("R/sources.R")
source("R/plotting/rasters/jonathan_raster_plot.R")
#source("R/analysis/analysis.R")

jonathan_subject_codes <- fread("data/jonathan_subject_list.csv")$Subject

jonathan_data <- list()
jonathan_data$a <- fread("data/18b2xx_NREM_AUC_Fitted.csv")
setnames(jonathan_data$a, c("subject_code","activity_or_bedrest_episode", "data_type", "labtime", "value"))
jonathan_data$a[,subject_code:='18B2XX']

jonathan_data$b <- fread("data/18bexx_NREM_AUC_Fitted.csv")
setnames(jonathan_data$b, c("subject_code","activity_or_bedrest_episode", "data_type", "labtime", "value"))
jonathan_data$b[,subject_code:='18E3XX']

jonathan_data$c <- fread("data/20a4dx_NREM_AUC_Fitted.csv")
setnames(jonathan_data$c, c("subject_code","activity_or_bedrest_episode", "data_type", "labtime", "value"))
jonathan_data$c[,subject_code:='20A4DX']

jonathan_data$d <- fread("data/20C1DX_NREM_AUC_Fitted.csv")
setnames(jonathan_data$d, c("subject_code","activity_or_bedrest_episode", "data_type", "labtime", "value"))
jonathan_data$d[,subject_code:='20C1DX']

jonathan_data <- rbindlist(jonathan_data)

load_data(local=FALSE, subject_list=jonathan_subject_codes)

jonathan_subject_codes <- unique(jonathan_data$subject_code)
sleep_data <- sleep_data[subject_code %in% jonathan_subject_codes]

setup_episodes(sleep_data, sleep_data)
setup_cycles(sleep_data, episodes)

setup_raster_data(sleep_data, episodes, cycles, jonathan_data)
p <- plot_raster('18B2XX', first_day = 1, number_of_days = 5)
p

jepisodes[,mean(length),by='method,label']

episodes[method=="iterative"]

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
for_jon[,`:=`(label=NULL, start_position=NULL, end_position=NULL, method=NULL, complete=NULL)]
for_jon[,nrem_episode_number:=1:.N,by='subject_code,activity_or_bedrest_episode']

st <- fread("~/Desktop/jonathan/NREM_episodes_20150728_2_full.csv")
sleep_episode_times <- st[,data.table(sleep_episode_start_time=min(sleep_episode_start_time),sleep_episode_end_time=min(sleep_episode_end_time)),by='subject_code,activity_or_bedrest_episode']

sleep_episode_times

output <- merge(for_jon,sleep_episode_times,all.x=TRUE,by=c('subject_code','activity_or_bedrest_episode'))
write.table(output,file="~/Desktop/jonathan/NREM_episodes_extended_20150813.csv",row.names = FALSE,sep=',')

