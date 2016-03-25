source("R/sources.R")
source("R/plotting/rasters/jonathan_raster_plot.R")
library(gtable)
#source("R/analysis/analysis.R")

jonathan_subject_codes <- as.data.table(read.csv("data/jonathan_subject_list.csv"))$Subject
jonathan_dir <- '/X/People/Students/DeShields_J/Subject Files'

jonathan_data <- lapply(jonathan_subject_codes, function(sc){

  fp <- paste(jonathan_dir, sc, paste(tolower(sc), 'NREM_AUC_Fitted.xls', sep='_'), sep='/')
  if(file.exists(fp)) {
    print(paste("Loading", sc, fp, sep=" | "))
    s_data <- as.data.table(read.xls(fp, header=FALSE))
    setnames(s_data, c("subject_code","activity_or_bedrest_episode", "data_type", "labtime", "value"))
    s_data[,subject_code:=sc]
    s_data
  }
  else {
    print(paste("Could not load", sc, fp, sep=" | "))
    NULL
  }
})

jonathan_data <- rbindlist(jonathan_data)
jonathan_subject_codes <- as.character(unique(jonathan_data$subject_code))

load_data(subject_list=jonathan_subject_codes, subject_fp = subject_fp.all)

setup_episodes(sleep_data, sleep_data)
setup_cycles(sleep_data, episodes)

setup_raster_data(sleep_data, episodes, cycles, jonathan_data)

plot_raster("2844GX", first_day=1,number_of_days = 5)

r <- lapply(jonathan_subject_codes, function(x) {
  p <-plot_raster(x, first_day=1)
  #ggsave(plot=p, file=paste("/home/pwm4/Desktop/jonathan_rasters/", x, '.svg', sep=''), height=30, width=10, scale=2, limitsize=FALSE)
  p
})



for(p in r) {
  ggsave(plot=p, file=paste("/home/pwm4/Desktop/jonathan_rasters/", p$data$subject_code[[1]], ".svg", sep=''), height=40, width=10, scale=2, limitsize=FALSE)
}



###############

for_jon <- copy(episodes[method=='iterative' & label=='NREM'])
for_jon[,`:=`(label=NULL, start_position=NULL, end_position=NULL, method=NULL, complete=NULL)]
for_jon[,nrem_episode_number:=1:.N,by='subject_code,activity_or_bedrest_episode']

st <- fread("~/Desktop/jonathan/NREM_episodes_20150728_2_full.csv")
sleep_episode_times <- st[,data.table(sleep_episode_start_time=min(sleep_episode_start_time),sleep_episode_end_time=min(sleep_episode_end_time)),by='subject_code,activity_or_bedrest_episode']

sleep_episode_times

output <- merge(for_jon,sleep_episode_times,all.x=TRUE,by=c('subject_code','activity_or_bedrest_episode'))
write.table(output,file="~/Desktop/jonathan/NREM_episodes_extended_20150813.csv",row.names = FALSE,sep=',')


###############
p <- plot_raster(jonathan_subject_codes[[3]], first_day = 5, number_of_days = 5)
p

episodes[,mean(length),by='method,label']

episodes[method=="iterative"]

ggsave(plot=r[[1]], file="/home/pwm4/Desktop/test.svg", height=20, width=6, scale=2, limitsize=FALSE)


