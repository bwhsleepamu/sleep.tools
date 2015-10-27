source("R/sources.R")
source("R/plotting/rasters/swa_raster_plot.R")

#data_location <- "/X/Studies/Analyses/McHill-Shaw SWA/Subjects"
#data_location <- "/X/Studies/Analyses/McHill_Glucose SWA/For JDs Program"

# Beth and Andrew Notes
#skipped_first_rem <- c("103_1","131_1","1516_1","310_1","33_1","33_2","912_1","B05271999_2","B11092004_1","D04072006_1","D07252001_2","D10012007_1","F03102000_2","H09282005_2","J0642000_2","K01062006_1","M02011998_2","M10202005_1","N10062000_2","O06142000_2","P07232001_2","R11032005_1","S05161999_2","S06101999_2","W06031999_2")
#do_not_use <- c("H09282005_1", "K11102003_1", "S10272004_1", "L11182005_1")

function(data_location){
  # Location of input files
  data_location <- list()
  data_location$disrupt <- "/X/Studies/Analyses/McHill-Shaw SWA/For PM_15-10-20/For PM_Disrupt/"
  data_location$precoc_pub <- "/X/Studies/Analyses/McHill-Shaw SWA/For PM_15-10-20/For PM_Precoc Pub/"
  
  # Set up subject table
  subjects <- data.table(study="precocious_puberty", file_path=list.files(path=data_location$precoc_pub, pattern='.detail.spectral.xls', recursive=FALSE, full.names=TRUE))
  subjects <- rbind(subjects, data.table(study="disrupt", file_path=list.files(path=data_location$disrupt, pattern='.detail.spectral.xls', recursive=FALSE, full.names=TRUE)))
  subjects[,subject_code:=str_match(basename(file_path), "^([[:alnum:]]*)[[:punct:][:space:]]")[,2]]
  setkey(subjects, subject_code)
  
  # Load and merge data from input files
  data_list <- list()
  t <- subjects[,{
    dt <- as.data.table(read.xlsx(file_path,startRow=2));
    dt[,`:=`(subject_code=subject_code, study=study, file_path=file_path)];
    data_list[[paste(subject_code,file_path)]] <<- dt;
    0
  },by='subject_code,study,file_path']
  full_sleep_data <- rbindlist(data_list)
  
  # Clean up and add columns
  full_sleep_data <- full_sleep_data[,c(112:114,1:54,106:111), with=FALSE]
  full_sleep_data[, activity_or_bedrest_episode:=as.numeric(as.factor(file_path)),by='subject_code']
  full_sleep_data[, subject_code:=paste(subject_code, activity_or_bedrest_episode, sep="_")]
  full_sleep_data[, activity_or_bedrest_episode:=1]
  full_sleep_data[,labtime:=(24*(activity_or_bedrest_episode-1))+(0:(.N-1))*30/3600,by='subject_code,file_path']
  full_sleep_data[,pk:=.I]
  full_sleep_data[,stage:=map_numHypno_to_stage(numHypno),by='pk']
  full_sleep_data[,epoch_type:=as.factor(as.character(lapply(stage, map_epoch_type))),]
  
  # Sum delta power
  cols_to_sum <- c("0.50.Hz", "1.00.Hz", "1.50.Hz", "2.00.Hz","2.50.Hz", "3.00.Hz", "3.50.Hz","4.00.Hz")
  full_sleep_data[, delta_power:=sum(.SD), .SDcols=cols_to_sum, by='pk']
  
  # Fix column order
  setcolorder(full_sleep_data,c(1:3, 65, 64, 67:68, 69, 66, 4:63))
  
  # Insert simulated REM episodes
  insert_skipped_first_rem(full_sleep_data)
  
  # Grab subset of columns
  sleep_data <- full_sleep_data[,1:9,with=FALSE]
  
  # Setup sleep episodes
  setup_episodes(sleep_data, sleep_data)
  
  # Merge nrem episodes with sleep data
  nrem_episode_output <- copy(episodes[method=='iterative' & label=='NREM'])
  nrem_episode_output[,nrem_episode_number:=1:.N,by='subject_code,activity_or_bedrest_episode']
  setkey(nrem_episode_output,subject_code,activity_or_bedrest_episode,nrem_episode_number)
  sleep_data[,nrem_episode_number:=nrem_episode_output[subject_code==.SD$subject_code & activity_or_bedrest_episode==.SD$activity_or_bedrest_episode & pk>=start_position & pk <= end_position]$nrem_episode_number,by='pk']
  full_sleep_data[,nrem_episode_number:=nrem_episode_output[subject_code==.SD$subject_code & activity_or_bedrest_episode==.SD$activity_or_bedrest_episode & pk>=start_position & pk <= end_position]$nrem_episode_number,by='pk']
  
  # Sum delta power in each episode
  total_delta_powers <- sleep_data[!is.na(nrem_episode_number),list(total_delta_power=sum(.SD[epoch_type!="WAKE"]$delta_power)),by='subject_code,activity_or_bedrest_episode,nrem_episode_number']
  total_delta_powers <- merge(total_delta_powers,nrem_episode_output,by=c('subject_code','activity_or_bedrest_episode','nrem_episode_number'),all.x=TRUE,all.y=FALSE)
  total_delta_powers[,labtime:=start_labtime+((end_labtime-start_labtime)/2)]
  
  # Setup data for plotting rasters
  setup_raster_data(sleep_data, episodes, total_delta_powers)
  
  # Plot individual rasters
  plot_list <- list()
  setkey(subjects, subject_code)
  for(st in unique(sleep_data$study)) {
    plot_list[[st]] <- list()
    for(sc in unique(sleep_data[study==st]$subject_code)) {
      for(abe in unique(sleep_data[subject_code==sc]$activity_or_bedrest_episode)) {
        max_hour <- max(sleep_data[subject_code == sc & activity_or_bedrest_episode == abe]$labtime) - (24.0 * (abe-1))
        #print(max_hour)
        
        plot_list[[st]][[paste(sc,abe,sep='_')]] <- plot_swa_raster(sc, activity_or_bedrest_episodes = c(abe), hour_range = c(0,max_hour))
      }
    }
  }

  for(s in plot_list) {
    for(p in s) {
      ggsave(plot=p, file=paste("/home/pwm4/Desktop/swa_rasters/", p$data$study[[1]], "/", p$data$subject_code[[1]], ".svg", sep=''), height=3, width=8, scale=2, limitsize=FALSE)
    }
  }
  
  # Create single file with rasters
  marrangeGrob(plot_list[[1]], ncol=1, nrow=length(plot_list[[1]]))
  marrangeGrob(plot_list[[2]], ncol=1, nrow=length(plot_list[[2]]))
  
  
  # Output NREM episodes
  nrem_episodes.out <- copy(nrem_episode_output)
  nrem_episodes.out[,`:=`(start_position=NULL, end_position=NULL, method=NULL, complete=NULL,label=NULL)]
  nrem_episodes.out[,midpoint:=start_labtime+(end_labtime-start_labtime)/2]
  write.csv(nrem_episodes.out, file='/home/pwm4/Desktop/nrem_episodes_20151027.csv', row.names=FALSE, na="")
  
  # Output Sleep Data
  full_sleep_data.out <- copy(full_sleep_data)
  full_sleep_data.out[,pk:=NULL]
  full_sleep_data.out[,file_path:=str_replace(file_path, "/home/pwm4/Desktop/SWA", "")]
  write.csv(sleep_data.out, file='/home/pwm4/Desktop/merged_full_sleep_data_20151027.csv', row.names=FALSE, na="")
  
  sleep_data.out <- copy(sleep_data)
  sleep_data.out[,pk:=NULL]  
  sleep_data.out[,file_path:=str_replace(file_path, "/home/pwm4/Desktop/SWA", "")]
  write.csv(sleep_data.out, file='/home/pwm4/Desktop/merged_sleep_data_20151027.csv', row.names=FALSE, na="")
  
  # Output Delta Power
  total_delta_powers.out <- copy(total_delta_powers)
  total_delta_powers.out[,`:=`(start_position=NULL, end_position=NULL, complete=NULL, method=NULL, label=NULL)]
  write.csv(total_delta_powers.out, file='/home/pwm4/Desktop/total_delta_power_20151027.csv', row.names=FALSE, na="")
}

## Insertion of Skipped First REM
insert_skipped_first_rem <- function(sleep_data) {
  #View(sleep_data[subject_code == "33_2" & nrem_episode_number == 1])
  
  # 33_1: 2nd WAKE around 2.00 
  # 6336: 2.0083333
  sleep_data[pk==6336, `:=`(stage=16, epoch_type="SREM")]
  
  # 33_2: min Delta in NREM2 section
  # 7487: 1.4https://www.facebook.com/joeschwarcz/posts/10154263960630744?fref=nf
  #min_delta <- min(sleep_data[subject_code == "33_2" & nrem_episode_number == 1 & labtime > 1.1 & labtime < 1.7]$delta_power)
  #sleep_data[subject_code == "33_2" & nrem_episode_number == 1 & delta_power == min_delta]
  sleep_data[pk==7487, `:=`(stage=16, epoch_type="SREM")]

  # 103_1: 1st WAKE around 2.0
  # 276: 2.291667
  #sleep_data[subject_code == "103_1" & nrem_episode_number == 1 & epoch_type=="WAKE"]
  sleep_data[pk==276, `:=`(stage=16, epoch_type="SREM")]
  
  # 131_1: 1st WAKE around 2.0
  # 1369: 1.9000000
  #sleep_data[subject_code == "131_1" & nrem_episode_number == 1 & epoch_type=="WAKE"]
  sleep_data[pk==1388, `:=`(stage=16, epoch_type="SREM")]
  
  # 310_1: 1st Wake around 1.5
  # 5167: 1.583333
  #sleep_data[subject_code == "310_1" & nrem_episode_number == 1 & epoch_type=="WAKE"]
  sleep_data[pk==5167, `:=`(stage=16, epoch_type="SREM")]

  # 912_1: 1st Wake around 2.5
  # 8782: 2.433333333
  #sleep_data[subject_code == "912_1" & nrem_episode_number == 1 & epoch_type=="WAKE"]
  sleep_data[pk==8782, `:=`(stage=16, epoch_type="SREM")]
  
  # ** UNCLEAR PLACEMENT 
  # 1516_1: 1st Wake around 2
  # 2454: 2.108333
  #sleep_data[subject_code == "1516_1" & nrem_episode_number == 1 & epoch_type=="WAKE"]
  sleep_data[pk==2454, `:=`(stage=16, epoch_type="SREM")]
  
  # B05271999_2: min Delta in NREM2 section around 3.0
  # 12183: 3.2
  #min_delta <- min(sleep_data[subject_code == "B05271999_2" & nrem_episode_number == 1 & labtime > 3 & labtime < 4]$delta_power)
  #sleep_data[subject_code == "B05271999_2" & nrem_episode_number == 1 & delta_power == min_delta]
  sleep_data[pk==12183, `:=`(stage=16, epoch_type="SREM")]

  # B11092004_1: min Delta in NREM2 section around 2.0
  # 13859: 2.183333
  #min_delta <- min(sleep_data[subject_code == "B11092004_1" & nrem_episode_number == 1 & labtime > 2 & labtime < 2.7]$delta_power)
  #sleep_data[subject_code == "B11092004_1" & nrem_episode_number == 1 & delta_power == min_delta]
  sleep_data[pk==13859, `:=`(stage=16, epoch_type="SREM")]
  
  # D04072006_1: min Delta in NREM2 section around 1.8
  # 17277: 1.7
  #min_delta <- min(sleep_data[subject_code == "D04072006_1" & nrem_episode_number == 1 & labtime > 1.5 & labtime < 1.9]$delta_power)
  #sleep_data[subject_code == "D04072006_1" & nrem_episode_number == 1 & delta_power == min_delta]
  sleep_data[pk==17277, `:=`(stage=16, epoch_type="SREM")]
  
  # ** UNCLEAR PLACEMENT
  # D07252001_2: DISRUPTION? no clear division...
  #sleep_data[subject_code == "D07252001_2" & nrem_episode_number == 1 & epoch_type=="WAKE"]
  sleep_data[pk==21381, `:=`(stage=16, epoch_type="SREM")]
  
  # ** UNCLEAR PLACEMENT 
  # D10012007_1: min Delta in NREM2 section around 3
  # 22626: 2.825
  #min_delta <- min(sleep_data[subject_code == "D10012007_1" & nrem_episode_number == 1 & labtime > 2.8 & labtime < 3.5]$delta_power)
  #sleep_data[subject_code == "D10012007_1" & nrem_episode_number == 1 & delta_power == min_delta]
  sleep_data[pk==22626, `:=`(stage=16, epoch_type="SREM")]  
  
  # F03102000_2: 1st Wake around 1.8
  # 25017: 1.833333
  #sleep_data[subject_code == "F03102000_2" & nrem_episode_number == 1 & epoch_type=="WAKE"]
  sleep_data[pk==25017, `:=`(stage=16, epoch_type="SREM")]
  
  # K01062006_1: 1st Wake around 2
  # 32498: 2.233333data_location <- list()
  data_location$disrupt <- "/X/Studies/Analyses/McHill-Shaw SWA/For PM_15-10-20/For PM_Disrupt/"
  data_location$precoc_pub <- "/X/Studies/Analyses/McHill-Shaw SWA/For PM_15-10-20/For PM_Precoc Pub/"
  data_location <- list()
  data_location$disrupt <- "/X/Studies/Analyses/McHill-Shaw SWA/For PM_15-10-20/For PM_Disrupt/"
  data_location$precoc_pub <- "/X/Studies/Analyses/McHill-Shaw SWA/For PM_15-10-20/For PM_Precoc Pub/"
  
  #sleep_data[subject_code == "K01062006_1" & nrem_episode_number == 1 & epoch_type=="WAKE"]
  sleep_data[pk==32498, `:=`(stage=16, epoch_type="SREM")]
 
  # ** UNCLEAR PLACEMENT - two?
  # M02011998_2: min Delta in NREM3 section around 1
  # 37472: 0.9583333
  #min_delta <- min(sleep_data[subject_code == "M02011998_2" & nrem_episode_number == 1 & labtime > 0.8 & labtime < 1.3]$delta_power)
  #sleep_data[subject_code == "M02011998_2" & nrem_episode_number == 1 & delta_power == min_delta]
  sleep_data[pk==37472, `:=`(stage=16, epoch_type="SREM")]  
  
  # ** UNCLEAR PLACEMENT 
  # M10202005_1: 1st Wake around 2
  # 38582: 1.791666667
  #sleep_data[subject_code == "M10202005_1" & nrem_episode_number == 1 & epoch_type=="WAKE"]
  sleep_data[pk==38582, `:=`(stage=16, epoch_type="SREM")]
  
  # N10062000_2: min Delta in NREM2 section around 2
  # 40741: 2.008333
  #min_delta <- min(sleep_data[subject_code == "N10062000_2" & nrem_episode_number == 1 & labtime > 1 & labtime < 2.5]$delta_power)
  #sleep_data[subject_code == "N10062000_2" & nrem_episode_number == 1 & delta_power == min_delta]
  sleep_data[pk==40741, `:=`(stage=16, epoch_type="SREM")]  
  
  # ** UNCLEAR PLACEMENT 
  # O06142000_2: min Delta in first trough
  # 42739: 1.808333
  #min_delta <- min(sleep_data[subject_code == "O06142000_2" & nrem_episode_number == 1 & labtime > 1.5 & labtime < 2]$delta_power)
  #sleep_data[subject_code == "O06142000_2" & nrem_episode_number == 1 & delta_power == min_delta]
  sleep_data[pk==42739, `:=`(stage=16, epoch_type="SREM")]  
  
  
  # ** UNCLEAR PLACEMENT 
  # P07232001_2: 1st Wake around 1.7
  # 45897: 1.8166667
  #sleep_data[subject_code == "P07232001_2" & nrem_episode_number == 1 & epoch_type=="WAKE"]
  sleep_data[pk==45897, `:=`(stage=16, epoch_type="SREM")]
  
  # ** NO PLACEMENT 
  # R11032005_1: DISRUPTION? no clear div
  #sleep_data[subject_code == "R11032005_1" & nrem_episode_number == 1 & epoch_type=="WAKE"]
  sleep_data[pk==47022, `:=`(stage=16, epoch_type="SREM")]
  
  # S05161999_2: 1st Wake around 1.5
  # 49193: 1.508333
  #sleep_data[subject_code == "S05161999_2" & nrem_episode_number == 1 & epoch_type=="WAKE"]
  sleep_data[pk==49193, `:=`(stage=16, epoch_type="SREM")]
  
  # S06101999_2: 2nd Wake around 1.5
  # 51234: 1.7
  #sleep_data[subject_code == "S06101999_2" & nrem_episode_number == 1 & epoch_type=="WAKE"]
  sleep_data[pk==51234, `:=`(stage=16, epoch_type="SREM")]
  
  # ** UNCLEAR PLACEMENT 
  # W06031999_2: Wake around 1.5
  # 57661: 1.3750000
  #sleep_data[subject_code == "W06031999_2" & nrem_episode_number == 1 & epoch_type=="WAKE"]
  sleep_data[pk==57661, `:=`(stage=16, epoch_type="SREM")]
  
  # L11182005v1_1: trough around 3.0
  # 35590: 2.766667
  # min_delta <- min(sleep_data[subject_code == "L11182005v1_1" & nrem_episode_number == 1 & labtime > 2.5 & labtime < 3.5]$delta_power)
  # sleep_data[subject_code == "L11182005v1_1" & nrem_episode_number == 1 & delta_power == min_delta]
  sleep_data[pk==35590, `:=`(stage=16, epoch_type="SREM")]  
  
  # J06042000_2: Wake around 1.8
  # 30705: 1.7833333
  #sleep_data[subject_code == "J06042000_2" & nrem_episode_number == 1 & epoch_type=="WAKE"]
  sleep_data[pk==30705, `:=`(stage=16, epoch_type="SREM")]
}

map_numHypno_to_stage <- function(numHypno) {
  if(numHypno %in% c(1,2,3)){
    numHypno
  } else if (numHypno == 5) {
    6
  } else if (numHypno == 0) {
    5
  } else {
    9
  }
}

