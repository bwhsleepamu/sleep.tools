source("R/sources.R")
source("R/plotting/rasters/swa_raster_plot.R")

#data_location <- "/X/Studies/Analyses/McHill-Shaw SWA/Subjects"
#data_location <- "/X/Studies/Analyses/McHill_Glucose SWA/For JDs Program"

# Beth and Andrew Notes
#skipped_first_rem <- c("103_1","131_1","1516_1","310_1","33_1","33_2","912_1","B05271999_2","B11092004_1","D04072006_1","D07252001_2","D10012007_1","F03102000_2","H09282005_2","J0642000_2","K01062006_1","M02011998_2","M10202005_1","N10062000_2","O06142000_2","P07232001_2","R11032005_1","S05161999_2","S06101999_2","W06031999_2")
#do_not_use <- c("H09282005_1", "K11102003_1", "S10272004_1", "L11182005_1")

function(){
  # Location of input files
  data_location <- "/media/pwm4/Software/De Shields/EDF files"
  # data_location <- "/X/People/Students/DeShields_J/Subject Files"
  
  # Set up subject table
  subjects <- data.table(study="deshields", file_path=list.files(path=data_location, pattern='.detail.spectral.xls', recursive=TRUE, full.names=TRUE))
  subjects[,subject_code:=toupper(str_match(basename(file_path), "^([[:alnum:]]*)[[:punct:][:space:]]")[,2])]
  subjects[,activity_or_bedrest_episode:=as.numeric(str_match(file_path, "/SP([[:digit:]]+)/")[,2])]
  subjects[,modified_at:=file.info(file_path)$mtime,by='file_path']
  subjects[,created_at:=file.info(file_path)$ctime,by='file_path']
  
  
  setkey(subjects, subject_code, activity_or_bedrest_episode)
  
  # Load and merge data from input files
  data_list <- list()
  
  t <- subjects[,{
    dt <- as.data.table(read.xlsx(file_path,startRow=2));
    dt[,`:=`(subject_code=subject_code, study=study, file_path=file_path, activity_or_bedrest_episode=activity_or_bedrest_episode)];
    data_list[[paste(subject_code,file_path,activity_or_bedrest_episode)]] <<- dt;
    0
  },by='subject_code,study,file_path']
  full_sleep_data <- rbindlist(data_list)
  
  
  # Analyze types of values
  unique_stages <- full_sleep_data[,list(unique_charHypno=paste(unique(charHypno),collapse=','), 
                                         unique_numHypno=paste(unique(numHypno),collapse=','),
                                         has_stage_4=as.character(4 %in% unique(numHypno))),by='file_path']
  unique_stages <- merge(subjects, unique_stages, by='file_path')
  unique_stages[,activity_or_bedrest_episode:=as.numeric(as.character(activity_or_bedrest_episode))]
  setkey(subjects, subject_code, activity_or_bedrest_episode);
  
  write.xlsx(unique_stages, "/home/pwm4/Desktop/unique_stages_harddrive.xlsx")
  
  # Clean up and add columns
  full_sleep_data <- full_sleep_data[,c(112:115,1:54,106:111), with=FALSE]
  full_sleep_data[,labtime:=(24*(activity_or_bedrest_episode-1))+(0:(.N-1))*30/3600,by='subject_code,file_path']
  setkey(full_sleep_data, subject_code, activity_or_bedrest_episode)
  full_sleep_data[,pk:=.I]
  full_sleep_data[,stage:=map_numHypno_to_stage(numHypno),by='pk']
  full_sleep_data[,epoch_type:=as.factor(as.character(lapply(stage, map_epoch_type))),]
  
  # Sum delta power
  cols_to_sum <- c("0.50.Hz", "1.00.Hz", "1.50.Hz", "2.00.Hz","2.50.Hz", "3.00.Hz", "3.50.Hz","4.00.Hz")
  full_sleep_data[, delta_power:=sum(.SD), .SDcols=cols_to_sum, by='pk']
  
  # Add per-subject and per-abepisode pk
  full_sleep_data[, se_pk:=1:.N, by='subject_code,activity_or_bedrest_episode']
  
  # Fix column order
  setcolorder(full_sleep_data,c(1:3, 65, 64, 67:68, 69, 70, 66, 4:63))
  
  # Setup sleep episodes
  setup_episodes(full_sleep_data, full_sleep_data)
  
  # Merge nrem episodes with sleep data
  nrem_episode_output <- copy(episodes[method=='iterative' & label=='NREM'])
  nrem_episode_output[,nrem_episode_number:=1:.N,by='subject_code,activity_or_bedrest_episode']
  setkey(nrem_episode_output,subject_code,activity_or_bedrest_episode,nrem_episode_number)
  #sleep_data[,nrem_episode_number:=nrem_episode_output[subject_code==.SD$subject_code & activity_or_bedrest_episode==.SD$activity_or_bedrest_episode & pk>=start_position & pk <= end_position]$nrem_episode_number,by='pk']
  full_sleep_data[,nrem_episode_number:=nrem_episode_output[subject_code==.SD$subject_code & activity_or_bedrest_episode==.SD$activity_or_bedrest_episode & pk>=start_position & pk <= end_position]$nrem_episode_number,by='pk']
  
  # Sum delta power in each episode
  #full_median <- median(full_sleep_data$delta_power, na.rm=TRUE)
  full_sleep_data[stage %in% c(2,3), delta_power_in_stage_2_3:=delta_power]
  full_sleep_data[,q75:=quantile(delta_power_in_stage_2_3, na.rm=TRUE)[4],by='subject_code']
  full_sleep_data[,iqr:=IQR(delta_power_in_stage_2_3, na.rm=TRUE),by='subject_code']
  
  full_sleep_data[,delta_cutoff:=1.5*iqr+q75,by='subject_code']
  full_sleep_data[delta_power_in_stage_2_3 <= delta_cutoff, delta_power_below_cutoff:=delta_power_in_stage_2_3]
  
  # Max ranges and values
  delta_subject_ranges <- full_sleep_data[,list(q95=quantile(delta_power_in_stage_2_3, probs=c(.95), na.rm=TRUE)[1], q75=quantile(delta_power_in_stage_2_3, na.rm=TRUE)[4],iqr=IQR(delta_power_in_stage_2_3, na.rm=TRUE), min_delta=min(delta_power_in_stage_2_3, na.rm = TRUE), max_delta=max(delta_power_in_stage_2_3, na.rm = TRUE), median=median(delta_power_in_stage_2_3, na.rm = TRUE), cutoff=5*median(delta_power_in_stage_2_3, na.rm = TRUE), mean=5*mean(delta_power_in_stage_2_3, na.rm = TRUE)),by='subject_code']
  delta_abe_ranges <- full_sleep_data[,list(min_delta=min(delta_power_in_stage_2_3, na.rm = TRUE), max_delta=max(delta_power_in_stage_2_3, na.rm = TRUE), median=median(delta_power_in_stage_2_3, na.rm = TRUE), cutoff=5*median(delta_power_in_stage_2_3, na.rm = TRUE), mean=5*mean(delta_power_in_stage_2_3, na.rm = TRUE)),by='subject_code,activity_or_bedrest_episode']
  delta_subject_ranges[,new_cutoff:=1.5*iqr+q75]
  
  delta_subject_ranges[,subject_code:=as.numeric(factor(subject_code, ordered = TRUE))]
  
  # Plotting max ranges/values
  p <- ggplot(data=delta_abe_ranges[subject_code=="26O2GXT2"], mapping = aes(x=activity_or_bedrest_episode, group=subject_code))
  p + geom_line(mapping=aes(y=min_delta), color="red")
  p + geom_line(mapping=aes(y=max_delta), color="blue")
  p + geom_line(mapping=aes(y=median), color="black")
  p + geom_line(mapping=aes(y=mean), color="green")
  
  
  
  p <- ggplot(data=delta_subject_ranges, mapping = aes(x=as.numeric(subject_code)))
  p + geom_line(mapping=aes(y=min_delta), color="red")
  p + geom_line(mapping=aes(y=max_delta), color="blue")
  p + geom_line(mapping=aes(y=median), color="black") + geom_line(mapping=aes(y=mean), color="green")
  p + geom_line(mapping=aes(y=max_delta/median), color="blue")
  
  
  # Boxplot
  p <- ggplot(data=full_sleep_data) + geom_boxplot(mapping=aes(subject_code, delta_power_in_stage_2_3))
  p + coord_cartesian(ylim=c(0, 6000)) + geom_segment(data=delta_subject_ranges, mapping=aes(x=subject_code-.5, xend=subject_code+.5, y=5*median, yend=5*median), color="red") + 
    geom_segment(data=delta_subject_ranges, mapping=aes(x=subject_code-.5, xend=subject_code+.5, y=new_cutoff, yend=new_cutoff), color="blue") +
    geom_segment(data=delta_subject_ranges, mapping=aes(x=subject_code-.5, xend=subject_code+.5, y=q95, yend=q95), color="green")
  p + scale_y_log10()
  
  total_delta_powers <- full_sleep_data[!is.na(nrem_episode_number),list(delta_power_sum_full=sum(delta_power), delta_power_sum_2_3=sum(delta_power_in_stage_2_3,na.rm=TRUE), delta_power_sum_filtered=sum(delta_power_below_cutoff,na.rm=TRUE)),by='subject_code,study,activity_or_bedrest_episode,nrem_episode_number']
  total_delta_powers <- merge(total_delta_powers,nrem_episode_output,by=c('subject_code','activity_or_bedrest_episode','nrem_episode_number'),all.x=TRUE,all.y=FALSE)
  total_delta_powers[,labtime:=start_labtime+((end_labtime-start_labtime)/2)]
  
  setkey(full_sleep_data,subject_code,activity_or_bedrest_episode,nrem_episode_number)
  total_delta_powers[, `:=`(total_time=nrow(full_sleep_data[subject_code==.BY$subject_code & activity_or_bedrest_episode == .BY$activity_or_bedrest_episode & nrem_episode_number == .BY$nrem_episode_number])*EPOCH_LENGTH,
                            time_in_2_3=nrow(full_sleep_data[subject_code==.BY$subject_code & activity_or_bedrest_episode == .BY$activity_or_bedrest_episode & nrem_episode_number == .BY$nrem_episode_number & !is.na(delta_power_in_stage_2_3)])*EPOCH_LENGTH,
                            time_with_valid_delta=nrow(full_sleep_data[subject_code==.BY$subject_code & activity_or_bedrest_episode == .BY$activity_or_bedrest_episode & nrem_episode_number == .BY$nrem_episode_number & !is.na(delta_power_below_cutoff)])*EPOCH_LENGTH),
                     by='subject_code,activity_or_bedrest_episode,nrem_episode_number']  
  
  # Setup data for plotting rasters
  setup_raster_data(full_sleep_data, episodes, total_delta_powers)
  
  # Plot individual rasters
  plot_list <- list()
  setkey(subjects, subject_code)

  plot_swa_raster("26O2GXT2")
  
  for(sc in unique(full_sleep_data$subject_code)) {

    plot_list[[sc]] <- plot_swa_raster(sc)
  }
  
  

  for(p in plot_list) {
    cat(paste("/home/pwm4/Desktop/deshields_rasters/", p$data$subject_code[[1]], ".png\n", sep=''))
    h <- ceiling(length(unique(p$data$activity_or_bedrest_episode)))
    print(h)
    ggsave(plot=p, file=paste("/home/pwm4/Desktop/deshields_rasters/below_cutoff/", p$data$subject_code[[1]], ".png", sep=''), height=h/2, width=4, scale=3, limitsize=FALSE)
  }
  
  # Create single file with rasters
  grid.arrange(arrangeGrob(grobs=plot_list[[1]], nrow=length(plot_list[[1]]), ncol=1))
  #marrangeGrob(plot_list[[2]], ncol=1, nrow=length(plot_list[[2]]))
  
  
  # Output NREM episodes
  nrem_episodes.out <- copy(nrem_episode_output)
  nrem_episodes.out[,`:=`(start_position=NULL, end_position=NULL, method=NULL, complete=NULL,label=NULL,length=NULL)]
  nrem_episodes.out[,end_labtime:=end_labtime+EPOCH_LENGTH]
  nrem_episodes.out[,midpoint:=start_labtime+(end_labtime-start_labtime)/2]
  setkey(nrem_episodes.out,subject_code,activity_or_bedrest_episode)
  
  write.csv(nrem_episodes.out[subject_code %in% c("R11032005_1", "R11032005_2", "W02282008_1")], file='/home/pwm4/Desktop/nrem_episodes_20160205.csv', row.names=FALSE, na="")
  
  # Output Sleep Data
  
  
  full_sleep_data.out <- copy(full_sleep_data)
  setcolorder(full_sleep_data.out,c(1:7, 71, 72, 8, 9, 73, 74, 10:70))
  
  full_sleep_data.out[,pk:=NULL]
  full_sleep_data.out[,se_pk:=NULL]
  
  full_sleep_data.out[,file_path:=str_replace(file_path, "/home/pwm4/Desktop/SWA", "")]
  setkey(full_sleep_data.out,study,subject_code,activity_or_bedrest_episode)
  
  write.csv(full_sleep_data.out, file='/home/pwm4/Desktop/merged_full_sleep_data_20160119.csv', row.names=FALSE, na="")
  
#   sleep_data.out <- copy(sleep_data)
#   sleep_data.out[,pk:=NULL]  
#   sleep_data.out[,file_path:=str_replace(file_path, "/home/pwm4/Desktop/SWA", "")]
#   write.csv(sleep_data.out, file='/home/pwm4/Desktop/merged_sleep_data_20151027.csv', row.names=FALSE, na="")
#   
  # Output Delta Power
  total_delta_powers.out <- copy(total_delta_powers)
  total_delta_powers.out[,`:=`(start_position=NULL, end_position=NULL, complete=NULL, method=NULL, label=NULL,length=NULL,labtime=NULL)]
  total_delta_powers.out[,end_labtime:=end_labtime+EPOCH_LENGTH]
  setkey(total_delta_powers.out,study,subject_code,activity_or_bedrest_episode)
  write.csv(total_delta_powers.out, file='/home/pwm4/Desktop/total_delta_power_20160119.csv', row.names=FALSE, na="")
}


map_numHypno_to_stage <- function(numHypno) {
  if(numHypno %in% c(1,2,3,4)){
    numHypno
  } else if (numHypno == 5) {
    6
  } else if (numHypno == 0) {
    5
  } else {
    9
  }
}

