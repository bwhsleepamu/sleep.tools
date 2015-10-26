source("R/sources.R")
source("R/plotting/rasters/swa_raster_plot.R")

#data_location <- "/X/Studies/Analyses/McHill-Shaw SWA/Subjects"
#data_location <- "/X/Studies/Analyses/McHill_Glucose SWA/For JDs Program"

data_location <- list()
data_location$disrupt <- "/X/Studies/Analyses/McHill-Shaw SWA/For PM_15-10-20/For PM_Disrupt/"
data_location$precoc_pub <- "/X/Studies/Analyses/McHill-Shaw SWA/For PM_15-10-20/For PM_Precoc Pub/"


# Beth and Andrew Notes
skipped_first_rem <- c("103_1","131_1","1516_1","310_1","33_1","33_2","912_1","B05271999_2","B11092004_1","D04072006_1","D07252001_2","D10012007_1","F03102000_2","H09282005_2","J0642000_2","K01062006_1","L11182005v1_1","M02011998_2","M10202005_1","N10062000_2","O06142000_2","P07232001_2","R11032005_1","S05161999_2","S06101999_2","W06031999_2")
do_not_use <- c("H09282005_1", "K11102003_1", "S10272004_1", "L11182005_1")


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

function(data_location){
  subjects <- data.table(study="precocious_puberty", file_path=list.files(path=data_location$precoc_pub, pattern='.detail.spectral.xls', recursive=FALSE, full.names=TRUE))
  subjects <- rbind(subjects, data.table(study="disrupt", file_path=list.files(path=data_location$disrupt, pattern='.detail.spectral.xls', recursive=FALSE, full.names=TRUE)))
  subjects[,subject_code:=str_match(basename(file_path), "^([[:alnum:]]*)[[:punct:][:space:]]")[,2]]

  setkey(subjects, subject_code)
  
  data_list <- list()
  subjects[,{
    dt <- as.data.table(read.xlsx(file_path,startRow=2));
    dt[,`:=`(subject_code=subject_code, study=study, file_path=file_path)];
    data_list[[paste(subject_code,file_path)]] <<- dt;
    0
  },by='subject_code,study,file_path']
  
  full_sleep_data <- rbindlist(data_list)
  full_sleep_data <- full_sleep_data[,c(112:114,1:54,106:111), with=FALSE]
  
  full_sleep_data[,activity_or_bedrest_episode:=as.numeric(as.factor(file_path)),by='subject_code']
  full_sleep_data[, subject_code:=paste(subject_code, activity_or_bedrest_episode, sep="_")]
  full_sleep_data[, activity_or_bedrest_episode:=1]
  
  full_sleep_data[,labtime:=(24*(activity_or_bedrest_episode-1))+(0:(.N-1))*30/3600,by='subject_code,file_path']
  full_sleep_data[,pk:=.I]
  full_sleep_data[,stage:=map_numHypno_to_stage(numHypno),by='pk']
  full_sleep_data[,epoch_type:=as.factor(as.character(lapply(stage, map_epoch_type))),]
  
  cols_to_sum <- c("0.50.Hz", "1.00.Hz", "1.50.Hz", "2.00.Hz","2.50.Hz", "3.00.Hz", "3.50.Hz","4.00.Hz")
  full_sleep_data[, delta_power:=sum(.SD), .SDcols=cols_to_sum, by='pk']
  
  setcolorder(full_sleep_data,c(1:3, 65, 64, 67:68, 69, 66, 4:63))
  
  sleep_data <- full_sleep_data[,1:9,with=FALSE]
  setup_episodes(sleep_data, sleep_data)
  
  nrem_episode_output <- copy(episodes[method=='iterative' & label=='NREM'])
  nrem_episode_output[,nrem_episode_number:=1:.N,by='subject_code,activity_or_bedrest_episode']
  setkey(nrem_episode_output,subject_code,activity_or_bedrest_episode,nrem_episode_number)
  
  sleep_data[,nrem_episode_number:=nrem_episode_output[subject_code==.SD$subject_code & activity_or_bedrest_episode==.SD$activity_or_bedrest_episode & pk>=start_position & pk <= end_position]$nrem_episode_number,by='pk']
  full_sleep_data[,nrem_episode_number:=nrem_episode_output[subject_code==.SD$subject_code & activity_or_bedrest_episode==.SD$activity_or_bedrest_episode & pk>=start_position & pk <= end_position]$nrem_episode_number,by='pk']
  
  sleep_data[subject_code %in% skipped_first_rem & nrem_episode_number == 1]
  
  total_delta_powers <- sleep_data[!is.na(nrem_episode_number),list(total_delta_power=sum(.SD[epoch_type!="WAKE"]$delta_power)),by='subject_code,activity_or_bedrest_episode,nrem_episode_number']
  total_delta_powers <- merge(total_delta_powers,nrem_episode_output,by=c('subject_code','activity_or_bedrest_episode','nrem_episode_number'),all.x=TRUE,all.y=FALSE)
  
  total_delta_powers[,labtime:=start_labtime+((end_labtime-start_labtime)/2)]
  
  
  setup_raster_data(sleep_data, episodes, total_delta_powers)
  
  plot_list <- list()
  
  setkey(subjects, subject_code)
  
  for(sc in skipped_first_rem) {
    for(abe in unique(sleep_data[subject_code==sc]$activity_or_bedrest_episode)) {
      max_hour <- max(sleep_data[subject_code == sc & activity_or_bedrest_episode == abe]$labtime) - (24.0 * (abe-1))
      #print(max_hour)
      
      plot_list[[paste(sc,abe,sep='_')]] <- plot_swa_raster(sc, activity_or_bedrest_episodes = c(abe), hour_range = c(0,max_hour))
    }
  }
  
  for(p in plot_list) {
    ggsave(plot=p, file=paste("/home/pwm4/Desktop/swa_rasters/", p$data$subject_code[[1]], ".svg", sep=''), height=3, width=8, scale=2, limitsize=FALSE)
  }
  
  marrangeGrob(plot_list, ncol=1, nrow=length(plot_list))
  
  
  nrem_episodes.out <- copy(nrem_episode_output)
  nrem_episodes.out[,`:=`(start_position=NULL, end_position=NULL, method=NULL, complete=NULL,label=NULL)]
  nrem_episodes.out[,midpoint:=start_labtime+(end_labtime-start_labtime)/2]
  write.csv(nrem_episodes.out, file='/home/pwm4/Desktop/nrem_episodes_20151021.csv', row.names=FALSE, na="")
  
  
  full_sleep_data.out <- copy(full_sleep_data)
  full_sleep_data.out[,pk:=NULL]
  full_sleep_data.out[,file_path:=str_replace(file_path, "/home/pwm4/Desktop/SWA", "")]
  write.csv(sleep_data.out, file='/home/pwm4/Desktop/merged_full_sleep_data_20151021.csv', row.names=FALSE, na="")
  
    
  sleep_data.out <- copy(sleep_data)
  sleep_data.out[,pk:=NULL]  
  sleep_data.out[,file_path:=str_replace(file_path, "/home/pwm4/Desktop/SWA", "")]
  write.csv(sleep_data.out, file='/home/pwm4/Desktop/merged_sleep_data_20151021.csv', row.names=FALSE, na="")
  
  total_delta_powers.out <- copy(total_delta_powers)
  total_delta_powers.out[,`:=`(start_position=NULL, end_position=NULL, complete=NULL, method=NULL, label=NULL)]
  write.csv(total_delta_powers.out, file='/home/pwm4/Desktop/total_delta_power_20151021.csv', row.names=FALSE, na="")
  
}

## Insertion of Skipped First REM
function() {
  View(sleep_data[subject_code == "33_2" & nrem_episode_number == 1])
  
  # 33_1: 2nd WAKE around 2.00 
  # 6336: 2.0083333
  sleep_data[pk==6336, `:=`(stage=16, epoch_type="SREM")]
  
  # 33_2: min Delta in NREM2 section
  # 7487: 1.4
  min_delta <- min(sleep_data[subject_code == "33_2" & nrem_episode_number == 1 & labtime > 1.1 & labtime < 1.7]$delta_power)
  sleep_data[subject_code == "33_2" & nrem_episode_number == 1 & delta_power == min_delta]
  sleep_data[pk==7487, `:=`(stage=16, epoch_type="SREM")]

  # 103_1: 1st WAKE around 2.0
  # 276: 2.291667
  sleep_data[subject_code == "103_1" & nrem_episode_number == 1 & epoch_type=="WAKE"]
  sleep_data[pk==276, `:=`(stage=16, epoch_type="SREM")]
  
  # 131_1: 1st WAKE around 2.0
  # 1388: 2.0583333
  sleep_data[subject_code == "131_1" & nrem_episode_number == 1 & epoch_type=="WAKE"]
  sleep_data[pk==1388, `:=`(stage=16, epoch_type="SREM")]
  
  # 310_1: 1st Wake around 1.5
  # 5167: 1.583333
  sleep_data[subject_code == "310_1" & nrem_episode_number == 1 & epoch_type=="WAKE"]
  sleep_data[pk==5167, `:=`(stage=16, epoch_type="SREM")]
  
  # ** UNCLEAR PLACEMENT 
  # 912_1: 1st Wake around 2.5
  # 8782: 2.433333333
  sleep_data[subject_code == "912_1" & nrem_episode_number == 1 & epoch_type=="WAKE"]
  sleep_data[pk==8782, `:=`(stage=16, epoch_type="SREM")]
  
  # ** UNCLEAR PLACEMENT 
  # 1516_1: 1st Wake around 2
  # 2454: 2.108333
  sleep_data[subject_code == "1516_1" & nrem_episode_number == 1 & epoch_type=="WAKE"]
  sleep_data[pk==2454, `:=`(stage=16, epoch_type="SREM")]
  
  # B05271999_2: min Delta in NREM2 section around 3.0
  # 12183: 3.2
  min_delta <- min(sleep_data[subject_code == "B05271999_2" & nrem_episode_number == 1 & labtime > 3 & labtime < 4]$delta_power)
  sleep_data[subject_code == "B05271999_2" & nrem_episode_number == 1 & delta_power == min_delta]
  sleep_data[pk==12183, `:=`(stage=16, epoch_type="SREM")]
  
  # B11092004_1: min Delta in NREM2 section around 2.0
  # 13859: 2.183333
  min_delta <- min(sleep_data[subject_code == "B11092004_1" & nrem_episode_number == 1 & labtime > 2 & labtime < 2.7]$delta_power)
  sleep_data[subject_code == "B11092004_1" & nrem_episode_number == 1 & delta_power == min_delta]
  sleep_data[pk==13859, `:=`(stage=16, epoch_type="SREM")]
  
  # D04072006_1: min Delta in NREM2 section around 1.8
  # 17277: 1.7
  min_delta <- min(sleep_data[subject_code == "D04072006_1" & nrem_episode_number == 1 & labtime > 1.5 & labtime < 1.9]$delta_power)
  sleep_data[subject_code == "D04072006_1" & nrem_episode_number == 1 & delta_power == min_delta]
  sleep_data[pk==17277, `:=`(stage=16, epoch_type="SREM")]
  
  # ** NO PLACEMENT 
  # D07252001_2: DISRUPTION? no clear division...

  # ** UNCLEAR PLACEMENT 
  # D10012007_1: min Delta in NREM2 section around 3
  # 22626: 2.825
  min_delta <- min(sleep_data[subject_code == "D10012007_1" & nrem_episode_number == 1 & labtime > 2.8 & labtime < 3.5]$delta_power)
  sleep_data[subject_code == "D10012007_1" & nrem_episode_number == 1 & delta_power == min_delta]
  sleep_data[pk==22626, `:=`(stage=16, epoch_type="SREM")]  
  
  
  
}

####
####
map_andrew_stages <- function(x) {
  if(x == 7) { res <- 5 }
  else if (x == 5) { res <- 6 }
  else { res <- x }
  as.integer(res)
}

map_andrew_epoch_type <- function(x) {
  ## Possibly speed up if x is a factor??
  if (x >= 1 & x <=3) { res <- "NREM" }
  else if (x == 7) { res <- "WAKE" }
  else if (x == 5) { res <- "REM" }
  else { res <- "UNDEF" }
  
  res
}

plot_andrew_raster <- function(subject_list = NULL, #, epoch_length=EPOCH_LENGTH, output_dir="/home/pwm4/Desktop/", l="",
                        number_of_days=NA, 
                        first_day=1,
                        cycle_types=c("NREM")
) {  
  
  epoch_length = EPOCH_LENGTH
  
  # Limit by subject
  if(is.null(subject_list))
    subject_list <- unique(sleep_data.v$subject_code)
  
  # Limit by day
  days_to_graph <- unique(sleep_data.v[subject_code %in% subject_list]$day_number)
  if(!is.na(number_of_days))
    days_to_graph <- days_to_graph[first_day:(first_day+number_of_days-1)]
  
  print(days_to_graph)
  
  graph_data <<- copy(sleep_data.v[subject_code %in% subject_list & day_number %in% days_to_graph])
  setkeyv(graph_data, c('subject_code', 'labtime'))
  graph_episodes <<- copy(episodes.v[subject_code %in% subject_list & day_number %in% days_to_graph & activity_or_bedrest_episode > 0])
  graph_cycles <- copy(cycles.v[subject_code %in% subject_list & day_number %in% days_to_graph & activity_or_bedrest_episode > 0 & type %in% cycle_types])
  #graph_bedrest_episodes <- copy(bedrest_episodes.v[subject_code %in% subject_list & day_number %in% days_to_graph])
  
  # Draw
  .e <- environment()
  
  # Main Plot
  plot <- ggplot(graph_data, aes(x=day_labtime, y=stage_for_raster, group=day_number), environment = .e)
  
  # Labels and theming
  plot <- plot + theme(axis.title.y=element_blank(), legend.title=element_blank(), axis.line = element_blank(),panel.grid.minor=element_blank())
  plot <- plot + xlab("Time (hours)")
  
  # Faceting
  plot <- plot + facet_grid(subject_code ~ .)
  
  # Scaling and Margins
  #plot <- plot + theme(panel.margin = unit(0, "npc"))
  y_breaks <- c(-3.5,-2.5,-1.5,-0.5,.5,2,3,3.5,4)
  
  plot <- plot + scale_x_continuous(limits=c(0 - epoch_length, 10 + epoch_length), expand=c(0,0)) 
  plot <- plot + scale_y_continuous(limits=c(-4, 4), breaks=y_breaks, labels=lapply(y_breaks,y_axis_andrew_formatter))
  
  # Colors
  plot <- plot + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  
  #plot <- plot + scale_fill_manual(values=alpha(c("blue", "red", "black", "purple", "green", "yellow"), 0.8))
  
  plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -.95, ymax = -0.05, data = graph_episodes[method=='raw'])# & keep==TRUE])
  
  
  ## Episodes and Cycles
  #   methods <- c('raw')#, 'classic')#, 'changepoint')
  #   r <- foreach(i=1:length(methods)) %do% {
  #     end_pos <- i * -2    
  #     text_y_pos <- end_pos + 0.5
  #     
  #     #for_this_graph <- graph_cycles[method==methods[i]]
  #     #print(nrow(for_this_graph))
  #     #print(end_pos)
  #     plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = end_pos+.2, ymax = end_pos+1.8, data = graph_episodes[method==methods[i]])
  #     #plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), fill=NA, color='black', ymin = end_pos, ymax = end_pos+1, data=for_this_graph)    
  #     #plot <- plot + geom_text(aes(x=(start_day_labtime+end_day_labtime)/2, label=cycle_number), y=text_y_pos, data=for_this_graph)
  #   }  
  #plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -1, ymax = -0.2, data = graph_episodes[method=='classic'])
  plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -1.95, ymax = -1.05, data = graph_episodes[method=='classic'])# & keep==TRUE])
  #plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), fill=NA, color='black', ymin = -1.45, ymax = -1.05, data=graph_cycles[method=="classic"])    
  #plot <- plot + geom_text(aes(x=(start_day_labtime+end_day_labtime)/2, label=cycle_number), y=-2.25, data=graph_cycles[method=="classic"])
  
  plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -2.95, ymax = -2.05, data = graph_episodes[method=='iterative'])
  #plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), fill=NA, color='black', ymin = -2.95, ymax = -2.55, data=graph_cycles[method=="iterative"])    
  #plot <- plot + geom_text(aes(x=(start_day_labtime+end_day_labtime)/2, label=cycle_number), y=-3.95, data=graph_cycles[method=="iterative"])
  
  
  plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -3.95, ymax = -3.05, data = graph_episodes[method=='changepoint_compact'])
  #plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -6, ymax = -5.2, data = graph_episodes[method=='changepoint_compact'])
  #plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), fill=NA, color='black', ymin = -4.45, ymax = -4.05, data=graph_cycles[method=="changepoint_compact"])    
  #plot <- plot + geom_text(aes(x=(start_day_labtime+end_day_labtime)/2, label=cycle_number), y=-6.45, data=graph_cycles[method=="changepoint"])
  
  
  #  plot <- plot + geom_text(aes(x=(start_day_labtime+end_day_labtime)/2, y=-1.5, label=cycle_number), data=graph_cyles[method=='classic'])
  #   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -3, ymax = -2, data = graph_periods[method=="iterative"])
  #   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), ymin = -4, ymax = -3, data = graph_cyles[method=="iterative"])
  #   
  #   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length, fill = label), ymin = -5, ymax = -4, data = graph_periods[method=="changepoint"])
  #   plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), ymin = -6, ymax = -5, data = graph_cyles[method=="changepoint"])
  
  ## Bedrest Episodes
  #  plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + epoch_length), ymin = 0, ymax = 4, alpha=.5, data = graph_bedrest_episodes)
  
  
  
  #plot <- plot + geom_point(shape='.', size=2)
  #plot <- plot + geom_line(data=graph_data[activity_or_bedrest_episode>0],mapping=(aes(group=activity_or_bedrest_episode))) #aes(colour=epoch_type)
  plot <- plot + geom_line(data=graph_data[activity_or_bedrest_episode>0],mapping=(aes(x=day_labtime,y=scaled_delta_power,group=activity_or_bedrest_episode))) #aes(colour=epoch_type)
  
  #file_name = file.path(output_dir, paste(subject_code, "_", l, '.svg', sep=''))
  #print(file_name)
  #print(length(days_to_graph))
  #ggsave(plot=plot, file=file_name, height=(length(days_to_graph)*1 + 0.5), width=7, scale=2.5, limitsize=FALSE)
  
  plot
  
}


y_axis_andrew_formatter <- function(x) {
  if (x == 0.5) { res <- "" }
  else if (x == 2) { res <- "" }
  else if (x == 3) { res <- "" }
  else if (x == 3.5) { res <- "" }
  else if (x == 4) { res <- "" }
  #else if (x == 3.5) { res <- "" }
  #else if (x == 0) { res <- ""}
  else if (x == -0.5) { res <- "Raw"}
  else if (x == -1.5) { res <- "Traditional"}
  else if (x == -2.5) { res <- "Extended"}
  else if (x == -3.5) { res <- "Changepoint"}
  else { res <- as.character(x) }
  
  res
}