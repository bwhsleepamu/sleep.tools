source("R/sources.R")
source("R/plotting/rasters/swa_raster_plot.R")

#data_location <- "/X/Studies/Analyses/McHill-Shaw SWA/Subjects"
#data_location <- "/X/Studies/Analyses/McHill_Glucose SWA/For JDs Program"


data_location <- "/home/pwm4/Desktop/SWA/McHill-Shaw SWA/For PM_15-10-20"

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
  target_file_paths <- list.files(path=data_location, pattern='.detail.spectral.xls', recursive=TRUE, full.names=TRUE)
  target_file_paths
  
  subject_codes <- str_match(basename(target_file_paths), "^([[:alnum:]]*)[[:punct:][:space:]]")[,2]
  
  subjects <- data.table(subject_code=subject_codes, file_path=target_file_paths)
  setkey(subjects, subject_code)
  
  data_list <- list()
  subjects[,{
    dt <- as.data.table(read.xlsx(file_path,startRow=2));
    dt[,`:=`(subject_code=subject_code, file_path=file_path)];
    data_list[[paste(subject_code,file_path)]] <<- dt;
    0
  },by='subject_code,file_path']
  
  full_sleep_data <- rbindlist(data_list)
  full_sleep_data <- full_sleep_data[,c(112:113,1:54,106:111), with=FALSE]
  
  full_sleep_data[,activity_or_bedrest_episode:=as.numeric(as.factor(file_path)),by='subject_code']
  full_sleep_data[,labtime:=(24*(activity_or_bedrest_episode-1))+(0:(.N-1))*30/3600,by='subject_code,file_path']
  full_sleep_data[,pk:=.I]
  full_sleep_data[,stage:=map_numHypno_to_stage(numHypno),by='pk']
  full_sleep_data[,epoch_type:=as.factor(as.character(lapply(stage, map_epoch_type))),]
  full_sleep_data[,delta_power:=sum(.SD), .SDcols=(7:14), by='pk']
  setcolorder(full_sleep_data,c(1:2, 64, 63, 66:67, 68, 65, 3:62))
  
  sleep_data <- full_sleep_data[,1:8,with=FALSE]
  setup_episodes(sleep_data, sleep_data)
  
  nrem_episode_output <- copy(episodes[method=='iterative' & label=='NREM'])
  nrem_episode_output[,nrem_episode_number:=1:.N,by='subject_code,activity_or_bedrest_episode']
  setkey(nrem_episode_output,subject_code,activity_or_bedrest_episode,nrem_episode_number)
  
  sleep_data[,nrem_episode_number:=nrem_episode_output[subject_code==.SD$subject_code & activity_or_bedrest_episode==.SD$activity_or_bedrest_episode & pk>=start_position & pk <= end_position]$nrem_episode_number,by='pk']
  full_sleep_data[,nrem_episode_number:=nrem_episode_output[subject_code==.SD$subject_code & activity_or_bedrest_episode==.SD$activity_or_bedrest_episode & pk>=start_position & pk <= end_position]$nrem_episode_number,by='pk']
  
  total_delta_powers <- sleep_data[!is.na(nrem_episode_number),list(total_delta_power=sum(delta_power)),by='subject_code,activity_or_bedrest_episode,nrem_episode_number']
  total_delta_powers <- merge(total_delta_powers,nrem_episode_output,by=c('subject_code','activity_or_bedrest_episode','nrem_episode_number'),all.x=TRUE,all.y=FALSE)
  
  total_delta_powers[,labtime:=start_labtime+((end_labtime-start_labtime)/2)]
  
  
  setup_raster_data(sleep_data, episodes, total_delta_powers)
  p<-  plot_swa_raster("K11022003")
    
  
  
  
  
  plot_swa_raster("S05161999", first_day = 1, doubleplot = FALSE, hour_range = c(0,8.1), label_sleep_episode = FALSE)
  
  plot_list <- list()
  
  setkey(subjects, subject_code)
  
  for(sc in subjects$subject_code) {
    for(abe in unique(sleep_data[subject_code==sc]$activity_or_bedrest_episode)) {
      max_hour <- max(sleep_data[subject_code == sc & activity_or_bedrest_episode == abe]$labtime) - (24.0 * (abe-1))
      print(max_hour)
      
      plot_list[[paste(sc,abe,sep='_')]] <- plot_swa_raster(sc, activity_or_bedrest_episodes = c(abe), hour_range = c(0,max_hour))
    }
  }
  
  for(p in plot_list) {
    ggsave(plot=p, file=paste("/home/pwm4/Desktop/swa_rasters/", p$data$subject_code[[1]], "_", p$data$activity_or_bedrest_episode[[1]], ".svg", sep=''), height=3, width=8, scale=2, limitsize=FALSE)
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



function() { 

  file_names <- list.files("data/ANDREW_M/")
  sheet_list <- lapply(file_names, function(file_name){
    subject_code <- strsplit(file_name, ".", fixed=TRUE)[[1]][1]
    file_sheet <- as.data.table(read.xls(paste("data/ANDREW_M/", file_name, sep='')))
    file_sheet[,subject_code:=subject_code]
    file_sheet[,activity_or_bedrest_episode:=1]
  })
  
  andrew_data <- rbindlist(sheet_list)
  setnames(andrew_data, c("Time.min.", "Sleep", "delta..1.3.5."), c("labtime", "stage", "delta_power"))
  andrew_data[,pk:=.I]
  andrew_data[,epoch_type:=map_andrew_epoch_type(stage),by='pk']
  andrew_data[,stage:=map_andrew_stages(stage),by='pk']
  andrew_data[,labtime:=labtime/60.0]
  andrew_data[stage==2 | stage == 3,scale_factor:=4/boxplot.stats(delta_power)$stats[5],by='subject_code']
  andrew_data[stage==2 | stage == 3,scaled_delta_power:=delta_power * scale_factor]
  andrew_data[scaled_delta_power>4, scaled_delta_power:=4]
  
  sleep_data <- copy(andrew_data)
  setup_episodes(sleep_data, sleep_data)
  setup_cycles(sleep_data, episodes)
  setup_raster_data(sleep_data, episodes, cycles)

  plot_andrew_raster(subject_list=NULL, first_day = 1)
  
  andrew_episodes <-generate_episodes.iterative(andrew_data, min_nrem_length=CLASSIC_MIN_NREM, min_rem_length=CLASSIC_MIN_REM, min_wake_length=CLASSIC_MIN_REM)
  andrew_episodes[,count:=1:.N,by='subject_code,activity_or_bedrest_episode,label']
  andrew_episodes[,episode_id:=paste(label, count, sep="_")]
  
  episode_ids <- rep.int(andrew_episodes$episode_id, andrew_episodes$length)
  andrew_data[,episode_id:=episode_ids]
  
  
  
  View(andrew_episodes)
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