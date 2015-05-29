source('R/analysis/agreement.R')
source('R/plotting/agreement_plot.R')
library(raster)

analysis_main <- function() {
  all_episodes <- copy(merged_episodes[protocol_section != 'fd'])
  all_episodes[,typ:='all']
  good_episodes <- merged_episodes[raw_se > 0.95 & protocol_section != 'fd']
  good_episodes[,typ:='good']
  bad_episodes <- merged_episodes[raw_se < 0.80 & protocol_section != 'fd']
  bad_episodes[,typ:='bad']
  analysis_episodes <- rbindlist(list(all_episodes,good_episodes,bad_episodes))
  analysis_episodes <- analysis_episodes[!is.na(protocol_section)]
  
  all_cycles <- copy(merged_cycles[protocol_section != 'fd'])
  all_cycles[,typ:='all']
  good_cycles <- merged_cycles[raw_se > 0.95 & protocol_section != 'fd']
  good_cycles[,typ:='good']
  bad_cycles <- merged_cycles[raw_se < 0.80 & protocol_section != 'fd']
  bad_cycles[,typ:='bad']
  analysis_cycles <- rbindlist(list(all_cycles,good_cycles,bad_cycles))
  analysis_cycles <- analysis_cycles[!is.na(protocol_section)]
  
  
  set_agreement(analysis_episodes, sleep_data)
  
  # Pre During Post
  
  # Male Female
  
  # Young Old
  
  # Habitual CSR
  
  # T Cycle
  
  ## I WANT TO SEE IF THERE ARE SIGNIFICANT DIFFERENCES BETWEEN THESE POPULATIONS
  
  episode_counts <- analysis_episodes[,count_in_bedrest_episode(label),by='subject_code,method,activity_or_bedrest_episode,typ']
  episode_counts <- episode_counts[method %in% c('changepoint_compact', 'iterative', 'classic')]
  
  cycle_counts <- analysis_cycles[,data.table(num_of_cycles=length(cycle_number)),by='subject_code,activity_or_bedrest_episode,method,typ']
  cycle_counts <- cycle_counts[method %in% c('changepoint_compact', 'iterative', 'classic')]

  
  analysis_episodes <- analysis_episodes[method %in%c('changepoint_compact', 'iterative') & label!='UNDEF']
  
  analysis_cycles <- analysis_cycles[method %in%c('changepoint_compact', 'iterative', 'classic') & type == 'NREM'] 
                                         
  plot_agreement(analysis_episodes, facet_x_by='typ')
  
  
  p <- ggplot(analysis_episodes[label!="UNDEF"], aes(factor(typ), agreement))
  p <- p + facet_grid(label ~ .)
  p + geom_boxplot(aes(fill = factor(method))) + coord_flip()
  
  
  medians <- analysis_episodes[,data.table(med=median(length)),by='typ,method,label']
  p <- ggplot(analysis_episodes[label!="UNDEF"], aes(factor(typ), length))
  p <- p + facet_grid(. ~ label)
  p + geom_boxplot(aes(fill = factor(method))) + geom_text(data=medians[label != "UNDEF"], aes(x=factor(typ), y=med, label = med)) + scale_y_log10()
  
  p <- ggplot(episode_counts, aes(factor(method), nrem_count))
  p <- p + facet_grid(. ~ typ)
  p + geom_boxplot(aes(fill = factor(method)))
  
  p <- ggplot(episode_counts, aes(factor(method), rem_count))
  p <- p + facet_grid(. ~ typ)
  p + geom_boxplot(aes(fill = factor(method)))
  
  p <- ggplot(episode_counts, aes(factor(method), wake_count))
  p <- p + facet_grid(. ~ typ)
  p + geom_boxplot(aes(fill = factor(method)))
  
  p <- ggplot(cycle_counts, aes(factor(typ), num_of_cycles))
  p <- p + facet_grid(. ~ method)
  p + geom_boxplot(aes(fill = factor(method)))
  
  medians <- analysis_cycles[,data.table(med=median(length)),by='typ,method,cycle_number']
  p <- ggplot(analysis_cycles[method != 'changepoint' & cycle_number < 7], aes(factor(method), length)) 
  p <- p + facet_grid(typ ~ cycle_number)
  p + geom_boxplot(aes(fill = factor(method))) + geom_text(data=medians[cycle_number < 7], aes(x=factor(method), y=med, label = med)) + scale_y_log10()
  
}


analysis_main <- function() {
  all_episodes <- copy(merged_episodes)
  all_episodes[,typ:='all']
  good_episodes <- merged_episodes[raw_se > 0.95]
  good_episodes[,typ:='good']
  bad_episodes <- merged_episodes[raw_se < 0.80]
  bad_episodes[,typ:='bad']
  analysis_episodes <- rbindlist(list(all_episodes,good_episodes,bad_episodes))
  analysis_episodes <- analysis_episodes[!is.na(protocol_section)]
  
  all_cycles <- copy(merged_cycles)
  all_cycles[,typ:='all']
  good_cycles <- merged_cycles[raw_se > 0.95]
  good_cycles[,typ:='good']
  bad_cycles <- merged_cycles[raw_se < 0.80]
  bad_cycles[,typ:='bad']
  analysis_cycles <- rbindlist(list(all_cycles,good_cycles,bad_cycles))
  analysis_cycles <- analysis_cycles[!is.na(protocol_section)]
  
  set_agreement(analysis_episodes, sleep_data)
  
  # Pre During Post
  
  # Male Female
  
  # Young Old
  
  # Habitual CSR
  
  # T Cycle
  
  ## I WANT TO SEE IF THERE ARE SIGNIFICANT DIFFERENCES BETWEEN THESE POPULATIONS
  
  episode_counts <- analysis_episodes[,count_in_bedrest_episode(label),by='subject_code,method,activity_or_bedrest_episode,typ']
  episode_counts <- episode_counts[method %in% c('changepoint_compact', 'iterative', 'classic')]
  
  cycle_counts <- analysis_cycles[,data.table(num_of_cycles=length(cycle_number)),by='subject_code,activity_or_bedrest_episode,method,typ']
  cycle_counts <- cycle_counts[method %in% c('changepoint_compact', 'iterative', 'classic')]
  
  
  analysis_episodes <- analysis_episodes[method %in%c('changepoint_compact', 'iterative') & label!='UNDEF']
  
  analysis_cycles <- analysis_cycles[method %in%c('changepoint_compact', 'iterative', 'classic') & type == 'NREM'] 
  
  plot_agreement(analysis_episodes, facet_x_by='typ', facet_y_by='protocol_section')
  
  
  p <- ggplot(analysis_episodes[label!="UNDEF"], aes(factor(typ), agreement))
  p <- p + facet_grid(label ~ protocol_section)
  p + geom_boxplot(aes(fill = factor(method))) + coord_flip()
  
  
  p <- ggplot(analysis_episodes[label!="UNDEF"], aes(factor(typ), length))
  p <- p + facet_grid(protocol_section ~ label)
  p + geom_boxplot(aes(fill = factor(method))) + scale_y_log10()
  
  p <- ggplot(episode_counts, aes(factor(method), nrem_count))
  p <- p + facet_grid(. ~ typ)
  p + geom_boxplot(aes(fill = factor(method)))
  
  p <- ggplot(episode_counts, aes(factor(method), rem_count))
  p <- p + facet_grid(. ~ typ)
  p + geom_boxplot(aes(fill = factor(method)))
  
  p <- ggplot(episode_counts, aes(factor(method), wake_count))
  p <- p + facet_grid(. ~ typ)
  p + geom_boxplot(aes(fill = factor(method)))
  
  p <- ggplot(cycle_counts, aes(factor(typ), num_of_cycles))
  p <- p + facet_grid(. ~ method)
  p + geom_boxplot(aes(fill = factor(method)))
  
  medians <- analysis_cycles[,data.table(med=median(length)),by='typ,method,cycle_number,protocol_section']
  p <- ggplot(analysis_cycles[method != 'changepoint' & cycle_number < 7 & protocol_section != 'fd'], aes(factor(method), length)) 
  p <- p + facet_grid(typ ~ cycle_number)
  p + geom_boxplot(aes(fill = factor(method))) + geom_text(data=medians[cycle_number < 7 & protocol_section != 'fd'], aes(x=factor(method), y=med, label = med)) + scale_y_log10()
  
  medians <- analysis_cycles[,data.table(med=median(length)),by='typ,method,cycle_number,protocol_section']
  p <- ggplot(analysis_cycles[method != 'changepoint' & cycle_number < 7 & protocol_section == 'fd'], aes(factor(method), length)) 
  p <- p + facet_grid(typ ~ cycle_number)
  p + geom_boxplot(aes(fill = factor(method))) + geom_text(data=medians[cycle_number < 7 & protocol_section == 'fd'], aes(x=factor(method), y=med, label = med)) + scale_y_log10()
  
}





count_in_bedrest_episode <- function(episode_types) {
  t <- table(episode_types)
  data.table(nrem_count=t["NREM"], rem_count=t["REM"], wake_count=t["WAKE"], undef_count=t["UNDEF"])
}
# 
# wd[,`:=`(cmethod=NULL,a=NULL,b=NULL,c=NULL,iterative_num=NULL,classic_num=NULL,changepoint_compact_num=NULL)]
# wd[method=='iterative', iterative_num:=num_of_cycles]
# wd[method=='classic', classic_num:=num_of_cycles]
# wd[method=='changepoint_compact', changepoint_compact_num:=num_of_cycles]
# wd[,`:=`(iterative_num=max(iterative_num,na.rm=TRUE),classic_num=max(classic_num,na.rm=TRUE),changepoint_compact_num=max(changepoint_compact_num,na.rm=TRUE)),by='subject_code,activity_or_bedrest_episode']
# 
# 
# wd <- cycle_counts[typ=='all']
# r<-wd[,list(iterative_num=max(iterative_num,na.rm=TRUE),classic_num=max(classic_num,na.rm=TRUE),changepoint_compact_num=max(changepoint_compact_num,na.rm=TRUE)),by='subject_code,activity_or_bedrest_episode']
# 
# qplot(factor(episode_type), agreement, data = joint_episodes_fd, geom = "boxplot")
