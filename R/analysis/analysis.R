source('R/analysis/agreement.R')
source('R/plotting/agreement_plot.R')
library(raster)

analysis_main <- function() {
  all_episodes <- copy(merged_episodes[protocol_section != 'fde' & method != "changepoint"])
  all_episodes[method == 'changepoint_compact', method:="Changepoint"]
  all_episodes[method == 'iterative', method:="Extended"]
  all_episodes[method == 'classic', method:="Traditional"]
  all_episodes[,typ:='All Bedrests']
  good_episodes <- all_episodes[raw_se > 0.95]
  good_episodes[,typ:='High SE']
  bad_episodes <- all_episodes[raw_se < 0.80]
  bad_episodes[,typ:='Low SE']
  analysis_episodes <- rbindlist(list(all_episodes,good_episodes,bad_episodes))
  analysis_episodes <- analysis_episodes[!is.na(protocol_section)]
  
  all_cycles <- copy(merged_cycles[protocol_section != 'fde' & method != "changepoint"])
  all_cycles[method == 'changepoint_compact', method:="Changepoint"]
  all_cycles[method == 'classic', method:="Traditional"]
  all_cycles[method == 'iterative', method:="Extended"]
  all_cycles[,typ:='All Bedrests']
  good_cycles <- all_cycles[raw_se > 0.95]
  good_cycles[,typ:='High SE']
  bad_cycles <- all_cycles[raw_se < 0.80]
  bad_cycles[,typ:='Low SE']
  analysis_cycles <- rbindlist(list(all_cycles,good_cycles,bad_cycles))
  analysis_cycles <- analysis_cycles[!is.na(protocol_section)]
  
  
  set_agreement(analysis_episodes, sleep_data)
  
  # Pre During Post
  
  # Male Female
  
  # Young Old
  
  # Habitual CSR
  
  # T Cycle
  
  ## I WANT TO SEE IF THERE ARE SIGNIFICANT DIFFERENCES BETWEEN THESE POPULATIONS
  
  episode_counts <- analysis_episodes[,count_in_bedrest_episode(label),by='subject_code,method,protocol_section,activity_or_bedrest_episode,typ']
  episode_counts <- episode_counts[method %in% c('Changepoint', 'Extended', 'Traditional')]
  
  cycle_counts <- analysis_cycles[,data.table(num_of_cycles=length(cycle_number)),by='subject_code,activity_or_bedrest_episode,method,protocol_section,typ,t_cycle']
  cycle_counts <- cycle_counts[method %in% c('Changepoint', 'Extended', 'Traditional')]

  
  analysis_episodes <- analysis_episodes[method %in%c('Changepoint', 'Extended', 'Traditional') & label!='UNDEF']
  analysis_episodes[,length:=length/2.0]
  
  analysis_cycles <- analysis_cycles[method %in%c('Changepoint', 'Extended', 'Traditional') & type == 'NREM'] 
  analysis_cycles[,length:=length/2.0]
  
  plot_agreement(analysis_episodes, facet_x_by='typ')
  
  p <- ggplot(analysis_episodes[label!="UNDEF"], aes(factor(typ), agreement))
  p <- p + facet_grid(label ~ .) + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  p <- p + geom_boxplot(aes(fill = factor(method)), outlier.size = 1) + coord_flip() + labs(title="Episode Agreement by Method and Sleep Efficiency (SE)", y="Agreement", x='', fill="") #+ theme(legend.position='bottom') + scale_y_continuous(breaks=pretty_breaks()) + guides(fill=FALSE)
  ggsave(plot=p, file="/home/pwm4/Desktop/rps/agreement.svg", height=2, width=2.6, scale=3, limitsize=FALSE)
  
  
  
  
  p <- ggplot(analysis_episodes[label!="UNDEF"], aes(factor(typ), length))
  p <- p + facet_grid(label ~ .) + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  p <- p + geom_boxplot(aes(fill = factor(method)), outlier.size = 1) + scale_y_log10()+ labs(title="Length of Episodes by Method and Sleep Efficiency (SE)", y="Episode Length (min)", x='', fill="") + theme(legend.position='bottom') +  coord_flip()+ guides(fill=FALSE)
  ggsave(plot=p, file="/home/pwm4/Desktop/rps/length.svg", height=2, width=2.6, scale=3, limitsize=FALSE)
  
  p <- ggplot(episode_counts, aes(factor(method), nrem_count))
  p <- p + facet_grid(. ~ typ) + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  p + geom_boxplot(aes(fill = factor(method)))
  
  
  p <- ggplot(episode_counts, aes(factor(method), rem_count))
  p <- p + facet_grid(. ~ typ) + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  p + geom_boxplot(aes(fill = factor(method)))
  
  p <- ggplot(episode_counts, aes(factor(method), wake_count))
  p <- p + facet_grid(. ~ typ) + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  p + geom_boxplot(aes(fill = factor(method)))
  
  p <- ggplot(cycle_counts, aes(factor(method), num_of_cycles))
  p <- p + facet_grid(typ ~ .) + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  p <- p + geom_boxplot(aes(fill = factor(method)), outlier.size = 1) + labs(title="Number of NREM Cycles per Bedrest by Method", y="Number of Cycles", x='', fill="") + theme(legend.position='bottom') + scale_y_continuous(breaks=pretty_breaks()) + coord_flip()+ guides(fill=FALSE)
  ggsave(plot=p, file="/home/pwm4/Desktop/rps/number_of_cycles.svg", height=2, width=2.6, scale=3, limitsize=FALSE)
  
  medians <- analysis_cycles[,data.table(med=median(length)),by='typ,method,cycle_number,protocol_section']
  
  p <- ggplot(analysis_cycles[method != 'changepoint' & cycle_number < 7 & protocol_section == 'fd'], aes(factor(method), length)) 
  p <- p + facet_grid(typ ~ .) + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  p <- p + geom_boxplot(aes(fill = factor(method)), outlier.size = 1) + scale_y_log10()+ labs(title="Length of NREM Cycles by Method", y="Cycle Length (min)", x='', fill="") + theme(legend.position='bottom') + coord_flip() + guides(fill=FALSE)
  ggsave(plot=p, file="/home/pwm4/Desktop/rps/cycle_length.svg", height=2, width=2.6, scale=3, limitsize=FALSE)
  


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#   ae <- analysis_episodes[label!="UNDEF"]
#   ylims <- ae[,data.table(plot_min=boxplot.stats(agreement)$stats[1], plot_max=boxplot.stats(agreement)$stats[5]),by='method,typ']
#   ylim <- c(min(ylims$plot_min), max(ylims$plot_max))
#   
  p <- ggplot(ae, aes(factor(method), agreement))
  p <- p + facet_grid(typ ~ .)
  p + geom_boxplot(aes(fill = factor(method))) + coord_flip(ylim = ylim) + scale_y_continuous(breaks=pretty_breaks())
  
  medians <- analysis_episodes[,data.table(med=median(length)),by='typ,method,label,protocol_section']
  p <- ggplot(analysis_episodes[label!="UNDEF"], aes(factor(method), length))
  p <- p + facet_grid(label ~ typ)
  p + geom_boxplot(aes(fill = factor(method))) + scale_y_log10() + coord_flip()# + scale_y_continuous(breaks=pretty_breaks())
  wilcox.test(analysis_episodes[method=='iterative']$length, analysis_episodes[method=='changepoint_compact']$length)
  
  p <- ggplot(episode_counts, aes(factor(method), nrem_count))
  p <- p + facet_grid(typ ~ .)
  p + geom_boxplot(aes(fill = factor(method))) + coord_flip() + scale_y_continuous(breaks=pretty_breaks())
  wilcox.test(episode_counts[method=='classic']$nrem_count,episode_counts[method=='iterative']$nrem_count)
  wilcox.test(episode_counts[method=='classic']$nrem_count,episode_counts[method=='changepoint_compact']$nrem_count)
  wilcox.test(episode_counts[method=='changepoint_compact']$nrem_count,episode_counts[method=='iterative']$nrem_count)
  
  p <- ggplot(episode_counts, aes(factor(method), rem_count))
  p <- p + facet_grid(typ ~ .)
  p + geom_boxplot(aes(fill = factor(method))) + coord_flip() + scale_y_continuous(breaks=pretty_breaks())
  wilcox.test(episode_counts[method=='classic']$rem_count,episode_counts[method=='iterative']$rem_count)
  wilcox.test(episode_counts[method=='classic']$rem_count,episode_counts[method=='changepoint_compact']$rem_count)
  wilcox.test(episode_counts[method=='changepoint_compact']$rem_count,episode_counts[method=='iterative']$rem_count)
  
  p <- ggplot(episode_counts[method!='classic'], aes(factor(method), wake_count))
  p <- p + facet_grid(typ ~ .)
  p + geom_boxplot(aes(fill = factor(method))) + coord_flip() + scale_y_continuous(breaks=pretty_breaks())
  
  p <- ggplot(cycle_counts, aes(factor(method), num_of_cycles))
  p <- p + facet_grid(typ ~ t_cycle)
  p + geom_boxplot(aes(fill = factor(method))) + coord_flip() + scale_y_continuous(breaks=pretty_breaks())
  
  medians <- analysis_cycles[,data.table(med=median(length)),by='typ,method,cycle_number']
  p <- ggplot(analysis_cycles[method != 'changepoint' & cycle_number < 7], aes(factor(method), length)) 
  p <- p + facet_grid(typ ~ cycle_number)
  p + geom_boxplot(aes(fill = factor(method))) + geom_text(data=medians[cycle_number < 7], aes(x=factor(method), y=med, label = med)) + scale_y_log10()
  
}
# 
# 
# analysis_main <- function() {
#   all_episodes <- copy(merged_episodes)
#   all_episodes[,typ:='all']
#   good_episodes <- merged_episodes[raw_se > 0.95]
#   good_episodes[,typ:='good']
#   bad_episodes <- merged_episodes[raw_se < 0.80]
#   bad_episodes[,typ:='bad']
#   analysis_episodes <- rbindlist(list(all_episodes,good_episodes,bad_episodes))
#   analysis_episodes <- analysis_episodes[!is.na(protocol_section)]
#   
#   all_cycles <- copy(merged_cycles)
#   all_cycles[,typ:='all']
#   good_cycles <- merged_cycles[raw_se > 0.95]
#   good_cycles[,typ:='good']
#   bad_cycles <- merged_cycles[raw_se < 0.80]
#   bad_cycles[,typ:='bad']
#   analysis_cycles <- rbindlist(list(all_cycles,good_cycles,bad_cycles))
#   analysis_cycles <- analysis_cycles[!is.na(protocol_section)]
#   
#   set_agreement(analysis_episodes, sleep_data)
#   
#   # Pre During Post
#   
#   # Male Female
#   
#   # Young Old
#   
#   # Habitual CSR
#   
#   # T Cycle
#   
#   ## I WANT TO SEE IF THERE ARE SIGNIFICANT DIFFERENCES BETWEEN THESE POPULATIONS
#   
#   episode_counts <- analysis_episodes[,count_in_bedrest_episode(label),by='subject_code,method,activity_or_bedrest_episode,typ']
#   episode_counts <- episode_counts[method %in% c('changepoint_compact', 'iterative', 'classic')]
#   
#   cycle_counts <- analysis_cycles[,data.table(num_of_cycles=length(cycle_number)),by='subject_code,activity_or_bedrest_episode,method,typ']
#   cycle_counts <- cycle_counts[method %in% c('changepoint_compact', 'iterative', 'classic')]
#   
#   
#   analysis_episodes <- analysis_episodes[method %in%c('changepoint_compact', 'iterative') & label!='UNDEF']
#   
#   analysis_cycles <- analysis_cycles[method %in%c('changepoint_compact', 'iterative', 'classic') & type == 'NREM'] 
#   
#   plot_agreement(analysis_episodes, facet_x_by='typ', facet_y_by='protocol_section')
#   
#   
# }





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
