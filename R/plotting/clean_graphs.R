library(ggthemes)
source("R/plotting/plot_theme.R")
## Datasets

# Name Changes etc.
stp<-function() {
  sequences[label=="N1",label:="NREM1",]
  sequences[label=="N2",label:="NREM2",]
  sequences[prev_label=="N1",prev_label:="NREM1",]
  sequences[prev_label=="N2",prev_label:="NREM2",]
  inter_state_intervals[type=="N1", type:="NREM1"]
  inter_state_intervals[type=="N2", type:="NREM2"]
  
}
#setnames(sequences, c('next_N2', "N2_latency"), c('next_NREM2', "NREM2_latency"))


graph_sequences <- rbind(sequences[tag=="high_res" & label %in% c("NREM1", "NREM2", "SWS")], sequences[tag=='normal'])
high_res_sequences <- sequences[tag=="high_res"]

## Continuous scale functions
log10p <- function(x) { log10(x+1) }
log10p_inv <- function(x) { 10^x - 1 }
log10p_trans <- function(x) { trans_new("log10p", "log10p", "log10p_inv") }

circ_breaks <- function() {
  function(x) fullseq(x, 30)
}

minor_circ_breaks <- function() {
  function(x) fullseq(x, 10)
}



## Figure 1: Distribution of Sequence Lengths by State
# Export: 1200x1000, fig1_seq_dist.png

distOfSeqLengthByState <- function() {
  source("R/plotting/plot_theme.R")
  
  plot <- ggplot(data=graph_sequences[label!="UNDEF" & length <= 240], aes(x=length))
  plot <- plot + geom_histogram(binwidth=2) + facet_wrap(~ label, scales='free', nrow=2)
  plot <- plot + ggtitle(element_blank()) + xlab("Length (minutes)") + ylab("")
  plot <- plot + scale_y_continuous(
    trans=log10p_trans(),
    breaks = trans_breaks(log10p, log10p_inv), 
    labels = trans_format("log10p", scales::math_format(10^.x))
  ) + scale_x_continuous(breaks=circ_breaks())
  plot + theme_tufte(base_size = 24, base_family="helvetica") + scale_colour_few() + scale_fill_few()
}

## Figure 2: Frequency of Arousal States by Circadian Phase

freqByCircPhase <- function() {
  by_phase <- high_res_sequences[protocol_section=='fd' & !is.na(phase_bin) & label != "UNDEF",list(label_length=sum(length)),by='protocol_section,label,phase_bin']
  by_phase[,total_length:=sum(label_length), by='protocol_section,phase_bin']
  by_phase[,p:=label_length/total_length]
  by_phase[,se:=sqrt(p*(1-p)/(total_length*2))]
  
  ggplot(data=by_phase) + geom_line(aes(x=phase_bin, y=p, color=label, group=label)) + 
    geom_ribbon(aes(x=phase_bin, ymax=p+se, ymin=p-se, fill=label, group=label), alpha=.4) + 
    theme_tufte(base_size=24, base_family="helvetica") + scale_colour_few() +
    labs(x="Circadian Phase\n(15-degree bins - upper bound)", y="Relative Frequency") +
    theme(legend.title=element_blank())
}


## Figure 3: Frequency of Arousal States by Time Since Sleep Onset
freqByTimeSinceSleepOnset <- function() {
  by_phase <- high_res_sequences[protocol_section=='fd' & !is.na(time_since_sleep_onset_bin) & label != "UNDEF",list(label_length=sum(length)),by='protocol_section,label,time_since_sleep_onset_bin']
  by_phase[,total_length:=sum(label_length), by='protocol_section,time_since_sleep_onset_bin']
  by_phase[,p:=label_length/total_length]
  by_phase[,se:=sqrt(p*(1-p)/(total_length*2))]
  
  ggplot(data=by_phase) + geom_line(aes(x=time_since_sleep_onset_bin, y=p, color=label, group=label)) + 
    geom_ribbon(aes(x=time_since_sleep_onset_bin, ymax=p+se, ymin=p-se, fill=label, group=label), alpha=.4) + 
    theme_tufte(base_size=24, base_family="helvetica") + scale_colour_few() +
    labs(x="Time Since Sleep Onset\n(half-hour bins - upper bound)", y="Relative Frequency") +
    theme(legend.title=element_blank())
}

## Figure 4: Arousal State Transition Heatmap
# 
stateTransitionHeatmap <- function() {
  plot_trans_heatmap <- function(d, breaks=c(0,.5, 1, 2,5,10,15,20,30,60), ps = "fd") {
    d <- copy(d[protocol_section == ps & tag=='high_res' & label != "UNDEF"])
    
    max_l <- max(d$prev_length, na.rm=TRUE)+1
    breaks <- c(breaks[breaks < max_l], max_l)
    labels <- paste(breaks[-length(breaks)], breaks[-1L], sep=' to ')
    
    d[,prev_length_label:=cut(prev_length, breaks = breaks, labels = labels, ordered_result = TRUE)]
    
    counts <- d[!is.na(prev_label) & prev_label!="UNDEF",list(count=.N),by='prev_label,prev_length_label']
    counts[,fake_x:="--"]
    
    # trans_d <- d[,list(prev_length, label, length, total=.N),by='prev_label,prev_length_label']
    # trans_d <- trans_d[,list(count=.N,total=max(total)),by='prev_label,prev_length_label,label']
    
    trans_d <- d[,list(prev_length, prev_length_label, label, length, total=.N), by='prev_label,prev_length_label']
    trans_d <- trans_d[,list(count=.N,total=max(total)),by='prev_label,prev_length_label,label']
    
    trans_d[,prob:=count/total]
    
    ggplot(trans_d[!is.na(prev_label) & prev_label != "UNDEF" & total > 50], aes(label,revFactor(prev_length_label))) +
      geom_tile(aes(fill=prob), color="white") + 
      scale_fill_gradient(low="white", high="steelblue4") + 
      ggtitle("") + geom_text(data=counts[count > 50], mapping=aes(label=count, x=fake_x)) +
      facet_wrap(~ prev_label, scales = "free", nrow=2) + labs(x = "Target State", y="Source State Length (minutes)", fill="") +
      theme_Publication(base_size=24, base_family="helvetica") + 
      theme(panel.background=element_blank(), panel.border = element_rect(colour = "black"), panel.grid.minor=element_blank(), panel.grid.major=element_blank(), axis.text.x = element_text(size = rel(.7)), axis.text.y=element_text(size=rel(.9))) 
      #labs(y=paste("Length of Inter-", d$type, "wake"), x=paste("length of Inter-", d$type, "non-wake")) +
    #scale_x_discrete(breaks=levels(d$heatmap_data$x_bin), labels=d$x_labs) +
    #scale_y_discrete(breaks=levels(d$heatmap_data$y_bin), labels=d$y_labs)
  }
  
  plot_trans_heatmap(high_res_sequences)
  
}

## Figure 5: Latecies after Wake by Previous State
latAfterWakeByPrev <- function() {
  l <- list(c("NREM", 60), c("REM", 240), c("WAKE", 120), c("SWS", 240), c("NREM2", 60))
  t <- l[[1]]
  
  ps <- lapply(l, function(t){
    #  cat(paste(typeof(t[2], '\n'))
    ll <- as.numeric(t[2])
    p <- ggplot(data=high_res_sequences[protocol_section == 'fd' & label=="WAKE" & prev_label %in% c("REM", "NREM1", "NREM2", "SWS")]) + 
      theme_Publication(base_size=24, base_family="helvetica") 
      #theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank()) 
    
    p <- p + geom_histogram(aes_string(x=paste(t[1],"latency", sep="_")), binwidth=2) + 
      scale_y_continuous(
        trans=log10p_trans(),
        breaks = trans_breaks(log10p, log10p_inv, n=2), 
        labels = trans_format("log10p", scales::math_format(10^.x))
      ) + scale_x_continuous(breaks=circ_breaks(), expand = c(0,0)) +
      
#       
#             scale_y_continuous(expand = c(0,0)) + #, trans = "log10p") +
#       scale_x_continuous(expand = c(0,0)) + 
#     # p <- p + geom_line(aes_string(x=paste(t[1],"latency", sep="_")), stat='density') +
      facet_grid(prev_label ~ ., scales = 'free') + 
      xlab(paste(t[1], "Latency (minutes)", sep=" ")) + 
      coord_cartesian(xlim=c(0.00,ll)) #+ ggtitle(paste(t, "Latency after WAKE by Preceding State",sep=" "))
    p  
  })
  
  grid.arrange(arrangeGrob(grobs=lapply(ps, function(x){x + theme(legend.position="none", axis.title.y=element_blank(), panel.border = element_rect(colour = "black"))}), ncol=3))
  
  
}

## Figure 6: Latencies by Length Class
latAfterWakeByLength <- function() {

  lat_p <- ggplot(data=high_res_sequences[tag=="high_res" & label=="WAKE" & protocol_section=="fd"])
  latency_types <- list(c("REM_latency", 250), c("WAKE_latency", 50), c("SWS_latency", 400))
    
  lat_graphs <- lapply(latency_types, function(name_x_val){
    print(name_x_val)
    lat_p + 
      geom_histogram(aes_string(x=name_x_val[1], fill="length_class"), binwidth = 2) + 
      coord_cartesian(xlim=c(0,as.numeric(name_x_val[2]))) +
      facet_grid(length_class ~ ., scales="free_x") + 
      theme_tufte(base_size = 24, base_family="helvetica") + scale_colour_few() + theme(axis.title.y=element_blank(), legend.position="none") +
      scale_x_continuous(expand = c(0,0)) #+ 
      # scale_y_continuous(trans="log10p",
      #   breaks = trans_breaks(log10p, log10p_inv), 
      #   labels = trans_format("log10p", scales::math_format(10^.x))
      # )
    
    #+ ggtitle(paste(name_x_val[1], "after", l)) #+ facet_grid(. ~ phase_label)
  });
    
  grid.arrange(arrangeGrob(grobs=lat_graphs, nrow=1, ncol=3))
}

## Figure 7
isiByType <- function() {
  p <- ggplot(data=inter_state_intervals, aes(x=interval_length)) + 
    geom_histogram(binwidth=1, aes(y=..count..+1))  + 
    facet_wrap(~ type, scales='free_x') + 
    scale_y_log10(breaks=trans_breaks(log10, function(x){ 10^x }), labels = trans_format("log10", scales::math_format(10^.x))) + 
    coord_cartesian(xlim=c(0,240)) +
    scale_x_continuous(breaks=circ_breaks(), expand = c(0,0)) +
    theme_tufte(base_size = 24, base_family="helvetica") + scale_colour_few() + 
    labs(x="Inter-State Interval Length (minutes)", y="")
  p
  
  
}

## Figure 8
isiRemHistogram <- function() {
  source("R/plotting/graph_helper.R")
  
  peak_lengths <<- list()
  avg_wakes <<- list()
  
  
  p1 <- plotIsiHist(inter_state_intervals, "REM")
  p2 <- plotIsiHist(inter_state_intervals, "REM", to_graph = "interval_length_without_wake")
  
  grid.arrange(grobs=append(p1,p2), ncol=2, as.table=FALSE)
}

## Figure 8b
isiHistogramTrend <- function() {
  source("R/plotting/graph_helper.R")
  

  
  p1 <- plotIsiTrend(inter_state_intervals, "REM", to_graph='interval_length_without_wake')
 # p2 <- plotIsiTrend(inter_state_intervals, "NREM")
  
#  grid.arrange(grobs=list(p1,p2), ncol=2, as.table=FALSE)
  
  p1
}

## Figure 9
isiPhase <- function() {
  
  isi_by_phase <- inter_state_intervals[!is.na(phase_angle) & protocol_section == 'fd', list(n1_sum=sum(N1), n2_sum=sum(N2), sws_sum=sum(SWS), wake_sum=sum(WAKE), rem_sum=sum(REM)) ,by='type,phase_bin']
  isi_by_phase[,total_sum:=n1_sum+n2_sum+sws_sum+wake_sum+rem_sum]
  isi_by_phase <- melt(isi_by_phase, measure.vars = c('rem_sum', 'sws_sum', 'n1_sum', 'n2_sum', 'wake_sum'), value.name='sum', variable.name = 'label')
  isi_by_phase[,p:=sum/total_sum]
  isi_by_phase[,se:=sqrt(p*(1-p)/(total_sum))]
  
  ggplot(data=isi_by_phase[type %in% c("REM", "SWS", "WAKE")]) + 
    geom_line(aes(x=phase_bin, y=(sum/total_sum), group=label, color=label)) + 
    geom_ribbon(aes(x=phase_bin, ymax=p+se, ymin=p-se, fill=label, group=label), alpha=.4) + 
    facet_grid(type ~ .) +
    theme_tufte(base_size=24, base_family="helvetica") + scale_colour_few(labels=c("REM", "SWS", "NREM1", "NREM2", "WAKE")) + scale_fill_few(labels=c("REM", "SWS", "NREM1", "NREM2", "WAKE")) +
    theme(legend.title=element_blank()) +
    labs(x="Circadian Phase\n(15-degree bins - upper bound)", y="Relative Frequency")

  
  
}

## Figure 10
isiTimeSinceSleepOnset <- function() {
  isi_by_so <- inter_state_intervals[!is.na(time_since_sleep_onset_bin) & protocol_section == 'fd', list(n1_sum=sum(N1), n2_sum=sum(N2), sws_sum=sum(SWS), wake_sum=sum(WAKE), rem_sum=sum(REM)) ,by='type,time_since_sleep_onset_bin']
  isi_by_so[,total_sum:=n1_sum+n2_sum+sws_sum+wake_sum+rem_sum]
  isi_by_so <- melt(isi_by_so, measure.vars = c('rem_sum', 'sws_sum', 'n1_sum', 'n2_sum', 'wake_sum'), value.name='sum', variable.name = 'label')
  isi_by_so[,p:=sum/total_sum]
  isi_by_so[,se:=sqrt(p*(1-p)/(total_sum))]
  
  ggplot(data=isi_by_so[type %in% c("REM", "SWS", "WAKE")]) + 
    geom_line(aes(x=time_since_sleep_onset_bin, y=(sum/total_sum), group=label, color=label)) + 
    geom_ribbon(aes(x=time_since_sleep_onset_bin, ymax=p+se, ymin=p-se, fill=label, group=label), alpha=.4) + 
    facet_grid(type ~ .) +
    theme_tufte(base_size=24, base_family="helvetica") + scale_colour_few(labels=c("REM", "SWS", "NREM1", "NREM2", "WAKE")) + scale_fill_few(labels=c("REM", "SWS", "NREM1", "NREM2", "WAKE")) +
    theme(legend.title=element_blank()) +
    labs(x="Time Since Sleep Onset\n(half-hour bins - upper bound)", y="Relative Frequency")
    
  
  
  
    
}





