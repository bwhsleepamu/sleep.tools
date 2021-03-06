# Mini
plot_raster("3319GX", plot_double=FALSE, labels=FALSE)

# One day
plot_raster("3319GX", plot_double=FALSE, first_day = 28, number_of_days = 1)

sequences$label_f <- as.factor(sequences$label)

# Distribution of sequence lengths - whole night and 1,2,3,4+
# - NREM
# - REM
# - WAKE 
  
seq_len_p <- ggplot(data=sequences[(label!="UNDEF" & tag=="high_res") | (tag=="normal" & label=="NREM")], aes(x=length)) + labs(x="Length (minutes)", y="") + scale_x_continuous(breaks=function(x){seq(x[1],x[2],by=60)})
seq_len_p + geom_histogram(binwidth=1) + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + facet_grid(phase_label ~ label_f, as.table=TRUE, scales='free') + coord_cartesian(xlim=c(0,300)) + ggtitle("Distribution of Sequence Lengths by State") + scale_y_continuous(trans='log1p')

seq_len_p1 <- ggplot(data=sequences[label%in% c("N1", "N2", "SWS")], aes(x=length,fill=label))
seq_len_p1 <- seq_len_p1 + geom_histogram(binwidth=1) + coord_cartesian(xlim=c(0,200)) + ggtitle("Distribution of NREM stage sequences") + scale_y_continuous(trans='log1p')

seq_len_p2 <- ggplot(data=sequences[label%in% c("NREM")], aes(x=length, fill=label))
seq_len_p2 <- seq_len_p2 + geom_histogram(binwidth=1) + coord_cartesian(xlim=c(0,200)) + ggtitle("Distribution of NREM stage sequences") + scale_y_continuous(trans='log1p')

marrangeGrob(list(seq_len_p1, seq_len_p2), nrow=2, ncol=1)

seq_len_p <- ggplot(data=sequences[label!="UNDEF" & !is.na(phase_label)], aes(x=length))
seq_len_p + geom_histogram(binwidth=2) + facet_grid(label ~ protocol_section, scales='free') + coord_cartesian(xlim=c(0,400)) + ggtitle("Distribution of Sequence Length by State and Phase of Sleep Episode") + scale_y_continuous(trans='log1p')


###
# ALL LATENCIES
###

# REM Latency post-wake
ggplot(data=sequences[tag=='high_res' & label == "WAKE"]) + geom_histogram(aes(x=REM_latency), binwidth=5) + facet_grid(. ~ phase_bin) + xlim(0,250)

ggplot(data=sequences[tag=='high_res' & label == "WAKE" & REM_latency < 50 & length > 10 & protocol_section=='fd']) + geom_density(aes(x=phase_angle)) + ggtitle("Distribution of SOREM Latencies vs. Phase Angle of Sleep Interruptions (> 5 min)")
ggplot(data=sequences[tag=='high_res' & label == "WAKE" & REM_latency > 50 & length > 10 & protocol_section=='fd']) + geom_density(aes(x=phase_angle)) + ggtitle("Distribution of non-SOREM Latencies vs. Phase Angle of Sleep Interruptions (> 5 min)")


+ geom_point(aes(x=abs(phase_angle), y=REM_latency))
# Post-WAKE
#wake_lat_p <- ggplot(data=sequences_with_latency[label=="WAKE"])
label_list <- c('WAKE', 'REM', 'NREM')

l <- 'WAKE'
name_x_val <- c("REM_latency", 100)
draw_latency <- function(l) {
  lat_p <- ggplot(data=sequences[tag=="high_res" & label==l & phase_label %in% c('in_phase', 'out_of_phase', 'neither')])
  latency_types <- list(c("REM_latency", 250), c("NREM_latency", 50), c("WAKE_latency", 100), c("NREM2_latency", 100), c("SWS_latency", 400))
  
  lat_graphs <- lapply(latency_types, function(name_x_val){
    print(name_x_val)
    lat_p + geom_density(aes_string(x=name_x_val[1], color="length_class")) + coord_cartesian(xlim=c(0,as.numeric(name_x_val[2]))) + ggtitle(paste(name_x_val[1], "after", l)) #+ facet_grid(. ~ phase_label)
  });
  
  grid.arrange(arrangeGrob(grobs=lat_graphs, nrow=5, ncol=1))
}

draw_latency(label_list)

draw_latency <- function(l) {
  lat_p <- ggplot(data=sequences[label==l & phase_label %in% c('in_phase', 'out_of_phase', 'neither') & cycle_number <= 6])
  latency_types <- list(c("rem_latency", 250), c("nrem_latency", 50), c("wake_latency", 100), c("stage_2_latency", 100), c("stage_3_latency", 400))
  
  lat_graphs <- lapply(latency_types, function(name_x_val){
    print(name_x_val)
    lat_p + geom_density(aes_string(x=name_x_val[1], color="phase_label")) + coord_cartesian(xlim=c(0,as.numeric(name_x_val[2]))) + ggtitle(paste(name_x_val[1], "after", l)) #+ facet_grid(cycle_number ~ .)
  });
  grid.arrange(arrangeGrob(grobs=lat_graphs, nrow=5, ncol=1))
  
  #marrangeGrob(grobs = lat_graphs, nrow=5, ncol=1)
}

draw_latency("WAKE")
draw_latency("NREM")
draw_latency("REM")


# Latency by length and preceding sequence
# - REM
# - NREM
# - Stage 2
# - Stage 3
# - WAKE

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

myleg <- g_legend(ps[[1]])



l <- list(c("NREM", 60), c("REM", 240), c("WAKE", 120), c("SWS", 240), c("N2", 60))
t <- "REM"
t <- "SWS"

ps <- lapply(l, function(t){
#  cat(paste(typeof(t[2], '\n'))
  ll <- as.numeric(t[2])
  p <- ggplot(data=high_res_sequences[protocol_section == 'fd' & label=="WAKE" & prev_label %in% c("REM", "NREM1", "NREM2", "SWS")]) + 
    scale_colour_manual(values=cbbbPalette) + 
    theme(axis.text.y = element_blank(), axis.ticks.y=element_blank()) + 
    scale_x_continuous(limits=c(0,ll), breaks=function(x){seq(x[1],x[2],by=30)}) 
  p <- p + geom_line(aes_string(x=paste(t[1],"latency", sep="_")), stat='density') + 
    facet_grid(prev_label ~ ., scales = 'free') + 
    coord_cartesian(xlim=c(0,ll)) + ggtitle(paste(t, "Latency after WAKE by Preceding State",sep=" "))
  p  
})

grid.arrange(arrangeGrob(grobs=lapply(ps, function(x){x + theme(legend.position="none")}), ncol=1), myleg, ncol=2, widths=c(10,1))
#grid.arrange(arrangeGrob(grobs=lapply(ps, function(x){x + theme(legend.position="none")}), ncol=3), myleg, ncol=2, widths=c(10,1))
grid.arrange(arrangeGrob(grobs=lapply(ps, function(x){x + theme(legend.position="none")}), ncol=3))
####
# INTER-state Intervals
####
p <- ggplot(data=inter_state_intervals[type=="REM" & !is.na(phase_label)], aes(x=interval_length_in_epochs)) + geom_density(aes(color=phase_label)) + coord_cartesian(xlim=c(0,300))
p

p <- ggplot(data=inter_state_intervals, aes(x=interval_length)) + 
  geom_histogram(binwidth=1, aes(y=..count..+1, fill=..count..))  + 
  facet_grid('type ~ .') + 
  scale_y_log10() + 
  coord_cartesian(xlim=c(0,240)) +
  scale_x_continuous(breaks=function(x){seq(x[1],x[2],by=30)})
p




# REM

l <- c("REM", "WAKE", "NREM", "N1", "N2", "SWS")
e <- "REM"

ps <- lapply(l, function(e){
  p <- ggplot(data=inter_state_intervals[protocol_section == 'fd' & phase_label %in% c("in_phase", "neither", "out_of_phase")], aes(interval_length))
  p <- p + geom_histogram(binwidth=1) + coord_cartesian(xlim=c(0,200), ylim=c(0,500)) + ggtitle(paste("Inter-", e, " Interval Histogram", sep="")) + facet_grid(. ~ type, scales = "free")
  p
})

ps
ps <- 'baseline'
pl <- 'in_phase'

peak_lengths <- list()
avg_wakes <- list()

t <- "REM"
to_graph <- "interval_length"
bw=1

plot_isi_histogram <- function(t, ps=c("baseline", "fd", "recovery"), pl=c("in_phase", "out_of_phase", NA, "neither"), bw=1, to_graph='interval_length') {
  # narrow down data set
  d <- copy(inter_state_intervals[type==t & protocol_section == 'fd' & !is.na(phase_angle)])
  
  # Get list of labels
  labs <- levels(d$interval_length_wake_label)
  dist_dt <- as.data.table(expand.grid(l=labs, p=c("day", "night", "neither"), stringsAsFactors=FALSE))
  
  # Compute Distributions
  dist_calc_fn <- function(d, l, p) {
    cat(paste(l,p,sep=" | "))
    
    s_d <- d[interval_length_wake_label == l & cohen_phase == p]
    cat(paste(nrow(s_d),l,p,sep=" | "))
    min_bin <- min(s_d[[to_graph]], na.rm = TRUE)
    
    s_d <- s_d[get(to_graph) >= (15 + min_bin) & get(to_graph) <= (120 + min_bin)]
    r <- fitdist(s_d[[to_graph]], 'norm')
    
    peak_lengths[[l]] <<- r$estimate[1]
    avg_wakes[[l]] <<- mean(s_d$interval_length_wake)
    
    as.list(r$estimate)
  }
  
  dist_dt[,c("mean", "sd") := dist_calc_fn(d, l, p),by='l,p']
  
  # dists <- sapply(labs, function(l) {
  #   s_d <- d[interval_length_wake_label==l]
  #   min_bin <- min(s_d[[to_graph]], na.rm = TRUE)
  #   
  #   s_d <- s_d[get(to_graph) >= (15 + min_bin) & get(to_graph) <= (120 + min_bin)]
  #   r <- fitdist(s_d[[to_graph]], 'norm')
  #   
  #   peak_lengths[[l]] <<- r$estimate[1]
  #   avg_wakes[[l]] <<- mean(s_d$interval_length_wake)
  #   
  #   r$estimate
  # }, USE.NAMES=TRUE, simplify=FALSE)
  
  ps <- list()
  plot_dists <- function(d, l, p, ex, oh) {
    #l <- dist_dt[$l
    #p <- tddd$p
    #ex <- tddd$mean
    #oh <- tddd$sd
    
    plot <- ggplot(data=d[interval_length_wake_label==l & cohen_phase == p], aes_string(to_graph)) + 
      geom_histogram(binwidth=bw, aes(y=..density..))  + 
      coord_cartesian(xlim=c(0,180)) + 
      ggtitle(paste(l, "| P: ", p, "  N:", nrow(d[interval_length_wake_label==l & cohen_phase == p]), "| Mean:", round(ex), "| SD:", round(oh) )) + 
      stat_function(fun=dnorm, colour="red", arg=list(mean=as.numeric(copy(ex)), sd=as.numeric(copy(oh))))
    plot
    
    cat(paste(l,p,ex,oh,sep=" "))
    cat('\n')
    
    ps[[paste(l,p,sep=" ")]] <<- plot
    NULL
  }   
  
  dist_dt[,plot_dists(d, l, p, mean, sd),by='p,l']
  
  
  
  
  grid.arrange(grobs=ps, ncol=3, as.table=FALSE)
}

# Peak Vs. Avg Wake
peak_avg_wake <- data.table(average_wake=as.numeric(avg_wakes), mean=as.numeric(peak_lengths))

ggplot(peak_avg_wake, aes(average_wake,mean)) + geom_point()

## Transition heatmaps
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
    scale_fill_gradient(low="white", high="steelblue4") + ggtitle("State Transition Heatmaps") + geom_text(data=counts[count > 50], mapping=aes(label=count, x=fake_x)) +
    theme(panel.background=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + facet_wrap(~ prev_label, scales = "free", nrow=2) + labs(x = "Target State", y="Source State Length (minutes)")
    #labs(y=paste("Length of Inter-", d$type, "wake"), x=paste("length of Inter-", d$type, "non-wake")) +
    #scale_x_discrete(breaks=levels(d$heatmap_data$x_bin), labels=d$x_labs) +
    #scale_y_discrete(breaks=levels(d$heatmap_data$y_bin), labels=d$y_labs)
}

transition_heatmap(graph_sequences)

## Hazard Functions



## ISI heatmaps
plot_isi_heatmap <- function(d) {
  ggplot(d$heatmap_data, aes(x_bin,y_bin)) +
    geom_tile(aes(fill=val), color="white") + 
    scale_fill_gradient(low="white", high="steelblue") +
    theme(panel.background=element_blank()) +
    labs(y=paste("Length of Inter-", d$type, "wake"), x=paste("length of Inter-", d$type, "non-wake")) +
    scale_x_discrete(breaks=levels(d$heatmap_data$x_bin), labels=d$x_labs) +
    scale_y_discrete(breaks=levels(d$heatmap_data$y_bin), labels=d$y_labs)
}

plot_isi_heatmap(heatmap_data_list$REM)


## Phase Distributions

ggplot(data=sequences[tag=="high_res" & protocol_section == 'fd' & label!="UNDEF"]) + geom_density(binwidth=30, mapping=aes(x=time_in_bed, color=length_class)) + facet_grid(label ~ ., scales = "free")
ggplot(data=sequences[tag=="high_res" & protocol_section == 'fd']) + geom_histogram(binwidth=5, mapping=aes(x=abs(phase_angle))) + facet_grid(length_class ~ label, scales = "free")

qplot(abs(time_in_bed), data=sequences[tag=="high_res" & label=='REM' & protocol_section == 'fd'], geom='density', color=length_class)
subjects


# Phase and Time in Bed

by_phase <- sequences[!is.na(phase_bin),list(label_length=sum(length)),by='tag,protocol_section,label,phase_bin']
by_phase[,total_length:=sum(label_length), by='tag,protocol_section,phase_bin']
by_phase[,p:=label_length/total_length]

ggplot(data=by_phase[tag=='high_res' & protocol_section=='fd']) + geom_line(aes(x=phase_bin, y=p, color=label, group=label))


by_tib <- sequences[!is.na(phase_bin),list(label_length=sum(length)),by='tag,protocol_section,label,time_in_bed_bin']
by_tib[,total_length:=sum(label_length), by='tag,protocol_section,time_in_bed_bin']
by_tib[,p:=label_length/total_length]

ggplot(data=by_tib[tag=='high_res' & protocol_section=='fd' & time_in_bed_bin != "NA"]) + geom_line(aes(x=time_in_bed_bin, y=p, color=label, group=label))


## 

ggplot(data=sequences[tag=='high_res' & label=="WAKE" & protocol_section=="fd" & !is.na(phase_angle) ]) + geom_boxplot(aes(x=phase_bin, y=REM_latency)) #+ facet_grid(. ~ ., scales = 'free')
ggplot(data=sequences[tag=='high_res' & label=="WAKE" & protocol_section=="fd" & !is.na(phase_angle)]) + geom_boxplot(aes(x=time_in_bed_bin, y=REM_latency)) #+ facet_grid(length_class ~ prev_label, scales='free')
# & prev_label %in% c("N1", "N2", "SWS", "REM", "WAKE")



##




isi_by_phase <- inter_state_intervals[!is.na(phase_angle) & protocol_section == 'fd', list(n1_sum=sum(N1), n2_sum=sum(N2), sws_sum=sum(SWS), wake_sum=sum(WAKE), undef_sum=sum(UNDEF), rem_sum=sum(REM)) ,by='type,interval_length_label,phase_bin']
isi_by_phase[,total_sum:=n1_sum+n2_sum+sws_sum+wake_sum+undef_sum+rem_sum]
isi_by_phase <- melt(isi_by_phase, measure.vars = c('rem_sum', 'sws_sum', 'undef_sum', 'n1_sum', 'n2_sum', 'wake_sum'), value.name='sum', variable.name = 'label')


ggplot(data=isi_by_phase) + 
  geom_line(aes(x=phase_bin, y=(sum/total_sum), group=label, color=label)) + 
  facet_grid(interval_length_label ~ type)

isi_by_tib <- inter_state_intervals[!is.na(time_in_bed_bin) & protocol_section == 'fd', list(n1_sum=sum(N1), n2_sum=sum(N2), sws_sum=sum(SWS), wake_sum=sum(WAKE), undef_sum=sum(UNDEF), rem_sum=sum(REM)) ,by='type,interval_length_label,time_in_bed_bin']
isi_by_tib[,number_of_intervals:=.N,by='type,interval_length_label']
isi_by_tib[,total_sum:=n1_sum+n2_sum+sws_sum+wake_sum+undef_sum+rem_sum]
isi_by_tib <- melt(isi_by_tib, measure.vars = c('rem_sum', 'sws_sum', 'undef_sum', 'n1_sum', 'n2_sum', 'wake_sum'), value.name='sum', variable.name = 'label')

setkey(isi_by_tib,type,interval_length_label,time_in_bed_bin)
isi_by_tib

ggplot(data=isi_by_tib[type=="REM"]) + 
  geom_line(aes(x=time_in_bed_bin, y=(sum/total_sum), group=label, color=label)) + 
  geom_text(aes(label=number_of_intervals,x=time_in_bed_bin), y=1) +
  facet_grid(interval_length_label ~ type)

