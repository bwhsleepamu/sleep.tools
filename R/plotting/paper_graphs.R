
# Mini
plot_raster("3319GX", plot_double=FALSE, labels=FALSE)
# One day
plot_raster("3319GX", plot_double=FALSE, first_day = 28, number_of_days = 1)

sequences$label_f <- as.factor(sequences$label)
#sequences$label_f <- factor(sequences$label_f, levels=c("WAKE", "REM", "NREM", "N1", "N2", "SWS", "UNDEF"))

#sequences$label_f <- factor(unique(sequences$label), levels=c("WAKE", "REM", "NREM", "N1", "N2", "SWS", "UNDEF"))

#sequences$old_label <- sequences$label  
#sequences$label <- sequences$label_f

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

# Post-WAKE
#wake_lat_p <- ggplot(data=sequences_with_latency[label=="WAKE"])
label_list <- c('WAKE', 'REM', 'NREM')

draw_latency <- function(l) {
  lat_p <- ggplot(data=sequences[tag=="high_res" & label==l & phase_label %in% c('in_phase', 'out_of_phase', 'neither')])
  latency_types <- list(c("REM_latency", 250), c("NREM_latency", 50), c("WAKE_latency", 100), c("N2_latency", 100), c("SWS_latency", 400))
  
  lat_graphs <- lapply(latency_types, function(name_x_val){
    print(name_x_val)
    lat_p + geom_density(aes_string(x=name_x_val[1], color="phase_label")) + coord_cartesian(xlim=c(0,as.numeric(name_x_val[2]))) + ggtitle(paste(name_x_val[1], "after", l)) #+ facet_grid(cycle_number ~ .)
  });
  
  marrangeGrob(grobs = lat_graphs, nrow=5, ncol=1)
}

draw_latency <- function(l) {
  lat_p <- ggplot(data=sequences[label==l & phase_label %in% c('in_phase', 'out_of_phase', 'neither') & cycle_number <= 6])
  latency_types <- list(c("rem_latency", 250), c("nrem_latency", 50), c("wake_latency", 100), c("stage_2_latency", 100), c("stage_3_latency", 400))
  
  lat_graphs <- lapply(latency_types, function(name_x_val){
    print(name_x_val)
    lat_p + geom_density(aes_string(x=name_x_val[1], color="phase_label")) + coord_cartesian(xlim=c(0,as.numeric(name_x_val[2]))) + ggtitle(paste(name_x_val[1], "after", l)) #+ facet_grid(cycle_number ~ .)
  });
  
  marrangeGrob(grobs = lat_graphs, nrow=5, ncol=1)
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



l <- c("NREM", "REM", "WAKE", "SWS", "N2")
t <- "REM"
t <- "SWS"

ps <- lapply(l, function(t){
  p <- ggplot(data=sequences[tag == "high_res" & label=="WAKE" & prev_label %in% c("REM", "N1", "N2", "SWS")]) + scale_colour_manual(values=cbbbPalette) + theme(axis.text.y = element_blank(), axis.ticks.y=element_blank()) + scale_x_continuous(breaks=function(x){seq(x[1],x[2],by=60)})
  p <- p + geom_line(aes_string(x=paste(t,"latency", sep="_"), color="length_class"), stat='density') + facet_grid(prev_label ~ protocol_section, scales = 'free') + coord_cartesian(xlim=c(0,240)) + ggtitle(paste(t, "latency after WAKE by preceding state",sep=" "))
  p  
})

grid.arrange(arrangeGrob(grobs=lapply(ps, function(x){x + theme(legend.position="none")})), myleg, ncol=2, widths=c(10,1))

####
# INTER-state Intervals
####
inter_state_intervals$interval_length


p <- ggplot(data=inter_state_intervals[type=="REM" & !is.na(phase_label)], aes(x=interval_length_in_epochs)) + geom_density(aes(color=phase_label)) + coord_cartesian(xlim=c(0,300))
p

# REM
l <- c("REM", "WAKE", "NREM", "N1", "N2", "SWS")
e <- "SWS"

ps <- lapply(l, function(e){
  p <- ggplot(data=inter_state_intervals[type==e], aes(interval_length))
  p <- p + geom_histogram(binwidth=1) + coord_cartesian(xlim=c(0,300)) + ggtitle(paste("Inter-", e, " Interval Histogram", sep=""))
  p
})

ps <- 'baseline'
pl <- 'in_phase'

plot_isi_histogram <- function(t, ps=c("baseline", "fd", "recovery"), pl=c("in_phase", "out_of_phase", "NA", "neither"), wl = c("0 - 2", "2 - 10", "10 - 20", ">50", "40 - 50", "30 - 40", "20 - 30"), bw=1, to_graph='interval_length') {
  d <- inter_state_intervals[type==t & protocol_section%in% ps & phase_label %in% pl & interval_length_wake_label %in% wl]
  p <- ggplot(data=d, aes_string(to_graph)) + geom_histogram(binwidth=bw) + ggtitle(paste("Inter-", t, " Interval Histogram | ", paste(ps, collapse='/'), " | ", paste(pl,collapse='/'), sep=""))
  
  # if(!is.na(to_facet))
  #   p <- p + facet_wrap(as.formula(paste("~", to_facet)), ncol=1)
  # 
  built_data <- as.data.table(ggplot_build(p)$data[[1]])
  
  #print(build_data)
  
  max_y <- built_data[x > 10]$count * 3

  p + coord_cartesian(xlim=c(0,180))  
  
  
  
}

plot_isi_histogram("REM", bw=3, to_graph = 'interval_length_without_wake', wl=c('0 - 2'))
plot_isi_histogram("REM", bw=3, to_graph = 'interval_length_without_wake', wl=c('2 - 10'))
plot_isi_histogram("REM", bw=3, to_graph = 'interval_length_without_wake', wl=c('10 - 20'))
plot_isi_histogram("REM", bw=3, to_graph = 'interval_length_without_wake', wl=c('20 - 30'))
plot_isi_histogram("REM", bw=3, to_graph = 'interval_length_without_wake', wl=c('30 - 40'))
plot_isi_histogram("REM", bw=5, to_graph = 'interval_length_without_wake', wl=c('40 - 50'))
plot_isi_histogram("REM", bw=5, to_graph = 'interval_length_without_wake', wl=c('>50'))

plot_isi_histogram("REM", bw=3, to_graph = 'interval_length', wl=c('0 - 2'))
plot_isi_histogram("REM", bw=3, to_graph = 'interval_length', wl=c('2 - 10'))
plot_isi_histogram("REM", bw=3, to_graph = 'interval_length', wl=c('10 - 20'))
plot_isi_histogram("REM", bw=3, to_graph = 'interval_length', wl=c('20 - 30'))
plot_isi_histogram("REM", bw=3, to_graph = 'interval_length', wl=c('30 - 40'))
plot_isi_histogram("REM", bw=3, to_graph = 'interval_length', wl=c('40 - 50'))
plot_isi_histogram("REM", bw=3, to_graph = 'interval_length', wl=c('>50'))



# REM - Baseline
plot_isi_histogram("REM", ps=c('fd'), bw=1)
ggplot(data=inter_state_intervals[type==e & protocol_section == 'baseline'], aes(interval_length)) + geom_histogram(binwidth=4) + coord_cartesian(ylim=c(0,250),xlim=c(0,180)) + ggtitle(paste("Inter-", e, " Interval Histogram | Baseline", sep=""))

# REM - In Phase
ggplot(data=inter_state_intervals[type==e & protocol_section == 'fd' & phase_label == 'in_phase'], aes(interval_length)) + geom_histogram(binwidth=2) + coord_cartesian(ylim=c(0,250),xlim=c(0,180)) + ggtitle(paste("Inter-", e, " Interval Histogram | Baseline", sep=""))

# REM - Out of Phase
ggplot(data=inter_state_intervals[type==e & protocol_section == 'fd' & phase_label == 'out_of_phase'], aes(interval_length)) + geom_histogram(binwidth=2) + coord_cartesian(ylim=c(0,250),xlim=c(0,180)) + ggtitle(paste("Inter-", e, " Interval Histogram | ",l, sep=""))


marrangeGrob(ps, ncol=2, nrow=3)


inter_p <- ggplot(data=inter_state_intervals[type%in%c("REM", "SWS")], aes(interval_length)) + scale_x_continuous(breaks=function(x){seq(x[1],x[2],by=60)})
inter_p <- inter_p + geom_histogram(binwidth=1, fill=element_blank()) + coord_cartesian(ylim=c(0, 400), xlim=c(0,180)) + ggtitle("Inter-State Intervals") + facet_grid(type ~ wake_level, scales = "free") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
inter_p


inter_p <- ggplot(data=inter_state_intervals[type%in%c("REM", "SWS")], aes(interval_length)) + scale_x_continuous(breaks=function(x){seq(x[1],x[2],by=60)})
inter_p <- inter_p + geom_histogram(binwidth=1, fill=element_blank()) + coord_cartesian(ylim=c(0, 400), xlim=c(0,180)) + ggtitle("Inter-State Intervals") + facet_grid(type ~ wake_level, scales = "free") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
inter_p

inter_p2 <- ggplot(data=inter_state_intervals[type%in%c("REM", "SWS")], aes(interval_length_without_wake)) + scale_x_continuous(breaks=function(x){seq(x[1],x[2],by=60)})
inter_p2 <- inter_p2 + geom_histogram(binwidth=1, fill=element_blank()) + coord_cartesian(ylim=c(0, 400), xlim=c(0,180)) + ggtitle("Inter-State Intervals - Excluding Wake") + facet_grid(type ~ wake_level) + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
inter_p2


inter_p3 <- ggplot(data=inter_state_intervals[type%in%c("REM", "SWS")], aes(interval_length_wake)) + scale_x_continuous(breaks=function(x){seq(x[1],x[2],by=60)})
inter_p3 <- inter_p3 + geom_histogram(binwidth=1, fill=element_blank()) + coord_cartesian(ylim=c(0, 400), xlim=c(0,120)) + ggtitle("Inter-State Intervals - Only Wake") + facet_grid(type ~ wake_level) + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
inter_p3

grid.arrange(inter_p, inter_p2, inter_p3)


inter_p <- ggplot(data=iri[cycle_number <= 6],aes(i_length))
inter_p + geom_histogram(binwidth=1) + coord_cartesian(ylim=c(0, 100), xlim=c(0,300)) + facet_grid(cycle_number ~ .) + ggtitle("Inter-REM Interval Histogram by NREM Cycle")

# NREM
inter_np <- ggplot(data=ini,aes(i_length))
inter_np <- inter_np + geom_histogram(binwidth=1) + coord_cartesian(ylim=c(0, 1000), xlim=c(0,300)) + ggtitle("Inter-NREM Interval Histogram") + scale_y_log10()

inter_p + geom_histogram(binwidth=.01) + ggtitle("Inter-NREM Interval Histogram") + scale_y_log10() + scale_x_log10()

inter_p <- ggplot(data=ini[cycle_number <= 6],aes(i_length))
inter_p + geom_histogram(binwidth=1) + coord_cartesian(ylim=c(0, 500), xlim=c(0,300)) + facet_grid(cycle_number ~ .) + ggtitle("Inter-NREM Interval Histogram by NREM Cycle")

# WAKE
inter_wp <- ggplot(data=iwi,aes(i_length))
inter_wp <- inter_wp + geom_histogram(binwidth=1) + coord_cartesian(ylim=c(0, 1000), xlim=c(0,300)) + ggtitle("Inter-WAKE Interval Histogram")

inter_p <- ggplot(data=iwi[cycle_number <= 6],aes(i_length))
inter_p + geom_histogram(binwidth=1) + coord_cartesian(ylim=c(0, 500), xlim=c(0,300)) + facet_grid(cycle_number ~ .) + ggtitle("Inter-WAKE Interval Histogram by NREM Cycle")

grid.arrange(inter_rp, inter_np, inter_wp, ncol=3)


## Transition table
transition_heatmap <- function(d, breaks=c(0,1,2,5,10,15,20,30,60,90), from_state="WAKE", ps = "fd") {
  d <- copy(d[protocol_section == ps & prev_label==from_state & tag=='high_res'])
  
  max_l <- max(d$prev_length)+1
  breaks <- c(breaks[breaks < max_l], max_l)
  labels <- paste(breaks[-length(breaks)], breaks[-1L], sep=' to ')
  
  d[,prev_length_label:=cut(prev_length, breaks = breaks, labels = labels, ordered_result = TRUE)]
  
  trans_d <- d[,list(prev_length, label, length, total=.N),by='prev_label,prev_length_label']
  trans_d <- trans_d[,list(count=.N,total=max(total)),by='prev_label,prev_length_label,label']
  trans_d[,prob:=count/total]
  
  ggplot(trans_d, aes(label,prev_length_label)) +
    geom_tile(aes(fill=prob), color="white") + 
    scale_fill_gradient(low="white", high="steelblue") +
    theme(panel.background=element_blank())
    #labs(y=paste("Length of Inter-", d$type, "wake"), x=paste("length of Inter-", d$type, "non-wake")) +
    #scale_x_discrete(breaks=levels(d$heatmap_data$x_bin), labels=d$x_labs) +
    #scale_y_discrete(breaks=levels(d$heatmap_data$y_bin), labels=d$y_labs)
}

transitions <- sequences[tag=='high_res' & label != "UNDEF" & !is.na(prev_label) & prev_label != "UNDEF",list(count=.N),by='prev_label,label,protocol_section']
transitions[,total:=sum(count),by='prev_label,protocol_section']
transitions[,p:=count/total]

null_transitions <- data.table(prev_label=c("N1", "N2", "REM", "SWS", "WAKE"), label=c("N1", "N2", "REM", "SWS", "WAKE"),count=c(0,0,0,0,0), total=c(108455, 137080, 49474, 59164,77204), p=c(0,0,0,0,0))

ggplot(transitions, aes(prev_label, label))


transitions <- rbind(transitions, null_transitions)

setkey(transitions, prev_label, label)

transition_matrix <- matrix(data=transitions$p, nrow=5, ncol=5, dimnames = list(c("N1", "N2", "REM", "SWS", "WAKE"), c("N1", "N2", "REM", "SWS", "WAKE")) , byrow=TRUE)




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
