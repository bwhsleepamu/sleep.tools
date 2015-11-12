library(gridExtra)
source("R/plotting/rasters/sequence_raster_plot.R")

# Mini
plot_raster("3319GX", plot_double=FALSE, labels=FALSE)
# One day
plot_raster("3319GX", plot_double=FALSE, first_day = 28, number_of_days = 1)

sequences$label <- sequences$old_label
sequences$label_f <- as.factor(sequences$label)
sequences$label_f <- factor(sequences$label_f, levels=c("WAKE", "REM", "NREM", "N1", "N2", "SWS", "UNDEF"))

sequences$label_f <- factor(unique(sequences$label), levels=c("WAKE", "REM", "NREM", "N1", "N2", "SWS", "UNDEF"))

sequences$old_label <- sequences$label  
sequences$label <- sequences$label_f

# Distribution of sequence lengths - whole night and 1,2,3,4+
# - NREM
# - REM
# - WAKE
  
  
seq_len_p <- ggplot(data=sequences[(label!="UNDEF" & tag=="high_res") | (tag=="normal" & label=="NREM")], aes(x=length)) + labs(x="Length (minutes)", y="") + scale_x_continuous(breaks=function(x){seq(x[1],x[2],by=60)})
seq_len_p + geom_histogram(binwidth=1) + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + facet_wrap(~ label_f, as.table=TRUE, ncol=3, scales='free') + coord_cartesian(xlim=c(0,300)) + ggtitle("Distribution of Sequence Lengths by State") + scale_y_continuous(trans='log1p')

seq_len_p1 <- ggplot(data=sequences[label%in% c("N1", "N2", "SWS")], aes(x=length,fill=label))
seq_len_p1 <- seq_len_p1 + geom_histogram(binwidth=1) + coord_cartesian(xlim=c(0,200)) + ggtitle("Distribution of NREM stage sequences") + scale_y_continuous(trans='log1p')

seq_len_p2 <- ggplot(data=sequences[label%in% c("NREM")], aes(x=length, fill=label))
seq_len_p2 <- seq_len_p2 + geom_histogram(binwidth=1) + coord_cartesian(xlim=c(0,200)) + ggtitle("Distribution of NREM stage sequences") + scale_y_continuous(trans='log1p')

marrangeGrob(list(seq_len_p1, seq_len_p2), nrow=2, ncol=1)

seq_len_p <- ggplot(data=sequences_with_latency[label!="UNDEF" & !is.na(phase_label)], aes(x=length))
seq_len_p + geom_histogram(binwidth=2) + facet_grid(label ~ phase_label, scales='free') + coord_cartesian(xlim=c(0,400)) + ggtitle("Distribution of Sequence Length by State and Phase of Sleep Episode") + scale_y_continuous(trans='log1p')


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

ps <- lapply(l, function(t){
  p <- ggplot(data=sequences[tag == "high_res" & label=="WAKE" & prev_label %in% c("REM", "N1", "N2", "SWS")]) + scale_colour_manual(values=cbbbPalette) + theme(axis.text.y = element_blank(), axis.ticks.y=element_blank()) + scale_x_continuous(breaks=function(x){seq(x[1],x[2],by=60)})
  p <- p + geom_density(aes_string(x=paste(t,"latency", sep="_"), color="length_class")) + facet_grid(prev_label ~ ., scales = 'free') + coord_cartesian(xlim=c(0,240)) + ggtitle(paste(t, "latency after WAKE by preceding state",sep=" "))
  p  
})

grid.arrange(arrangeGrob(grobs=lapply(ps, function(x){x + theme(legend.position="none")})), myleg, ncol=2, widths=c(10,1))

# rem_p1 <- rem_p1 + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ phase_label, scales = 'free') + coord_cartesian(xlim=c(0,250)) + ggtitle("REM latency after WAKE by preceding state")
# rem_p1
# 
# 
# nrem_p1 <- ggplot(data=sequences_with_latency[label=="WAKE" & prev_label %in% c("REM", "NREM") & phase_label %in% c("in_phase", "out_of_phase")], aes(x=nrem_latency))
# nrem_p1 <- nrem_p1 + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ phase_label, scales = 'free') + coord_cartesian(xlim=c(0,30)) + ggtitle("NREM latency after WAKE by preceding state")
# nrem_p1
# 
# 
# p <- ggplot(data=sequences_with_latency[label=="WAKE" & prev_label %in% c("REM", "WAKE") & phase_label %in% c("in_phase", "out_of_phase")], aes(x=rem_latency))
# p <- p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ phase_label, scales = 'free') + coord_cartesian(xlim=c(0,100)) + ggtitle("Latency after WAKE by preceding state")
# p



# Latency by length and night location
# - REM
# - NREM
# - Stage 2
# - Stage 3
# - WAKE

# rem_p <- ggplot(data=rem_latencies[label=="WAKE" & cycle_number < 6 ], aes(x=rem_latency))
# rem_p + geom_density(aes(color=length_class)) + facet_grid(cycle_number ~ ., scales = 'free') + coord_cartesian(xlim=c(0,250)) + ggtitle("REM latency after WAKE by NREM cycle and previous state")
# rem_p <- ggplot(data=rem_latencies[label=="WAKE" & cycle_number < 6 & !(length_class %in% c("[0,1]", "(1,2]"))], aes(x=rem_latency))
# rem_p + geom_density(aes(color=length_class)) + facet_grid(cycle_number ~ ., scales = 'free') + coord_cartesian(xlim=c(0,250)) + ggtitle("REM latency after WAKE by NREM cycle (WAKE length > 2 epochs)")
# 
# 
# nrem_p <- ggplot(data=nrem_latencies[label=="WAKE" & cycle_number < 6], aes(x=nrem_latency))
# nrem_p + geom_density(aes(color=length_class)) + facet_grid(cycle_number ~ ., scales = 'free') + coord_cartesian(xlim=c(0,30)) + ggtitle("NREM latency after WAKE by NREM cycle")
# nrem_p <- ggplot(data=nrem_latencies[label=="WAKE" & cycle_number < 6 & !(length_class %in% c("[0,1]", "(1,2]"))], aes(x=nrem_latency))
# nrem_p + geom_density(aes(color=length_class)) + facet_grid(cycle_number ~ ., scales = 'free') + coord_cartesian(xlim=c(0,30)) + ggtitle("REM latency after WAKE by NREM cycle (WAKE length > 2 epochs)")
# 
# wake_p <- ggplot(data=wake_latencies[label=="WAKE" & cycle_number < 6], aes(x=wake_latency))
# wake_p + geom_density(aes(color=length_class)) + facet_grid(cycle_number ~ ., scales = 'free') + coord_cartesian(xlim=c(0,50)) + ggtitle("WAKE latency after WAKE by NREM cycle")
# 
# s2_p <- ggplot(data=e[label=="WAKE" & cycle_number < 6], aes(x=stage_2_latency))
# s2_p + geom_density(aes(color=length_class)) + facet_grid(cycle_number ~ ., scales = 'free') + coord_cartesian(xlim=c(0,50)) + ggtitle("Stage 2 latency after WAKE by NREM cycle")
# 
# s3_p <- ggplot(data=e[label=="WAKE" & cycle_number < 6], aes(x=stage_3_latency))
# s3_p + geom_density(aes(color=length_class)) + facet_grid(cycle_number ~ ., scales = 'free') + coord_cartesian(xlim=c(0,400)) + ggtitle("Stage 3 latency after WAKE by NREM cycle")
# 
# # Latency by length and NREM cycle and prev state
# # - REM
# # - NREM
# # - Stage 2
# # - Stage 3
# # - WAKE
# 
# rem_p <- ggplot(data=rem_latencies[label=="WAKE" & cycle_number < 6 & prev_label %in% c("REM", "NREM")], aes(x=rem_latency))
# rem_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ cycle_number, scales = 'free') + coord_cartesian(xlim=c(0,250)) + ggtitle("REM latency after WAKE by NREM cycle and previous state")
# 
# nrem_p <- ggplot(data=nrem_latencies[label=="WAKE" & cycle_number < 6 & prev_label %in% c("REM", "NREM")], aes(x=nrem_latency))
# nrem_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ cycle_number, scales = 'free') + coord_cartesian(xlim=c(0,30)) + ggtitle("NREM latency after WAKE by episode type")
# 
# wake_p <- ggplot(data=wake_latencies[label=="WAKE" & cycle_number < 6 & prev_label %in% c("REM", "NREM")], aes(x=wake_latency))
# wake_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ cycle_number, scales = 'free') + coord_cartesian(xlim=c(0,50)) + ggtitle("WAKE latency after WAKE by episode type")
# 
# s2_p <- ggplot(data=e[label=="WAKE" & cycle_number < 6 & prev_label %in% c("REM", "NREM")], aes(x=stage_2_latency))
# s2_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ cycle_number, scales = 'free') + coord_cartesian(xlim=c(0,50)) + ggtitle("Stage 2 latency after WAKE by episode type")
# 
# s3_p <- ggplot(data=e[label=="WAKE" & cycle_number < 6 & prev_label %in% c("REM", "NREM")], aes(x=stage_3_latency))
# s3_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ cycle_number, scales = 'free') + coord_cartesian(xlim=c(0,400)) + ggtitle("Stage 3 latency after WAKE by episode type")
# 


####
# INTER-state Intervals
####
inter_state_intervals$interval_length


p <- ggplot(data=inter_state_intervals[type=="REM" & !is.na(phase_label)], aes(x=interval_length_in_epochs)) + geom_density(aes(color=phase_label)) + coord_cartesian(xlim=c(0,300))
p

# REM
l <- c("REM", "WAKE", "NREM", "N1", "N2", "SWS")

ps <- lapply(l, function(e){
  p <- ggplot(data=inter_state_intervals[type==e], aes(interval_length))
  p <- p + geom_histogram(binwidth=1) + coord_cartesian(ylim=c(0, 1000), xlim=c(0,300)) + ggtitle(paste("Inter-", e, " Interval Histogram", sep=""))
  p
})

marrangeGrob(ps, ncol=2, nrow=3)




inter_p <- ggplot(data=inter_state_intervals[type%in%c("REM", "SWS")], aes(interval_length)) + scale_x_continuous(breaks=function(x){seq(x[1],x[2],by=60)})
inter_p <- inter_p + geom_histogram(binwidth=1, fill=element_blank()) + coord_cartesian(ylim=c(0, 400), xlim=c(0,180)) + ggtitle("Inter-State Intervals") + facet_grid(type ~ wake_level) + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
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

transitions <- sequences[tag=='high_res' & label != "UNDEF" & !is.na(prev_label) & prev_label != "UNDEF",list(count=.N),by='prev_label,label']
transitions[,total:=sum(count),by='prev_label']
transitions[,p:=count/total]

null_transitions <- data.table(prev_label=c("N1", "N2", "REM", "SWS", "WAKE"), label=c("N1", "N2", "REM", "SWS", "WAKE"),count=c(0,0,0,0,0), total=c(108455, 137080, 49474, 59164,77204), p=c(0,0,0,0,0))
transitions <- rbind(transitions, null_transitions)

setkey(transitions, prev_label, label)

transition_matrix <- matrix(data=transitions$p, nrow=5, ncol=5, dimnames = list(c("N1", "N2", "REM", "SWS", "WAKE"), c("N1", "N2", "REM", "SWS", "WAKE")) , byrow=TRUE)

## Hazard Functions