library(gridExtra)


# Distribution of sequence lengths - whole night and 1,2,3,4+
# - NREM
# - REM
# - WAKE
seq

seq_len_p <- ggplot(data=sequences[(label!="UNDEF" & tag=="high_res") | (tag=="normal" & label=="NREM")], aes(x=length))
seq_len_p + geom_histogram(binwidth=1) + facet_grid(label ~ ., scales='free') + coord_cartesian(xlim=c(0,300)) + ggtitle("Distribution of sequence lengths by state") + scale_y_continuous(trans='log1p')

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


l <- c("REM", "NREM", "WAKE", "N2", "SWS", "SWS")

ps <- lapply(l, function(t){
  p <- ggplot(data=sequences[tag == "high_res" & label=="WAKE" & prev_label %in% c("REM", "N1", "N2", "SWS")])
  p <- p + geom_density(aes_string(x=paste(t,"latency", sep="_"), color="length_class")) + facet_grid(prev_label ~ ., scales = 'free') + coord_cartesian(xlim=c(0,250)) + ggtitle(paste(t, "latency after WAKE by preceding state",sep=" "))
  p  
})

marrangeGrob(ps, ncol=2, nrow=3)

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



inter_rp <- ggplot(data=inter_state_intervals[type=="REM" & percent_wake >= 0.1], aes(interval_length))
inter_rp <- inter_rp + geom_histogram(binwidth=1) + coord_cartesian(ylim=c(0, 1000), xlim=c(0,300)) + ggtitle("Inter-REM Interval Histogram")
inter_rp



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
