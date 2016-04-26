library(gridExtra)


# Distribution of sequence lengths - whole night and 1,2,3,4+
# - NREM
# - REM
# - WAKE

seq_len_p <- ggplot(data=e[label=="NREM" & !(prev_label%in%c("NONE", "NREM")) ], aes(x=length))
seq_len_p + geom_histogram(binwidth=2) + facet_grid(label ~ prev_label, scales='free') + coord_cartesian(xlim=c(0,300)) + ggtitle("Distribution of sequence lengths by state") + scale_y_log10()

seq_len_p <- ggplot(data=e[label!="UNDEF"], aes(x=length))
seq_len_p + geom_histogram(binwidth=2) + facet_grid(label ~ ., scales='free') + coord_cartesian(xlim=c(0,300)) + ggtitle("Distribution of sequence lengths by state") + scale_y_log10()


seq_len_p <- ggplot(data=e[label=="NREM" & cycle_number <= 6], aes(x=length))
seq_len_p + geom_histogram(binwidth=1) + facet_grid(cycle_number ~ ., scales='free') + coord_cartesian(xlim=c(0,300)) + ggtitle("Distribution of NREM sequence lengths by state and NREM cycle") + scale_y_log10()

seq_len_p <- ggplot(data=e[label=="REM" & cycle_number <= 6], aes(x=length))
seq_len_p + geom_histogram(binwidth=1) + facet_grid(cycle_number ~ ., scales='free') + coord_cartesian(xlim=c(0,200)) + ggtitle("Distribution of NREM sequence lengths by state and NREM cycle") + scale_y_log10()

seq_len_p <- ggplot(data=e[label=="WAKE" & cycle_number <= 6], aes(x=length))
seq_len_p + geom_histogram(binwidth=1) + facet_grid(cycle_number ~ ., scales='free') + coord_cartesian(xlim=c(0,100)) + ggtitle("Distribution of NREM sequence lengths by state and NREM cycle") + scale_y_log10()

###
# ALL LATENCIES
###

# WAKE
rem_p <- ggplot(data=rem_latencies[label=="WAKE"], aes(x=rem_latency))
rem_p <- rem_p + geom_density() + coord_cartesian(xlim=c(0,250)) + ggtitle("REM latency after WAKE")

nrem_p <- ggplot(data=nrem_latencies[label=="WAKE"], aes(x=nrem_latency))
nrem_p <- nrem_p + geom_density() + coord_cartesian(xlim=c(0,30)) + ggtitle("NREM latency after WAKE")

wake_p <- ggplot(data=wake_latencies[label=="WAKE"], aes(x=wake_latency))
wake_p <- wake_p + geom_density(aes()) + coord_cartesian(xlim=c(0,100)) + ggtitle("WAKE latency after WAKE")

s2_p <- ggplot(data=e[label=="WAKE"], aes(x=stage_2_latency))
s2_p <- s2_p + geom_density(aes()) + coord_cartesian(xlim=c(0,50)) + ggtitle("Stage 2 latency after WAKE")

s3_p <- ggplot(data=e[label=="WAKE"], aes(x=stage_3_latency))
s3_p <- s3_p + geom_density(aes()) + coord_cartesian(xlim=c(0,400)) + ggtitle("Stage 3 latency after WAKE")

grid.arrange(rem_p, nrem_p, wake_p, s2_p, s3_p, nrow=5)

# Cycle Number
rem_p <- ggplot(data=rem_latencies[label=="WAKE" & cycle_number <= 6], aes(x=rem_latency))
rem_p <- rem_p + geom_density() + coord_cartesian(xlim=c(0,250)) + ggtitle("REM latency after WAKE")
rem_p <- rem_p + facet_grid(cycle_number ~ .)

nrem_p <- ggplot(data=nrem_latencies[label=="WAKE" & cycle_number <= 6], aes(x=nrem_latency))
nrem_p <- nrem_p + geom_density() + coord_cartesian(xlim=c(0,30)) + ggtitle("NREM latency after WAKE")
nrem_p <- nrem_p + facet_grid(cycle_number ~ .)

grid.arrange(rem_p, nrem_p, ncol=2)

# NREM

rem_p <- ggplot(data=rem_latencies[label=="NREM"], aes(x=rem_latency))
rem_p + geom_density() + coord_cartesian(xlim=c(0,250)) + ggtitle("REM latency after NREM")

nrem_p <- ggplot(data=nrem_latencies[label=="NREM"], aes(x=nrem_latency))
nrem_p + geom_density() + coord_cartesian(xlim=c(0,30)) + ggtitle("NREM latency after NREM")

wake_p <- ggplot(data=wake_latencies[label=="NREM"], aes(x=wake_latency))
wake_p + geom_density(aes()) + coord_cartesian(xlim=c(0,100)) + ggtitle("WAKE latency after NREM")

s2_p <- ggplot(data=e[label=="NREM"], aes(x=stage_2_latency))
s2_p + geom_density(aes()) + coord_cartesian(xlim=c(0,50)) + ggtitle("Stage 2 latency after NREM")

s3_p <- ggplot(data=e[label=="NREM"], aes(x=stage_3_latency))
s3_p + geom_density(aes()) + coord_cartesian(xlim=c(0,400)) + ggtitle("Stage 3 latency after NREM")

# REM 

rem_p <- ggplot(data=rem_latencies[label=="REM"], aes(x=rem_latency))
rem_p + geom_density() + coord_cartesian(xlim=c(0,250)) + ggtitle("REM latency after REM")

nrem_p <- ggplot(data=nrem_latencies[label=="REM"], aes(x=nrem_latency))
nrem_p + geom_density() + coord_cartesian(xlim=c(0,30)) + ggtitle("NREM latency after REM")

wake_p <- ggplot(data=wake_latencies[label=="REM"], aes(x=wake_latency))
wake_p + geom_density(aes()) + coord_cartesian(xlim=c(0,100)) + ggtitle("WAKE latency after REM")

s2_p <- ggplot(data=e[label=="REM"], aes(x=stage_2_latency))
s2_p + geom_density(aes()) + coord_cartesian(xlim=c(0,50)) + ggtitle("Stage 2 latency after REM")

s3_p <- ggplot(data=e[label=="REM"], aes(x=stage_3_latency))
s3_p + geom_density(aes()) + coord_cartesian(xlim=c(0,400)) + ggtitle("Stage 3 latency after REM")

# Latency by length and preceding sequence
# - REM
# - NREM
# - Stage 2
# - Stage 3
# - WAKE


rem_p1 <- ggplot(data=rem_latencies[label=="WAKE" & prev_label %in% c("REM", "NREM")], aes(x=rem_latency))
rem_p1 <- rem_p1 + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ ., scales = 'free') + coord_cartesian(xlim=c(0,250)) + ggtitle("REM latency after WAKE by preceding state")
rem_p2 <- ggplot(data=rem_latencies[label=="WAKE" & prev_label %in% c("REM", "NREM") & !(length_class %in% c("[0,1]", "(1,2]"))], aes(x=rem_latency))
rem_p2 <- rem_p2 + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ ., scales = 'free') + coord_cartesian(xlim=c(0,250)) + ggtitle("REM latency after WAKE by preceding state (WAKE length > 2 epochs)")


nrem_p1 <- ggplot(data=nrem_latencies[label=="WAKE" & prev_label %in% c("REM", "NREM")], aes(x=nrem_latency))
nrem_p1 <- nrem_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ ., scales = 'free') + coord_cartesian(xlim=c(0,30)) + ggtitle("NREM latency after WAKE by preceding state")
nrem_p2 <- ggplot(data=nrem_latencies[label=="WAKE" & prev_label %in% c("REM", "NREM") & !(length_class %in% c("[0,1]", "(1,2]"))], aes(x=nrem_latency))
nrem_p2 <- nrem_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ ., scales = 'free') + coord_cartesian(xlim=c(0,30)) + ggtitle("REM latency after WAKE by preceding state (WAKE length > 2 epochs)")

wake_p <- ggplot(data=wake_latencies[label=="WAKE" & prev_label %in% c("REM", "NREM")], aes(x=wake_latency))
wake_p <- wake_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ ., scales = 'free') + coord_cartesian(xlim=c(0,50)) + ggtitle("WAKE latency after WAKE by preceding state")

s2_p <- ggplot(data=e[label=="WAKE" & prev_label %in% c("REM", "NREM")], aes(x=stage_2_latency))
s2_p <- s2_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ ., scales = 'free') + coord_cartesian(xlim=c(0,200)) + ggtitle("Stage 2 latency after WAKE by preceding state")
s2_p


s3_p <- ggplot(data=e[label=="WAKE" & prev_label %in% c("REM", "NREM")], aes(x=stage_3_latency))
s3_p <- s3_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ ., scales = 'free') + coord_cartesian(xlim=c(0,400)) + ggtitle("Stage 3 latency after WAKE by preceding state")

grid.arrange(rem_p1, rem_p2, ncol=2)
grid.arrange(nrem_p1, nrem_p2, ncol=2)
grid.arrange(wake_p, s2_p, s3_p, ncol=3)
# Latency by length and episode type
# - REM
# - NREM
# - Stage 2
# - Stage 3
# - WAKE


rem_p <- ggplot(data=rem_latencies[label=="WAKE" & episode_type %in% c("REM", "NREM","WAKE")], aes(x=rem_latency))
rem_p + geom_density(aes(color=length_class)) + facet_grid(episode_type ~ ., scales = 'free') + coord_cartesian(xlim=c(0,250)) + ggtitle("REM latency after WAKE by episode type")

rem_p <- ggplot(data=rem_latencies[label=="WAKE" & episode_type %in% c("REM", "NREM","WAKE") & !(length_class %in% c("[0,1]", "(1,2]"))], aes(x=rem_latency))
rem_p + geom_density(aes(color=length_class)) + facet_grid(episode_type ~ ., scales = 'free') + coord_cartesian(xlim=c(0,250)) + ggtitle("REM latency after WAKE by episode type (WAKE length > 2 epochs)")


nrem_p <- ggplot(data=nrem_latencies[label=="WAKE" & episode_type %in% c("REM", "NREM","WAKE")], aes(x=nrem_latency))
nrem_p + geom_density(aes(color=length_class)) + facet_grid(episode_type ~ ., scales = 'free') + coord_cartesian(xlim=c(0,30)) + ggtitle("NREM latency after WAKE by episode type")
nrem_p <- ggplot(data=nrem_latencies[label=="WAKE" & episode_type %in% c("REM", "NREM","WAKE") & !(length_class %in% c("[0,1]", "(1,2]"))], aes(x=nrem_latency))
nrem_p + geom_density(aes(color=length_class)) + facet_grid(episode_type ~ ., scales = 'free') + coord_cartesian(xlim=c(0,30)) + ggtitle("REM latency after WAKE by episode type (WAKE length > 2 epochs)")

wake_p <- ggplot(data=wake_latencies[label=="WAKE" & episode_type %in% c("REM", "NREM","WAKE")], aes(x=wake_latency))
wake_p + geom_density(aes(color=length_class)) + facet_grid(episode_type ~ ., scales = 'free') + coord_cartesian(xlim=c(0,50)) + ggtitle("WAKE latency after WAKE by episode type")

s2_p <- ggplot(data=e[label=="WAKE" & episode_type %in% c("REM", "NREM","WAKE")], aes(x=stage_2_latency))
s2_p + geom_density(aes(color=length_class)) + facet_grid(episode_type ~ ., scales = 'free') + coord_cartesian(xlim=c(0,50)) + ggtitle("Stage 2 latency after WAKE by episode type")

s3_p <- ggplot(data=e[label=="WAKE" & episode_type %in% c("REM", "NREM","WAKE")], aes(x=stage_3_latency))
s3_p + geom_density(aes(color=length_class)) + facet_grid(episode_type ~ ., scales = 'free') + coord_cartesian(xlim=c(0,400)) + ggtitle("Stage 3 latency after WAKE by episode type")

# Latency by length and night location
# - REM
# - NREM
# - Stage 2
# - Stage 3
# - WAKE

rem_p <- ggplot(data=rem_latencies[label=="WAKE" & cycle_number < 6 ], aes(x=rem_latency))
rem_p + geom_density(aes(color=length_class)) + facet_grid(cycle_number ~ ., scales = 'free') + coord_cartesian(xlim=c(0,250)) + ggtitle("REM latency after WAKE by NREM cycle and previous state")
rem_p <- ggplot(data=rem_latencies[label=="WAKE" & cycle_number < 6 & !(length_class %in% c("[0,1]", "(1,2]"))], aes(x=rem_latency))
rem_p + geom_density(aes(color=length_class)) + facet_grid(cycle_number ~ ., scales = 'free') + coord_cartesian(xlim=c(0,250)) + ggtitle("REM latency after WAKE by NREM cycle (WAKE length > 2 epochs)")


nrem_p <- ggplot(data=nrem_latencies[label=="WAKE" & cycle_number < 6], aes(x=nrem_latency))
nrem_p + geom_density(aes(color=length_class)) + facet_grid(cycle_number ~ ., scales = 'free') + coord_cartesian(xlim=c(0,30)) + ggtitle("NREM latency after WAKE by NREM cycle")
nrem_p <- ggplot(data=nrem_latencies[label=="WAKE" & cycle_number < 6 & !(length_class %in% c("[0,1]", "(1,2]"))], aes(x=nrem_latency))
nrem_p + geom_density(aes(color=length_class)) + facet_grid(cycle_number ~ ., scales = 'free') + coord_cartesian(xlim=c(0,30)) + ggtitle("REM latency after WAKE by NREM cycle (WAKE length > 2 epochs)")

wake_p <- ggplot(data=wake_latencies[label=="WAKE" & cycle_number < 6], aes(x=wake_latency))
wake_p + geom_density(aes(color=length_class)) + facet_grid(cycle_number ~ ., scales = 'free') + coord_cartesian(xlim=c(0,50)) + ggtitle("WAKE latency after WAKE by NREM cycle")

s2_p <- ggplot(data=e[label=="WAKE" & cycle_number < 6], aes(x=stage_2_latency))
s2_p + geom_density(aes(color=length_class)) + facet_grid(cycle_number ~ ., scales = 'free') + coord_cartesian(xlim=c(0,50)) + ggtitle("Stage 2 latency after WAKE by NREM cycle")

s3_p <- ggplot(data=e[label=="WAKE" & cycle_number < 6], aes(x=stage_3_latency))
s3_p + geom_density(aes(color=length_class)) + facet_grid(cycle_number ~ ., scales = 'free') + coord_cartesian(xlim=c(0,400)) + ggtitle("Stage 3 latency after WAKE by NREM cycle")

# Latency by length and NREM cycle and prev state
# - REM
# - NREM
# - Stage 2
# - Stage 3
# - WAKE

rem_p <- ggplot(data=rem_latencies[label=="WAKE" & cycle_number < 6 & prev_label %in% c("REM", "NREM")], aes(x=rem_latency))
rem_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ cycle_number, scales = 'free') + coord_cartesian(xlim=c(0,250)) + ggtitle("REM latency after WAKE by NREM cycle and previous state")

nrem_p <- ggplot(data=nrem_latencies[label=="WAKE" & cycle_number < 6 & prev_label %in% c("REM", "NREM")], aes(x=nrem_latency))
nrem_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ cycle_number, scales = 'free') + coord_cartesian(xlim=c(0,30)) + ggtitle("NREM latency after WAKE by episode type")

wake_p <- ggplot(data=wake_latencies[label=="WAKE" & cycle_number < 6 & prev_label %in% c("REM", "NREM")], aes(x=wake_latency))
wake_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ cycle_number, scales = 'free') + coord_cartesian(xlim=c(0,50)) + ggtitle("WAKE latency after WAKE by episode type")

s2_p <- ggplot(data=e[label=="WAKE" & cycle_number < 6 & prev_label %in% c("REM", "NREM")], aes(x=stage_2_latency))
s2_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ cycle_number, scales = 'free') + coord_cartesian(xlim=c(0,50)) + ggtitle("Stage 2 latency after WAKE by episode type")

s3_p <- ggplot(data=e[label=="WAKE" & cycle_number < 6 & prev_label %in% c("REM", "NREM")], aes(x=stage_3_latency))
s3_p + geom_density(aes(color=length_class)) + facet_grid(prev_label ~ cycle_number, scales = 'free') + coord_cartesian(xlim=c(0,400)) + ggtitle("Stage 3 latency after WAKE by episode type")



####
# INTER-state Intervals
####

# REM
inter_rp <- ggplot(data=iri,aes(i_length))
inter_rp <- inter_rp + geom_histogram(binwidth=1) + coord_cartesian(ylim=c(0, 1000), xlim=c(0,300)) + ggtitle("Inter-REM Interval Histogram")

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
