library(ggthemes)
library(fitdistrplus)
library(gridExtra)
source("R/sources.R")
source("R/plotting/rasters/sequence_raster_plot.R")

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
plot_single_day_raster("3450GX", time_range=c(0,13.0), day_number=3, plot_double=FALSE, labels = FALSE)

## Figure 7
interRemInterval <- function() {
  p <- ggplot(data=inter_state_intervals[type=="REM"], aes(x=interval_length)) + 
    geom_histogram(binwidth=1, aes(y=..count..+1))  + 
    scale_y_log10(breaks=trans_breaks(log10, function(x){ 10^x }), labels = trans_format("log10", scales::math_format(10^.x))) + 
    coord_cartesian(xlim=c(0,180)) +
    scale_x_continuous(breaks=circ_breaks(), expand = c(0,0)) +
    theme_tufte(base_size = 32, base_family="helvetica") + scale_colour_few() + 
    labs(x="Inter-REM Interval Length (minutes)", y="Bin Count")
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


## Definition Image
definitionPlot <- function() {
  setup_raster_data(sleep_data, episodes, cycles, melatonin_phase, normalize_labtime=TRUE, plot_double=FALSE)
  plot_raster("3450GX", plot_double=FALSE, labels = FALSE)
}



