## For periods:
### NEED: 
#DONE #### Agreement stats by subject and or subject group FOR EACH METHOD 
#### Number of periods per sleep episode for each period type by each method
#### Distribution of period lengths for each type by subject and or subject group for each method
#### OTHER COMPARISONS??

## For cycles:
### NEED: distribution of lengths and counts for 1st, 2nd...cycles by subject and/or subject group FOR EACH METHOD
### Possible others: start times by cycle, 



young_s <- subjects[study=="NIAPPG" & age_group=='Y']
old_s <- subjects[study=="NIAPPG" & age_group=='O']
csr_s <- subjects[study=="T20CSR-CSR"]
control_s <- subjects[study=="T20CSR-Control"]


## Agreement
function() {
  # Cleaning:
  ## No sleep periods
  clean.periods <- copy(periods)
  clean.periods <- clean.periods[sleep_wake_period>0]
  ## Strip wake at beginning and end of sleep periods
  clean.periods <- clean.periods[,strip_wake(.SD),by='subject_code,sleep_wake_period']
  clean.periods[,pik:=.I]
  
  clean.periods[,agreement:=sum(sleep_data[start_position:end_position]$epoch_type == label)/length,by='pik']
  
  ## Agreement graph
  p <- ggplot(clean.periods[label %in% c('NREM', 'REM')], aes(x=agreement, color=method))
  p <- p + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  p <- p + ggtitle("Agreement within Periods by Method")
  p <- p + xlab("Agreement")
  p <- p + facet_grid(label ~ ., scales='free_y')
  p <- p + geom_density(alpha=.3)
  p
  
  ggsave("/home/pwm4/Desktop/Visualizations/agreement_full.svg", scale=2, width=9, height=4)
  
  
  # Young
  p <- ggplot(clean.periods[label %in% c('NREM', 'REM') & subject_code %in% young_s$subject_code], aes(x=agreement, color=method))
  p <- p + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  p <- p + ggtitle("Agreement within Periods by Method\nYounger Subjects")
  p <- p + xlab("Agreement")
  p <- p + facet_grid(label ~ ., scales='free_y')
  p <- p + geom_density(alpha=.3)
  p
  
  ggsave("/home/pwm4/Desktop/Visualizations/agreement_young.svg", scale=2, width=9, height=4)
  
  # Old
  p <- ggplot(clean.periods[label %in% c('NREM', 'REM') & subject_code %in% old_s$subject_code], aes(x=agreement, color=method))
  p <- p + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  p <- p + ggtitle("Agreement within Periods by Method\nOlder Subjects")
  p <- p + xlab("Agreement")
  p <- p + facet_grid(label ~ ., scales='free_y')
  p <- p + geom_density(alpha=.3)
  p
  
  ggsave("/home/pwm4/Desktop/Visualizations/agreement_old.svg", scale=2, width=9, height=4)
  
  
  # CSR
  p <- ggplot(clean.periods[label %in% c('NREM', 'REM') & subject_code %in% csr_s$subject_code], aes(x=agreement, color=method))
  p <- p + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  p <- p + ggtitle("Agreement within Periods by Method\nChronic Sleep Restriction")
  p <- p + xlab("Agreement")
  p <- p + facet_grid(label ~ ., scales='free_y')
  p <- p + geom_density(alpha=.3)
  p
  
  ggsave("/home/pwm4/Desktop/Visualizations/agreement_csr.svg", scale=2, width=9, height=4)
  
  # NON-CSR
  p <- ggplot(clean.periods[label %in% c('NREM', 'REM') & subject_code %in% control_s$subject_code], aes(x=agreement, color=method))
  p <- p + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  p <- p + ggtitle("Agreement within Periods by Method\nControl")
  p <- p + xlab("Agreement")
  p <- p + facet_grid(label ~ ., scales='free_y')
  p <- p + geom_density(alpha=.3)
  p
  
  ggsave("/home/pwm4/Desktop/Visualizations/agreement_control.svg", scale=2, width=9, height=4)
  
}

function() {
  epoch_length <- EPOCH_LENGTH
  # period stats
  #period_counts <- clean.periods[,list(nrem_count=sum(label=='NREM'),rem_count=sum(label=='REM'),wake_count=sum(label=='WAKE')),by='subject_code,sleep_wake_period,method']
  period_counts <- copy(clean.periods)
  period_counts <- period_counts[,list(count=.N),by='subject_code,sleep_wake_period,method,label']
  
  
  plot.period_counts(period_counts, subjects, "period_counts_all", "All Subjects")
  plot.period_counts(period_counts, young_s, "period_counts_young", "Younger Subjects")
  plot.period_counts(period_counts, old_s, "period_counts_old", "Older Subjects")
  plot.period_counts(period_counts, control_s, "period_counts_control", "Control Subjects")
  plot.period_counts(period_counts, csr_s, "period_counts_csr", "Chronic Sleep Restriction Subjects")
  
  ## Length distribution of periods
  period_lengths <- copy(clean.periods)
  period_lengths[,length:=length*epoch_length*60]
  period_lengths[,period_number:=seq(.N),by='subject_code,sleep_wake_period,method,label']
  

  plot.period_lengths(period_lengths, subjects, "period_lengths_all", "All Subjects")
  plot.period_lengths(period_lengths, young_s, "period_lengths_young", "Younger Subjects")
  plot.period_lengths(period_lengths, old_s, "period_lengths_old", "Older Subjects")
  plot.period_lengths(period_lengths, control_s, "period_lengths_control", "Control Subjects")
  plot.period_lengths(period_lengths, csr_s, "period_lengths_csr", "Chronic Sleep Restriction Subjects")
  
}

plot.period_counts <- function(period_counts, subject_group, file_name, label) {
  ## Number of periods of each type per sleep episode
  p <- ggplot(period_counts[label %in% c('NREM', 'REM') & subject_code %in% subject_group$subject_code], aes(x=count, fill=method, color=method))
  p <- p + ggtitle(paste("Periods per Sleep Episode", label, sep="\n"))
  p <- p + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)                 
  p <- p + facet_grid(label ~ ., scales='free')
  p <- p + geom_freqpoly(binwidth=1, origin=-0.5)
  
  #p + geom_histogram(binwidth=1)  
  
  ggsave(paste("/home/pwm4/Desktop/Visualizations/", file_name, '.svg', sep=''), scale=2, width=9, height=4)
  
}

plot.period_lengths <- function(period_lengths, subject_group, file_name, label, by_period_count=FALSE, show_outliers=FALSE) {
  pl <- period_lengths[label %in% c('NREM', 'REM') & subject_code %in% subject_group$subject_code]
  ul <- unique(pl[method=='classic']$period_number)
  
  
  p <- ggplot(pl[period_number %in% ul], aes(factor(period_number), length, color=method))
  
  
  
  #p <- ggplot(period_lengths[label %in% c('NREM', 'REM') & subject_code %in% subject_group$subject_code], aes(method, length, color=method))
  p <- p + ggtitle(paste("Distribution of Period Lengths", label, sep="\n"))                   
  p <- p + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  p <- p + facet_grid(label ~ .)
  
  p <- p + geom_boxplot(outlier.shape = NA) + scale_y_continuous(limits = quantile(period_lengths$length, c(0.1, 0.9)))

  ggsave(paste("/home/pwm4/Desktop/Visualizations/", file_name, '.svg', sep=''), scale=2, width=9, height=4)
}



## Cycles
function() {
  nrem_cycles.s <- copy(nrem_cycles)
  nrem_cycles.s[,length:=length*epoch_length*60]
  
#  p <- ggplot(nrem_cycles.s, aes(factor(cycle_number), length, color=method))
#  p + geom_boxplot()

  cycle_counts <- nrem_cycles[,list(count=.N),by='subject_code,sleep_wake_period,method']
  
 # p <- ggplot(cycle_counts, aes(x=count, fill=method, color=method))
  #p <- p + facet_grid(label ~ ., scales='free')
  #p <- p + scale_x_discrete()
  #p + geom_freqpoly(binwidth=1)
  plot.cycles(nrem_cycles.s, cycle_counts, subjects, 'all')
  plot.cycles(nrem_cycles.s, cycle_counts, old_s, 'older')
  plot.cycles(nrem_cycles.s, cycle_counts, young_s, 'younger')
  plot.cycles(nrem_cycles.s, cycle_counts, csr_s, 'csr')
  plot.cycles(nrem_cycles.s, cycle_counts, control_s, 'control')

}

plot.cycles <- function(nrem_cycles, cycle_counts, subject_group, label, by_cycle_count = FALSE) {
  p <- ggplot(nrem_cycles[subject_code %in% subject_group$subject_code], aes(factor(cycle_number), length, color=method))
  p <- p + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  p <- p + ggtitle(paste("Distribution of Lengths of NREM Cycles by Method", label, sep="\n")) + ylab("Length (minutes)") + xlab("Cycle Number")
  p <- p + geom_boxplot(outlier.shape = NA) + scale_y_continuous(limits = quantile(nrem_cycles$length, c(0.1, 0.9)))
  ggsave(plot = p, filename=paste("/home/pwm4/Desktop/Visualizations/cycle_length_", label, '.svg', sep=''), scale=2, width=9, height=4)
  
  
  cycle_counts <- nrem_cycles[,list(count=.N),by='subject_code,sleep_wake_period,method']
  p <- ggplot(cycle_counts[subject_code %in% subject_group$subject_code], aes(x=count, fill=method, color=method))
  p <- p + scale_fill_manual(values=cbbPalette) + scale_colour_manual(values=cbbPalette)
  p <- p + geom_freqpoly(binwidth=1, origin=-.5)
  p <- p + ggtitle(paste("Number of NREM cycles per Sleep Episode", label, sep="\n")) + ylab("Number of Sleep Episodes") + xlab("Number of NREM Cycles")
  p <- p + scale_x_continuous(breaks=seq(from=0, to=14), limits=c(0,14))
  ggsave(plot = p, filename=paste("/home/pwm4/Desktop/Visualizations/cycle_counts_", label, '.svg', sep=''), scale=2, width=9, height=4)  
}

tmp <- function(lim) {
  seq(from=lim[1], to=lim[2], by=1)
}
#periods
function(){
  wd <- periods[subject_code=="3335GX" & sleep_wake_period==2]
  
  tst <- wd[method=='changepoint']
  
  # Cleaning:
  ## No sleep periods
  clean.periods <- periods[sleep_wake_period>0]
  ## Strip wake at beginning and end of sleep periods
  clean.periods <- clean.periods[,strip_wake(.SD),by='subject_code,sleep_wake_period']
  
  psts <- clean.periods[,list(count=length(length)),by='subject_code,sleep_wake_period,method,label']
  
  psts[, mean(NREM_count), by='method']
  
  
  
  plot <- ggplot(psts, aes(method, count))
  plot <- plot + facet_wrap(~ label)
  plot + geom_boxplot()
  
  
  plot <- ggplot(psts, aes(x=count))
  plot <- plot + facet_grid(method ~ label)
  plot <- plot + geom_histogram()
  
  plot
  
  #clean.periods[,table(label,method), by='subject_code,sleep_wake_period']
  
  #summary(table(clean.periods$method,clean.periods$label,clean.periods$subject_code,clean.periods$sleep_wake_period))
  
}

## Stats for number of periods
period_stats <- function(labels) {
  t <- table(labels)
  list(NREM_count=t[names(t)=='NREM'], REM_count=t[names(t)=='REM'],WAKE_count=t[names(t)=='WAKE'], total=length(labels))  
}


## Strips start and end wake off
strip_wake <- function(dt) {
  
  sleep_onset <- min(match(c("NREM", "REM"), dt$label, nomatch=1L))
  sleep_end <- (length(dt$label) - min(match(c("NREM", "REM"), rev(dt$label), nomatch=1L))) + 1L
  
  dt[sleep_onset:sleep_end]
}