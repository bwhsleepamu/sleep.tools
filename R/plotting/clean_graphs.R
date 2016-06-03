library(ggthemes)

## Figure 1: Distribution of Sequence Lengths by State
distOfSeqLengthByState <- function() {
  source("R/plotting/plot_theme.R")
  
  plot <- ggplot(data=sequences[label!="UNDEF" & !is.na(phase_label)], aes(x=length))
  plot <- plot + geom_histogram(binwidth=2) + facet_wrap(~ label, scales='fixed', nrow=2) + coord_cartesian(xlim=c(0,300))
  plot <- plot + ggtitle(element_blank()) + scale_y_continuous(trans='log1p')
  plot + theme_Publication(base_size = 16) + scale_colour_Publication()
}

