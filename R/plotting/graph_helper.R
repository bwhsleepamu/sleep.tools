## Fig 8 Helpers - REM dist

# Compute Distributions
computeIsiDist <- function(d, l, to_graph) {
  #cat(l)
  
  s_d <- d[interval_length_wake_label == l]
  #cat(paste(nrow(s_d),l,sep=" | "))
  min_bin <- min(s_d[[to_graph]], na.rm = TRUE)
  
  s_d <- s_d[get(to_graph) >= (15 + min_bin) & get(to_graph) <= (120 + min_bin)]
  r <- fitdist(s_d[[to_graph]], 'norm')
  
  cat(paste("N: ", nrow(s_d)))
  
  peak_lengths[[l]] <<- r$estimate[1]
  avg_wakes[[l]] <<- mean(s_d$interval_length_wake)
  
  as.list(r$estimate)
}

# Plot Distributions
plotIsiDist <- function(isi_data, wake_label, ex, oh, to_graph, bw=1) {
  #l <- dist_dt[$l
  #p <- tddd$p
  #ex <- tddd$mean
  #oh <- tddd$sd
  
  mynorm <- function(x, mean=0, sd=1, log=FALSE, c = .5){
    cat(c)
    dnorm(x,mean,sd,log) * c
  }
  
  plot <- ggplot(data=isi_data[interval_length_wake_label==wake_label], aes_string(to_graph)) + 
    geom_histogram(binwidth=bw, aes(y=..density..))  + 
    #ggtitle(paste(wake_label, "Minutes of Wake")) + 
    theme_Publication(base_size = 30, base_family="helvetica") + scale_colour_few() + 
    #theme(axis.title=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
    labs(x="Inter-REM Interval Length (minutes)", y="Density") + 
    scale_x_continuous(expand = c(0,1), breaks=circ_breaks())
  
  gdata <- as.data.table(ggplot_build(copy(plot))$data[[1]])
  ylimit <- max(gdata[x > 20]$density) * 1.3
  
  #plot <- plot + 
  my_c <- (0.8 * ylimit)/.024
  cat(my_c)
  plot <- plot + scale_y_continuous(expand=c(0, 0)) + #, limits=c(0, ylimit))
    stat_function(fun=mynorm, colour="red", args=list(mean=as.numeric(copy(ex)), sd=as.numeric(copy(oh)), c = my_c)) + 
    
    coord_cartesian(xlim=c(0,150), ylim=c(0, ylimit))
    
  
  #cat(paste(wake_label,ex,oh,sep=" "))
  
  
  cat('\n')
  
  histograms[[paste(wake_label,sep=" ")]] <<- plot
  NULL
}   



plotIsiHist <- function(d, isi_type, ps=c("baseline", "fd", "recovery"), pl=c("in_phase", "out_of_phase", NA, "neither"), to_graph='interval_length') {
  # narrow down data set
  isi_data <- copy(d[type==isi_type])
  
  # Get list of labels
  labs <- levels(isi_data$interval_length_wake_label)
  dist_dt <- as.data.table(expand.grid(wake_label=labs, stringsAsFactors=FALSE))
  
  dist_dt[,c("mean", "sd") := computeIsiDist(isi_data, wake_label, to_graph),by='wake_label']
  
  histograms <<- list()
  
  
  
  dist_dt[,plotIsiDist(isi_data, wake_label, mean, sd, to_graph),by='wake_label']
  
  
  
  
  histograms #grid.arrange(grobs=ps, ncol=1)
}



plotIsiScatter <- function(d, isi_type, to_graph='interval_length') {
  # isi_type <- "REM"
  # d <- inter_state_intervals
  # to_graph <- 'interval_length'
  # 
  
  inter_state_intervals
  
  
  isi_data <- inter_state_intervals[type=="SWS"]
  
  
  plot <- ggplot(data=isi_data, mapping=aes(x=interval_length_without_wake, y=interval_length_wake))
  plot + geom_point(size=.3) + theme_tufte(base_size = 24, base_family = "helvetica") + scale_color_few() + scale_fill_few() +
    #scale_y_continuous(expand = c(0,0), trans = "log10p") +
    scale_x_continuous(expand = c(0,0), limits = c(0,200)) +
    labs(x="Interval Length - Length of Wake", y = "Length of Wake")
  
  labs <- levels(isi_data$interval_length_wake_label)
  dist_dt <- as.data.table(expand.grid(wake_label=labs, stringsAsFactors=FALSE))
  
  dist_dt[,c("mean", "sd") := computeIsiDist(isi_data, wake_label, to_graph),by='wake_label']
  
}

plotIsiTrend <- function(d, isi_type, to_graph='interval_length') {
  isi_data <- copy(d[type==isi_type])
  
  
  labs <- levels(isi_data$interval_length_wake_label)
  dist_dt1 <- as.data.table(expand.grid(wake_label=labs, stringsAsFactors=FALSE))
  dist_dt2 <- as.data.table(expand.grid(wake_label=labs, stringsAsFactors=FALSE))
  
  wake_stats <- isi_data[,list(n=.N,mean_wake=mean(interval_length_wake)),by='interval_length_wake_label']
  setnames(wake_stats, "interval_length_wake_label", "wake_label")
  
  dist_dt1[,c("mean", "sd") := computeIsiDist(isi_data, wake_label, "interval_length"),by='wake_label']
  dist_dt1[,to_graph:="Including Wake"]
  
  dist_dt2[,c("mean", "sd") := computeIsiDist(isi_data, wake_label, "interval_length_without_wake"),by='wake_label']
  dist_dt2[,to_graph:="Excluding Wake"]
  dist_dt <- rbind(dist_dt1, dist_dt2)

  dist_dt1 <- merge(dist_dt1, wake_stats, by=c('wake_label'))  
  dist_dt2 <- merge(dist_dt2, wake_stats, by=c('wake_label'))  
  dist_dt <- merge(dist_dt, wake_stats, by=c('wake_label'))  
  lin_fit1 <- lm(mean~mean_wake, dist_dt1)
  lin_fit2 <- lm(mean~mean_wake, dist_dt2)
  
  ggplot(data=dist_dt) + 
    geom_point(mapping=aes(x=mean_wake, y=mean, color=to_graph), size=3) + 
    geom_abline(intercept=lin_fit1$coefficients[[1]], slope=lin_fit1$coefficients[[2]], color="#E69F00", size=1.25) +
    geom_abline(intercept=lin_fit2$coefficients[[1]], slope=lin_fit2$coefficients[[2]], size=1.25) +
    theme_tufte(base_size=30, base_family = "helvetica") + theme(legend.title=element_blank()) + 
    scale_color_manual(values=c("#000000", "#E69F00")) +
    scale_x_continuous(expand = c(0,1.5), limits=c(0,150), breaks=circ_breaks()) + scale_y_continuous(expand = c(0,0.5), limits=c(0,150), breaks=circ_breaks())  +
    #annotate("text", label = paste("Intercept:", round(lin_fit$coefficients[[1]], digits=2), "Slope:", round(lin_fit$coefficients[[2]], digits=2)), x = 55, y = 30, size = 9, colour = "red") +
    labs(x="Average Wake in Inter-REM Intervals (minutes)", y="Distribution Mean (minutes)")    
#   
}
