## Fig 8 Helpers - REM dist

# Compute Distributions
computeIsiDist <- function(d, l, to_graph) {
  cat(l)
  
  s_d <- d[interval_length_wake_label == l]
  cat(paste(nrow(s_d),l,sep=" | "))
  min_bin <- min(s_d[[to_graph]], na.rm = TRUE)
  
  s_d <- s_d[get(to_graph) >= (15 + min_bin) & get(to_graph) <= (120 + min_bin)]
  r <- fitdist(s_d[[to_graph]], 'norm')
  
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
  
  plot <- ggplot(data=isi_data[interval_length_wake_label==wake_label], aes_string(to_graph)) + 
    geom_histogram(binwidth=bw, aes(y=..density..))  + 
    coord_cartesian(xlim=c(0,200)) + 
    ggtitle(paste(wake_label, "| ", " N:", nrow(isi_data[interval_length_wake_label==wake_label]), "| Mean:", round(ex), "| SD:", round(oh) )) + 
    stat_function(fun=dnorm, colour="red", args=list(mean=as.numeric(copy(ex)), sd=as.numeric(copy(oh)))) + 
    theme_Publication(base_size = 30, base_family="helvetica") + scale_colour_few() + 
    theme(axis.title=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), title=element_text(size=11, face='plain'), axis.text.x=element_text(size=14), axis.title.x=element_text(size=16)) + 
    #labs(x="Interval Length (minutes)") + 
    scale_x_continuous(expand = c(0,0))
  
  cat(paste(wake_label,ex,oh,sep=" "))
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
  isi_data <- inter_state_intervals[type=="REM"]
  
  
  labs <- levels(isi_data$interval_length_wake_label)
  dist_dt <- as.data.table(expand.grid(wake_label=labs, stringsAsFactors=FALSE))
  
  dist_dt[,c("mean", "sd") := computeIsiDist(isi_data, wake_label, to_graph),by='wake_label']
  
  
}
