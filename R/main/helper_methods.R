## USED IN CYCLES AND EPISODES
label_by_schedule <- function(start_labtime, end_labtime, start_analysis, end_analysis) {
  labels <- rep(NA, length(start_labtime))
  labels[which(start_labtime >= start_analysis & end_labtime <= end_analysis)] <- "fd"
  labels[which(start_labtime < start_analysis)] <- "baseline"
  labels[which(end_labtime > end_analysis)] <- "recovery"
  
  labels
}

## Every combo of subject_code and method needs to have: NREM, REM, WAKE, UNDEF
## USED IN PLAYGROUND
generate_missing_combinations <- function(bout_type, by) {
  if(TRUE %in% !(levels(bout_type) %in% unique(bout_type))) {
    # cat(sprintf("%s | %s\n", by[[1]], levels(bout_type)[!(levels(bout_type) %in% unique(bout_type))]))
    data.table(bout_type=levels(bout_type)[!(levels(bout_type) %in% unique(bout_type))],activity_or_bedrest_episode=c(0), start_labtime=c(-1), end_labtime=c(-1), length=c(-1))
    #     lapply(, function(level) {
    #       (bout_type=level, )
    #     })
  }
}

# get index (for main sleep data) of first instance of a given stage in a given list of stages
## USED IN EPISODES AND CYCLES
get_first_stage <- function(stages, start_position, target, not_found=NA) {
  # print(cat(stages))
  # print(target)
  first_occurance <- (which(stages == target)[1] - 1 + start_position)
  
  if(is.na(first_occurance)) {
    first_occurance <- not_found
  }
  
  #print(as.integer(first_occurance))
  #print("----------")
  as.integer(first_occurance)
  
}


