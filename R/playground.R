source('R/sleep.tools.R')


subjects <-read.xls("data/subject_list.xls")[,c(1,2)]
subject_periods <- calculate_periods_for_subjects(subjects)

# Get specific results for a given subject
plot.bouts(subject_periods[["3335GX"]]$subject_df, subject_periods[["3335GX"]]$periods$changepoint, subject_periods[["3335GX"]]$periods$classic)  
df <- subject_periods[["3335GX"]]$subject_df
changepoint_periods <-subject_periods[["3335GX"]]$periods$changepoint 
classic_periods <-subject_periods[["3335GX"]]$periods$classic


stats <- calculate_subject_statistics(subject_periods)




## Classic vs. Changepoint

### Stats per period:

# smallest - nrem, rem, wake, undef
# largest - nrem, rem, wake, undef

# % correct in total
#   nrem
#   rem

DT1a <- as.data.table(stats[["3335GX"]]$classic$tabulated_periods)
DT1a[,`:=`(subject_code="3335GX", method="classic")]
DT1b <- as.data.table(stats[["3335GX"]]$changepoint$tabulated_periods)
DT1b[,`:=`(subject_code="3335GX", method="changepoint")]
DT1c <- as.data.table(stats[["3335GX"]]$untransformed$tabulated_periods)
DT1c[,`:=`(subject_code="3335GX", method="untransformed")]


DT2a <- as.data.table(stats[["23D8HS"]]$classic$tabulated_periods)
DT2a[,`:=`(subject_code="23D8HS", method="classic")]
DT2b <- as.data.table(stats[["23D8HS"]]$changepoint$tabulated_periods)
DT2b[,`:=`(subject_code="23D8HS", method="changepoint")]
DT2c <- as.data.table(stats[["23D8HS"]]$untransformed$tabulated_periods)
DT2c[,`:=`(subject_code="23D8HS", method="untransformed")]


DT3a <- as.data.table(stats[["2632DX"]]$classic$tabulated_periods)
DT3a[,`:=`(subject_code="2632DX", method="classic")]
DT3b <- as.data.table(stats[["2632DX"]]$changepoint$tabulated_periods)
DT3b[,`:=`(subject_code="2632DX", method="changepoint")]
DT3c <- as.data.table(stats[["2632DX"]]$untransformed$tabulated_periods)
DT3c[,`:=`(subject_code="2632DX", method="untransformed")]

DT4a <- as.data.table(stats[["28J8X"]]$classic$tabulated_periods)
DT4a[,`:=`(subject_code="28J8X", method="classic")]
DT4b <- as.data.table(stats[["28J8X"]]$changepoint$tabulated_periods)
DT4b[,`:=`(subject_code="28J8X", method="changepoint")]
DT4c <- as.data.table(stats[["28J8X"]]$untransformed$tabulated_periods)
DT4c[,`:=`(subject_code="28J8X", method="untransformed")]

DT <- rbindlist(list(DT1a, DT1b, DT1c, DT2a, DT2b, DT2c, DT3a, DT3b, DT3c, DT4a, DT4b, DT4c))

plot <- ggplot(DT, aes(x=length, fill=bout_type)) + geom_histogram(binwidth=1)
plot <- plot + scale_y_continuous(limits=c(0, max(table(DT[c("classic", "changepoint"),length]))))
plot <- plot + facet_grid(method ~ subject_code, scales="free") 
plot
#

# In general, We can bring the comparisons together.

# Determining cycles:
#  