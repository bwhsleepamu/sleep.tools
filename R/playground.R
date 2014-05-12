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
DT1b <- as.data.table(stats[["3335GX"]]$changepoint$tabulate_periods)
DT1b[,`:=`(subject_code="3335GX", method="changepoint")]

DT2a <- as.data.table(stats[["23D8HS"]]$classic$tabulated_periods)
DT2a[,`:=`(subject_code="23D8HS", method="classic")]
DT2b <- as.data.table(stats[["23D8HS"]]$changepoint$tabulate_periods)
DT2b[,`:=`(subject_code="23D8HS", method="changepoint")]

DT3a <- as.data.table(stats[["2632DX"]]$classic$tabulated_periods)
DT3a[,`:=`(subject_code="2632DX", method="classic")]
DT3b <- as.data.table(stats[["2632DX"]]$changepoint$tabulate_periods)
DT3b[,`:=`(subject_code="2632DX", method="changepoint")]

DT4a <- as.data.table(stats[["28J8X"]]$classic$tabulated_periods)
DT4a[,`:=`(subject_code="28J8X", method="classic")]
DT4b <- as.data.table(stats[["28J8X"]]$changepoint$tabulate_periods)
DT4b[,`:=`(subject_code="28J8X", method="changepoint")]

DT <- rbindlist(list(DT1a, DT1b, DT2a, DT2b, DT3a, DT3b, DT4a, DT4b))

plot <- ggplot(DT, aes(x=length)) + geom_histogram(binwidth=5)
plot <- plot + facet_grid(subject_code ~ method, scales="free") + scale_x_continuous(limits=c(0, 250))
plot
#

# In general, We can bring the comparisons together.

# Determining cycles:
#  