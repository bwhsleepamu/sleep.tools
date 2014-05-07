source('R/sleep.tools.R')


subjects <-read.xls("data/subject_list.xls")[,c(1,2)]

subject_periods <- calculate_periods_for_subjects(subjects)


plot.bouts(subject_periods[["3335GX"]]$subject_df, subject_periods[["3335GX"]]$periods$changepoint, subject_periods[["3335GX"]]$periods$classic)  


stats <- llply(subject_periods, function(subject) {
  #df <- subject$subject_df
  #changepoint <- subject$periods$changepoint
  tab_classic <- tabulate_periods(subject$periods$classic, subject$subject_df)
  tab_changepoint <- tabulate_periods(subject$periods$changepoint, subject$subject_df)
  
  stats_classic <- calculate_agreement_stats(tab_classic)
  stats_changepoint <- calculate_agreement_stats(tab_changepoint)
  
  list(classic=list(tabulated_periods=tab_classic, agreement_stats=stats_classic), changepoint=list(tabulate_periods=tab_changepoint, agreement_stats=stats_changepoint))
})

stats.df <- data.frame(subject_code=names(stats), classic_nrem_count=)

df <- subject_periods[["3335GX"]]$subject_df
changepoint_periods <-subject_periods[["3335GX"]]$periods$changepoint 
classic_periods <-subject_periods[["3335GX"]]$periods$classic

## Classic vs. Changepoint

### Stats per period:

tab_classic <- tabulate_periods(classic_periods, df)
tab_changepoint <- tabulate_periods(changepoint_periods, df)

stats_classic <- calculate_agreement_stats(tab_classic)
stats_changepoint <- calculate_agreement_stats(tab_changepoint)


# smallest - nrem, rem, wake, undef
# largest - nrem, rem, wake, undef

# % correct in total
#   nrem
#   rem
stat_df <- ldply(stats, function(subject_stats) {
  data.frame(
    n_classic=subject_stats$classic$agreement_stats$all$n, 
    proportion_classic=subject_stats$classic$agreement_stats$all$proportion,
    n_changepoint=subject_stats$changepoint$agreement_stats$all$n, 
    proportion_changepoint=subject_stats$changepoint$agreement_stats$all$proportion,
    
    n_classic_rem_nrem=subject_stats$classic$agreement_stats$REM_NREM$n, 
    proportion_classic_rem_nrem=subject_stats$classic$agreement_stats$REM_NREM$proportion,
    n_changepoint_rem_nrem=subject_stats$changepoint$agreement_stats$REM_NREM$n, 
    proportion_changepoint_rem_nrem=subject_stats$changepoint$agreement_stats$REM_NREM$proportion,
    
    n_classic_rem=subject_stats$classic$agreement_stats$REM$n, 
    proportion_classic_rem=subject_stats$classic$agreement_stats$REM$proportion,
    n_changepoint_rem=subject_stats$changepoint$agreement_stats$REM$n, 
    proportion_changepoint_rem=subject_stats$changepoint$agreement_stats$REM$proportion,
    
    n_classic_nrem=subject_stats$classic$agreement_stats$NREM$n, 
    proportion_classic_nrem=subject_stats$classic$agreement_stats$NREM$proportion,
    n_changepoint_nrem=subject_stats$changepoint$agreement_stats$NREM$n, 
    proportion_changepoint_nrem=subject_stats$changepoint$agreement_stats$NREM$proportion
  )
})



# 