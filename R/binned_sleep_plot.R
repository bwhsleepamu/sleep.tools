
collapsed_sleep_episode <- merge(collapsed_sleep_episode, subjects, all.x=TRUE, all.y=FALSE)
collapsed_cycle <- merge(collapsed_cycle, subjects, all.x=TRUE, all.y=FALSE)
collapsed_bedrest_episode <- merge(collapsed_bedrest_episode, subjects, all.x=TRUE, all.y=FALSE)

function(plot_data, y_var="nrem_sleep", group_by="age_group", facet_by="t_cycle", bin_range=c(1,10), cycle_type="NREM", method="changepoint", cycle_range=c(1,5)) {
  trendline <- plot_data[,list(bin_average=mean(get(y_var))), by=cat("")]
}

## Create trend lines across subjects (might need to be done on a graph by graph basis, or in bulk)
agg_nc <- to_plot_nc[,list(slow_wave_sleep=mean(slow_wave_sleep)), by='method,cycle_number,t_cycle,block_number,age_group']
agg_se <- to_plot_se[,list(slow_wave_sleep=mean(slow_wave_sleep)), by='block_number,age_group,t_cycle']


####### Plotting the blocks
dt <- to_plot_c[include == TRUE & type == "NREM" & method == "changepoint" & cycle_number < 6 & block_number < 10]

plot <- ggplot(dt, aes(x=block_number, y=nrem_sleep))
plot <- plot + geom_point(aes(color=schedule_label))
plot <- plot + facet_grid(. ~ cycle_number, scales='free')
plot <- plot + geom_smooth(aes(group=schedule_label, color=schedule_label), method=loess)



### Here are the things we can cut by
## We can plot:
# nrem_sleep
# rem_sleep
# slow_wave_sleep
# wake
# total_sleep
# missing

## We can limit by
# include
# block_number range


## We can facet or group by:
# age_group
# habitual_csr
# t_cycle
# schedule_label
# se_label
# sex

## CYCLES
## We can limit by
# method
# type
# cycle_number range

## We can facet by
# cycle_number

