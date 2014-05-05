source('R/sleep.tools.R')



res <- load_sleep_file("data/AMU/28J8XSlp.01.csv")
df <- res$df
min_day_number <- res$min_day_number

changepoint_bouts <- bouts.changepoint(df)
classic_bouts <- bouts.classic(df)

bouts <- classic_bouts

# set day numbers + labtimes for bouts
bouts$start_day_number <- floor(bouts$start_labtime / 24)
bouts$start_day_labtime <- (bouts$start_labtime - (bouts$start_day_number * 24))
bouts$start_day_number <- bouts$start_day_number - (min_day_number - 1)
bouts$end_day_number <- floor(bouts$end_labtime / 24)
bouts$end_day_labtime <- (bouts$end_labtime - (bouts$end_day_number * 24))
bouts$end_day_number <- bouts$end_day_number - (min_day_number - 1)

# Clean up bouts that span days

# Nothing needs to be done to these:
clean_bouts <- bouts[bouts$start_day_number == bouts$end_day_number,]

# These bouts span days
to_be_cleaned <- bouts[bouts$start_day_number != bouts$end_day_number,]

first_cleaned <- ddply(to_be_cleaned, .(start_day_number), first_div)
second_cleaned <- ddply(to_be_cleaned, .(start_day_number), second_div)
bouts <- rbind(first_cleaned, second_cleaned, clean_bouts)

# Draw
bouts$day_number <- bouts$start_day_number
.e <- environment()
# Main Plot
plot <- ggplot(df, aes(day_labtime, stage, group=day_number), environment = .e)
# Faceting
plot <- plot + facet_grid(day_number ~ .)
# Scaling and Margins
#plot <- plot + theme(panel.margin = unit(0, "npc"))
plot <- plot + scale_x_continuous(limits=c(0 - EPOCH_LENGTH, 24 + EPOCH_LENGTH), expand=c(0,0)) 
plot <- plot + scale_y_continuous(limits=c(-2, 10))



plot <- plot + geom_rect(aes(NULL, NULL, xmin = start_day_labtime, xmax = end_day_labtime + EPOCH_LENGTH, fill = bout_type), ymin = 0, ymax = 10, data = bouts)

plot <- plot + geom_point(aes(group=day_number), shape='.')

plot
