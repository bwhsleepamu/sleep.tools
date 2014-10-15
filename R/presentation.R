# Possible groupings:

# se_label:  c("20%", "40%", "60%", "80%", "100%") - Sleep Efficiency Quintiles, labeled by upper limit
# sleep_wake_label: c("SLEEP", "WAKE") - Within a sleep episode, SLEEP includes episodes from sleep onset to wake.
# habitual_csr: c("habitual", "csr") - Habitual vs. Chronic Sleep Restriction
# age_group: c("O", "Y") - Older vs. Younger
# age
# schedule_label: c("baseline", "fd", "recovery") Baseline vs. FD vs. Recovery part of protocol
# sex: c("M", "F")
# t_cycle: c(28, 20, 42.85)

# methods: c("changepoint", "classic", "iterative")
# binned_sleep_types: c("nrem_sleep", "rem_sleep", "wake", "slow_wave_sleep", "total_sleep")
# cycle_types: c("NREM", "REM")

# Limit by: cycle_number, block_number

plot_agreement(episodes, facet_x_by="habitual_csr")
plot_agreement(episodes, facet_x_by="age_group")
plot_agreement(episodes, facet_x_by="schedule_label")


plot_agreement(episodes, facet_x_by="se_label", sleep_efficiency_labels=c("20%", "80%"), schedule_label="fd")


# Raster
setup_raster_data(sleep_data, episodes, cycles, bedrest_episodes)
plot_raster("1122X")
