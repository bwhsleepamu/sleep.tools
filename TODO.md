# Sleep Cycles Project
[www.github.com/pmanko/sleep.tools](www.github.com/pmanko/sleep.tools)

## Introduction 
Overview of project with example raster plot
  
### Goals
- Implement updated methods for generating ultradian cycles based on scored EEG data.
- Create tools for comparing effectiveness of different methods
- Create tools for visualizing and analyzing bedrests based on ultradian cycles
- Compare ultradian-based analysis to other reference timepoints
- Distribute R package containing implemented tools.
  
  
### Data we're working with 
- 128 Forced Desynchrony subjects (see subject table)
- Scored epochs - Rechtschaffen and Kales(see sleep data table)
  

## Definitions 
*Based on Glossary by Czeisler et al. published in Sleep in 1980*

### General Terms
- epoch  

- sequence  

- episode  

- cycle  


### Labels
- bedrest  

- activity  

- sleep  

- NREM sleep  

- REM sleep  

- wake  

- slow wave sleep  


### Other Terms
- REM-NREM (REM) cycle vs. NREM-REM (NREM) cycle  

- cycle length
  
## Objectives of this meeting:
1. Demo current state of ultradian cycle software  

2. Show large range of different visualizations that can be generated  

3. **Get any feedback, criticism, or novel insight on these visualizations** - I've been struggling with deciding what the most effective graphs for supporting our goals are, especially with such a large variety of possibilities.  

4. For the most effective comparisons, how can we make a statistically valid case that:
  a. One method is better than another
  b. Analysis by ultradian timpoints is better than analysis by other references  

5. Are there methods based in information theory that could provide a metric for performance? (for example, similar to Akaike information criterion)  

6. If a suitable metric for performance can be found, can we use it to maximize the performance of each method by finding best fits for the different parameters?  

7. Currently we're showcasing ultradian analysis with binned sleep plots to highlight patterns that cannot be easily discerned using sleep onset and bedrest onset timepoints. Are these two comparison timepoints enough? Is this demo sufficient?  

## Other Thoughts
- Does the specific method for finding cycles really matter? Small differences in start/end times or exact number of cycles might not have a huge effect on the results of a ultradian-based analysis. How would be test this?  

- Do differences in performance depend on the quality of sleep? In other words, does performance diverge when, for example, sleep efficiency is lower, or sleep is more fragmented...when clear cycles are more difficult to detect, do some methods perform better than others?  

## Types of Visualizations
1. Raster plots

2. Within-episode agreement distributions

3. Episode length distribution

4. Number of episodes per bedrest

5. Cycle length distribution

** Comparison with bedrest and sleep onset**
6. Binned sleep (survival?) graphs
 
### Variables by which data can be constrained or compared

- `se_label:  c("20%", "40%", "60%", "80%", "100%")` - Sleep Efficiency Quintiles, labeled by upper limit
- `sleep_wake_label: c("SLEEP", "WAKE")` - Within a sleep episode, SLEEP includes episodes from sleep onset to wake.
- `habitual_csr: c("habitual", "csr")` - Habitual vs. Chronic Sleep Restriction
- `age_group: c("O", "Y")` - Older vs. Younger
- `age`
- `schedule_label: c("baseline", "fd", "recovery")` Baseline vs. FD vs. Recovery part of protocol
- `sex: c("M", "F")`
- `t_cycle: c(28, 20, 42.85)`

*Binned sleep graphs and episode graphs:*
- `methods: c("changepoint", "classic", "iterative")`
*Binned sleep graphs and cycle graphs:*
- `cycle_types: c("NREM", "REM")`
*Binned sleep graphs:*
- `binned_sleep_types: c("nrem_sleep", "rem_sleep", "wake", "slow_wave_sleep", "total_sleep")`
- `max_cycle_number`
- `max_block_number`

### Example Visualizations
- young vs. old
- csr vs. habitual
- high efficiency vs. low efficiency
- **suggestions?**

