T_CYCLE = 24.0
EPOCH_SECONDS <- 30
EPOCH_LENGTH <- (EPOCH_SECONDS / 3600)
REM_MIN_PERIOD_LENGTH = 10
NREM_MIN_PERIOD_LENGTH = 30

# Changepoint
CP_DISTANCES=list(wake=-0.9, rem=-1.3, stage1=-0.5, stage2=0, stage3=1) #-0.7	-1.1	-0.8 # -0.9	-1.1	-0.5


CP_STAGE1=TRUE
CP_CLEAN=TRUE
CP_IC="SIC"

# Classic
CLASSIC_MIN_NREM=30
CLASSIC_MIN_REM=10
CLASSIC_COMPLETION_CUTOFF=10

# Subject Data Paths
subject_fp.local <- "data/local_subject_list.csv"
subject_fp.all <- "data/full_subject_list.csv"
sleep_stats_fp <- "data/sleep_stats.csv"

# Color Palettes
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#F0E442", "#888888", "#000000", "#D55E00",  "#CC79A7", "#555555")

cbbbPalette <- c("#D55E00", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#888888", "#000000", "#F0E442",  "#CC79A7", "#555555")

# Custom Theme
theme_mine <- function (base_size = 12, base_family = "") 
{
  theme(line = element_line(colour = "black", size = 0.5, linetype = 1, 
                            lineend = "butt"), rect = element_rect(fill = "white", 
                                                                   colour = "black", size = 0.5, linetype = 1), text = element_text(family = base_family, 
                                                                                                                                    face = "plain", colour = "black", size = base_size, hjust = 0.5, 
                                                                                                                                    vjust = 0.5, angle = 0, lineheight = 0.9), axis.text = element_text(size = rel(0.8), 
                                                                                                                                                                                                        colour = "grey50"), strip.text = element_text(size = rel(0.8)), 
        axis.line = element_blank(), axis.text.x = element_text(vjust = 1), 
        axis.text.y = element_text(hjust = 1), axis.ticks = element_line(colour = "grey50"), 
        axis.title.x = element_text(), axis.title.y = element_text(angle = 90), 
        axis.ticks.length = unit(0.15, "cm"), axis.ticks.margin = unit(0.1, 
                                                                       "cm"), legend.background = element_rect(colour = NA), 
        legend.margin = unit(0.2, "cm"), legend.key = element_rect(fill = "white", 
                                                                   colour = "white"), legend.key.size = unit(1.2, "lines"), 
        legend.key.height = NULL, legend.key.width = NULL, legend.text = element_text(size = rel(0.8)), 
        legend.text.align = NULL, legend.title = element_text(size = rel(0.8), 
                                                              face = "bold", hjust = 0), legend.title.align = NULL, 
        legend.position = "right", legend.direction = NULL, legend.justification = "center", 
        legend.box = NULL, panel.background = element_blank(), panel.border = element_blank(), panel.grid.major = element_line(colour = "grey50", size=0.25), 
        panel.grid.minor = element_line(colour = "white", size = 0.25), 
        panel.margin = unit(0.25, "lines"), strip.background = element_rect(fill = "white", 
                                                                            colour = NA), strip.text.x = element_text(), strip.text.y = element_text(angle = -90), 
        plot.background = element_rect(colour = "white"), plot.title = element_text(size = rel(1.2)), 
        plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"), complete = TRUE)
}