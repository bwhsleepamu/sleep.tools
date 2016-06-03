library("ggthemes")

theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
  + theme(plot.title = element_text(face = "bold",
                                    size = rel(1.2), hjust = 0.5),
          text = element_text(),
          panel.background = element_rect(colour = NA),
          plot.background = element_rect(colour = NA),
          panel.border = element_rect(colour = NA),
          axis.title = element_text(face = "bold",size = rel(1)),
          axis.title.y = element_text(angle=90,vjust =2),
          axis.title.x = element_text(vjust = -0.2),
          axis.text = element_text(), 
          axis.line = element_line(colour="black"),
          axis.ticks = element_line(),
          panel.grid.major = element_line(colour="#f0f0f0"),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = NA),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.size= unit(0.2, "cm"),
          legend.margin = unit(0, "cm"),
          legend.title = element_text(face="italic"),
          plot.margin=unit(c(10,5,5,5),"mm"),
          strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
          strip.text = element_text(face="bold")
  ))
  
}


scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

# line	 all line elements (element_line)
# rect	 all rectangular elements (element_rect)
# text	 all text elements (element_text)
# title	 all title elements: plot, axes, legends (element_text; inherits from text)
# aspect.ratio	 aspect ratio of the panel
# axis.title	 label of axes (element_text; inherits from text)
# axis.title.x	 x axis label (element_text; inherits from axis.title)
# axis.title.y	 y axis label (element_text; inherits from axis.title)
# axis.text	 tick labels along axes (element_text; inherits from text)
# axis.text.x	 x axis tick labels (element_text; inherits from axis.text)
# axis.text.y	 y axis tick labels (element_text; inherits from axis.text)
# axis.ticks	 tick marks along axes (element_line; inherits from line)
# axis.ticks.x	 x axis tick marks (element_line; inherits from axis.ticks)
# axis.ticks.y	 y axis tick marks (element_line; inherits from axis.ticks)
# axis.ticks.length	 length of tick marks (unit)
# axis.line	 lines along axes (element_line; inherits from line)
# axis.line.x	 line along x axis (element_line; inherits from axis.line)
# axis.line.y	 line along y axis (element_line; inherits from axis.line)
# legend.background	 background of legend (element_rect; inherits from rect)
# legend.margin	 extra space added around legend (unit)
# legend.key	 background underneath legend keys (element_rect; inherits from rect)
# legend.key.size	 size of legend keys (unit; inherits from legend.key.size)
# legend.key.height	 key background height (unit; inherits from legend.key.size)
# legend.key.width	 key background width (unit; inherits from legend.key.size)
# legend.text	 legend item labels (element_text; inherits from text)
# legend.text.align	 alignment of legend labels (number from 0 (left) to 1 (right))
# legend.title	 title of legend (element_text; inherits from title)
# legend.title.align	 alignment of legend title (number from 0 (left) to 1 (right))
# legend.position	 the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
# legend.direction	 layout of items in legends ("horizontal" or "vertical")
# legend.justification	 anchor point for positioning legend inside plot ("center" or two-element numeric vector)
# legend.box	 arrangement of multiple legends ("horizontal" or "vertical")
# legend.box.just	 justification of each legend within the overall bounding box, when there are multiple legends ("top", "bottom", "left", or "right")
# panel.background	 background of plotting area, drawn underneath plot (element_rect; inherits from rect)
# panel.border	 border around plotting area, drawn on top of plot so that it covers tick marks and grid lines. This should be used with fill=NA (element_rect; inherits from rect)
# panel.margin	 margin around facet panels (unit)
# panel.margin.x	 horizontal margin around facet panels (unit; inherits from panel.margin)
# panel.margin.y	 vertical margin around facet panels (unit; inherits from panel.margin)
# panel.grid	 grid lines (element_line; inherits from line)
# panel.grid.major	 major grid lines (element_line; inherits from panel.grid)
# panel.grid.minor	 minor grid lines (element_line; inherits from panel.grid)
# panel.grid.major.x	 vertical major grid lines (element_line; inherits from panel.grid.major)
# panel.grid.major.y	 horizontal major grid lines (element_line; inherits from panel.grid.major)
# panel.grid.minor.x	 vertical minor grid lines (element_line; inherits from panel.grid.minor)
# panel.grid.minor.y	 horizontal minor grid lines (element_line; inherits from panel.grid.minor)
# panel.ontop	 option to place the panel (background, gridlines) over the data layers. Usually used with a transparent or blank panel.background. (logical)
# plot.background	 background of the entire plot (element_rect; inherits from rect)
# plot.title	 plot title (text appearance) (element_text; inherits from title)
# plot.margin	 margin around entire plot (unit with the sizes of the top, right, bottom, and left margins)
# strip.background	 background of facet labels (element_rect; inherits from rect)
# strip.text	 facet labels (element_text; inherits from text)
# strip.text.x	 facet labels along horizontal direction (element_text; inherits from strip.text)
# strip.text.y	 facet labels along vertical direction (element_text; inherits from strip.text)
# strip.switch.pad.grid	 space between strips and axes when strips are switched (unit)
# strip.switch.pad.wrap	 space between strips and axes when strips are switched (unit)
