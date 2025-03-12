# Author: Alvise Dabalà
# Date: 2024-09-04

pacman::p_load(tidyverse, wdpar, patchwork)

PAs_map_all <- readRDS("Figures/PAs_map_all.rds")
PAs_number_marine <- readRDS("Figures/PAs_number_marine.rds")
PAs_number_land <- readRDS("Figures/PAs_number_land.rds")

layout <- c(
  area(t = 1, l = 1, b = 5, r = 2),
  area(t = 6, l = 1, b = 7, r = 2),
  area(t = 8, l = 1, b = 9, r = 2)
)

plot_figure <- (plot_all + PAs_number_marine + PAs_number_land) + plot_layout(design = layout,
                                                                              guides = "collect", 
                                                                              axis_titles = "collect")  & theme(legend.position = 'bottom')

ggsave(plot =  plot_figure,
       filename = ("Figures/patchwork_01.png"),
       width = 10, height = 18, dpi = 300)
