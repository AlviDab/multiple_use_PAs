# Author: Alvise Dabalà
# Date: 2025-03-12

pacman::p_load(tidyverse, wdpar, MoMAColors)

# install.packages("devtools")
# devtools::install_github("BlakeRMills/MoMAColors")

wdpa <- read.csv("data/WDPA_Sep2024_Public_csv/WDPA_Sep2024_Public_csv.csv") %>%
  as_tibble() %>%
  filter(STATUS %in% c("Designated", "Inscribed", "Established")) %>%
  filter(DESIG_ENG != "UNESCO-MAB Biosphere Reserve") %>%   
  mutate(IUCN_CAT = case_when(IUCN_CAT %in% c("V", "VI") ~ "Multiple Use",
                              .default = "Other"
  )) %>% 
  mutate(IUCN_CAT = factor(IUCN_CAT, levels = c("Multiple Use", "Other")))

no_year_info <- wdpa %>%
  filter(STATUS_YR == 0)

missing_years <- tibble(
  IUCN_CAT = "Other",
  # A random one, the others are filled by `complete` later
  STATUS_YR = 1883:2024,
  area = 0,
  n_PAs = 0
)

wdpa_marine_by_IUCN <- wdpa %>%
  filter(STATUS_YR != 0) %>%
  filter(MARINE != 0) %>%
  arrange(STATUS_YR) %>%
  dplyr::select(STATUS_YR, IUCN_CAT, REP_AREA) %>%  
  group_by(IUCN_CAT, STATUS_YR) %>%
  summarise(
    area = sum(REP_AREA), # Used REP_AREA because there were some NAs in GIS_AREA
    n_PAs = n()
  ) %>%
  ungroup() %>%
  add_row(missing_years) %>%
  tidyr::complete(IUCN_CAT, STATUS_YR, fill = list(area = 0, n_PAs = 0)) %>%
  mutate(decade = floor(STATUS_YR/10)*10) %>% 
  group_by(decade, IUCN_CAT) %>% 
  summarize(decade = first(decade),
            IUCN_CAT = first(IUCN_CAT),
            area = sum(area),
            n_PAs = sum(n_PAs)) %>%
  group_by(IUCN_CAT) %>%
  mutate(
    area = cumsum(area),
    n_PAs = cumsum(n_PAs)
  ) %>% 
  pivot_wider(names_from = IUCN_CAT, values_from = c(area, n_PAs)) %>% 
  mutate(ratio_area = `area_Multiple Use`/area_Other,
         ratio_n = `n_PAs_Multiple Use`/n_PAs_Other)

ggPA_marine <- ggplot(data = wdpa_marine_by_IUCN, aes(y = ratio_area, 
                                       x = decade, 
                                       colour = ratio_n)) + 
  geom_line(colour = "black", linewidth = 1) +
  geom_point(size = 4) + 
  theme_bw() +
  scale_colour_viridis_c(name = "Number of designated protected areas ratio") +
  theme(
    legend.background = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    text = ggplot2::element_text(size = 20, colour = "black"),
    axis.text = ggplot2::element_text(size = 20, colour = "black"),
    plot.title = ggplot2::element_text(size = 20, colour = "black"),
    legend.key.width = unit(3, "cm")
  ) +
  guides(col = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  scale_x_continuous(
    expand = c(0, 0),
    name = "Decade of designation",
    limits = c(1870, 2024)
  ) +
  scale_y_continuous(
    expand = c(0.1, 0.1),
    name = "Area protected areas ratio",
  )

colorblind.friendly.moma("OKeeffe")

############## terrestrial
wdpa_terrestrial_by_IUCN <- wdpa %>%
  filter(STATUS_YR != 0) %>%
  filter(MARINE == 0) %>%
  arrange(STATUS_YR) %>%
  dplyr::select(STATUS_YR, IUCN_CAT, REP_AREA) %>%  
  group_by(IUCN_CAT, STATUS_YR) %>%
  summarise(
    area = sum(REP_AREA), # Used REP_AREA because there were some NAs in GIS_AREA
    n_PAs = n()
  ) %>%
  ungroup() %>%
  add_row(missing_years) %>%
  tidyr::complete(IUCN_CAT, STATUS_YR, fill = list(area = 0, n_PAs = 0)) %>%
  mutate(decade = floor(STATUS_YR/10)*10) %>% 
  group_by(decade, IUCN_CAT) %>% 
  summarize(decade = first(decade),
            IUCN_CAT = first(IUCN_CAT),
            area = sum(area),
            n_PAs = sum(n_PAs)) %>%
  group_by(IUCN_CAT) %>%
  mutate(
    area = cumsum(area),
    n_PAs = cumsum(n_PAs)
  ) %>% 
  pivot_wider(names_from = IUCN_CAT, values_from = c(area, n_PAs)) %>% 
  mutate(ratio_area = `area_Multiple Use`/area_Other,
         ratio_n = `n_PAs_Multiple Use`/n_PAs_Other)

ggPA_terrestrial <- ggplot(data = wdpa_terrestrial_by_IUCN, aes(y = ratio_area, 
                                       x = decade, 
                                       colour = ratio_n)) + 
  geom_line(colour = "black", linewidth = 1) +
  geom_point(size = 4) + 
  theme_bw() +
  scale_colour_viridis_c(name = "Number of designated protected areas ratio") +
  theme(
    legend.background = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    text = ggplot2::element_text(size = 20, colour = "black"),
    axis.text = ggplot2::element_text(size = 20, colour = "black"),
    plot.title = ggplot2::element_text(size = 20, colour = "black"),
    legend.key.width = unit(3, "cm")
  ) +
  guides(col = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  scale_x_continuous(
    expand = c(0, 0),
    name = "Decade of designation",
    limits = c(1820, 2024)
  ) +
  scale_y_continuous(
    expand = c(0.1, 0.1),
    name = "Area protected areas ratio",
  )

colorblind.friendly.moma("OKeeffe")

######### combine

ggsave(plot =  ggPA_marine,
       filename = ("Figures/03_MPAs_ratio_marine.png"),
       width = 10, height = 7, dpi = 300)

ggsave(plot =  ggPA_terrestrial,
       filename = ("Figures/03_MPAs_ratio_terrestrial.png"),
       width = 10, height = 7, dpi = 300)
