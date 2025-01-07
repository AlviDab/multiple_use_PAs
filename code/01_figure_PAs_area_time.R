# Author: Alvise Dabalà
# Date: 2024-09-04

pacman::p_load(tidyverse, wdpar, MoMAColors)

# install.packages("devtools")
# devtools::install_github("BlakeRMills/MoMAColors")

wdpa <- read.csv("data/WDPA_Sep2024_Public_csv/WDPA_Sep2024_Public_csv.csv") %>%
  as_tibble()

no_year_info <- wdpa %>%
  filter(STATUS %in% c("Designated", "Inscribed", "Established")) %>%
  filter(STATUS_YR == 0) %>%
  filter(MARINE != 0)

missing_years <- tibble(
  IUCN_CAT = "II",
  # A random one, the others are filled by `complete` later
  STATUS_YR = 1883:2024,
  area = 0,
  n_PAs = 0
)

wdpa_marine_by_IUCN <- wdpa %>%
  filter(STATUS %in% c("Designated", "Inscribed", "Established")) %>%
  filter(STATUS_YR != 0) %>%
  filter(MARINE != 0) %>%
  arrange(STATUS_YR) %>%
  mutate(IUCN_CAT = case_when(IUCN_CAT %in% c("Not Applicable", "Not Assigned", "Not Reported") ~ "Other",
    .default = IUCN_CAT
  )) %>%
  group_by(IUCN_CAT, STATUS_YR) %>%
  summarise(
    area = sum(REP_AREA), # Used REP_AREA because there were some NAs in GIS_AREA
    n_PAs = n()
  ) %>%
  ungroup() %>%
  add_row(missing_years) %>%
  tidyr::complete(IUCN_CAT, STATUS_YR, fill = list(area = 0, n_PAs = 0)) %>%
  group_by(IUCN_CAT) %>%
  mutate(
    area = cumsum(area),
    n_PAs = cumsum(n_PAs)
  ) %>%
  mutate(IUCN_CAT = factor(IUCN_CAT, levels = c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Other")))

wdpa_marine <- wdpa_marine_by_IUCN %>%
  group_by(STATUS_YR) %>%
  summarise(
    area = sum(area),
    n_PAs = sum(n_PAs)
  ) %>%
  dplyr::mutate(area = area / 1000000)

PAs_2024 <- wdpa_marine %>%
  ungroup() %>%
  filter(STATUS_YR == 2024) %>%
  summarise(
    area = sum(area),
    n_PAs = sum(n_PAs)
  )

ratio_nPAs_area <- PAs_2024$n_PAs / PAs_2024$area

colorblind.friendly.moma("OKeeffe")

############## terrestrial
wdpa_land_by_IUCN <- wdpa %>%
  filter(STATUS %in% c("Designated", "Inscribed", "Established")) %>%
  filter(STATUS_YR != 0) %>%
  filter(MARINE == 0) %>%
  arrange(STATUS_YR) %>%
  mutate(IUCN_CAT = case_when(IUCN_CAT %in% c("Not Applicable", "Not Assigned", "Not Reported") ~ "Other",
                              .default = IUCN_CAT
  )) %>%
  group_by(IUCN_CAT, STATUS_YR) %>%
  summarise(
    area = sum(REP_AREA), # Used REP_AREA because there were some NAs in GIS_AREA
    n_PAs = n()
  ) %>%
  ungroup() %>%
  add_row(missing_years) %>%
  tidyr::complete(IUCN_CAT, STATUS_YR, fill = list(area = 0, n_PAs = 0)) %>%
  group_by(IUCN_CAT) %>%
  mutate(
    area = cumsum(area),
    n_PAs = cumsum(n_PAs)
  ) %>%
  mutate(IUCN_CAT = factor(IUCN_CAT, levels = c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Other")))

wdpa_land <- wdpa_land_by_IUCN %>%
  group_by(STATUS_YR) %>%
  summarise(
    area = sum(area),
    n_PAs = sum(n_PAs)
  ) %>%
  dplyr::mutate(area = area / 1000000)

PAs_2024_land <- wdpa_land %>%
  ungroup() %>%
  filter(STATUS_YR == 2024) %>%
  summarise(
    area = sum(area),
    n_PAs = sum(n_PAs)
  )

ratio_nPAs_area_land <- PAs_2024_land$n_PAs / PAs_2024_land$area

colorblind.friendly.moma("OKeeffe")


############# plots
ggPA_land <- ggplot() +
  geom_col(data = wdpa_land_by_IUCN, aes(x = STATUS_YR, y = n_PAs, fill = IUCN_CAT)) +
  geom_line(
    data = wdpa_land, aes(x = STATUS_YR, y = area * ratio_nPAs_area_land),
    linewidth = 1
  )  +
  scale_fill_moma_d("OKeeffe", -1) +
  labs(fill = "IUCN category") +
  theme_bw() +
  theme(
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    text = ggplot2::element_text(size = 20, colour = "black"),
    axis.text = ggplot2::element_text(size = 20, colour = "black"),
    plot.title = ggplot2::element_text(size = 20, colour = "black")
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    name = "Year of designation",
    limits = c(1910, 2024)
  ) +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, PAs_2024_land$n_PAs * 1.05),
    name = "Total number of Protected Areas",
    sec.axis = sec_axis(
      trans = ~ . / ratio_nPAs_area_land,
      name = expression(paste("Total area (Million ", km^2, ")"))
    )
  ) +
  ggtitle("B. Terrestrial")


ggPA_marine <- ggplot() +
  geom_col(data = wdpa_marine_by_IUCN, aes(x = STATUS_YR, y = n_PAs, fill = IUCN_CAT)) +
  geom_line(
    data = wdpa_marine, aes(x = STATUS_YR, y = area * ratio_nPAs_area),
    linewidth = 1
  ) +
  scale_fill_moma_d("OKeeffe", -1) +
  labs(fill = "IUCN category") +
  theme_bw() +
  theme(
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    text = ggplot2::element_text(size = 20, colour = "black"),
    axis.text = ggplot2::element_text(size = 20, colour = "black"),
    plot.title = ggplot2::element_text(size = 20, colour = "black")
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    name = "Year of designation",
    limits = c(1910, 2024)
  ) +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, PAs_2024$n_PAs * 1.05),
    name = "Total number of Protected Areas",
    sec.axis = sec_axis(
      trans = ~ . / ratio_nPAs_area,
      name = expression(paste("Total area (Million ", km^2, ")"))
    )
  )# +
  #ggtitle("A. Marine")


######### combine
library(patchwork)
(gg_all <- ggPA_marine / ggPA_land + plot_layout(guides = "collect", 
                                                 axis_titles = "collect")  & theme(legend.position = 'bottom'))

ggsave(plot = gg_all,
       filename = ("Figures/PAs_number_area.png"),
       width = 12, height = 11, dpi = 300)


ggsave(plot =  ggPA_marine,
       filename = ("Figures/MPAs_number_area_light.png"),
       width = 10, height = 7, dpi = 300)


ggPA_marine <- ggPA_marine + ggdark::dark_theme_gray(base_size = 18)
