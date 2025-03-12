# Author: Alvise Dabalà
# Date: 2025-01-31

pacman::p_load(tidyverse, wdpar, MoMAColors, countrycode)

# install.packages("devtools")
# devtools::install_github("BlakeRMills/MoMAColors")

wdpa <- read.csv("data/WDPA_Sep2024_Public_csv/WDPA_Sep2024_Public_csv.csv") %>%
  as_tibble() %>%
  filter(STATUS %in% c("Designated", "Inscribed", "Established")) %>%
  filter(DESIG_ENG != "UNESCO-MAB Biosphere Reserve")

continent <- countrycode::countrycode(wdpa$ISO3 %>% 
                                        gsub("\\;.*","", .), origin = 'iso3c', destination = 'continent') 

wdpa <- wdpa %>% 
  #cbind(continent) %>% 
  mutate(continent = case_when(ISO3 %in% c("BVT", "HMD", "SGS") ~ "Antarctica",
                               ISO3 == "CCK" ~ "Asia",
                               ISO3 == "ABNJ" ~ "ABNJ",
                               ISO3 == "UMI" ~ "Oceania",
                               ORIG_NAME %in% c("Ile d'Europa",
                                               "Archipel des Glorieuses") ~ "Africa",
                               ORIG_NAME %in% c("Terres Australes Françaises", 	
                                               "Terres et mers australes françaises",
                                               "Réserve Naturelle Nationale des Terres Australes Française") ~ "Antarctica",
                               .default = continent))

no_year_info <- wdpa %>%
  filter(STATUS_YR == 0) 

missing_years <- tibble(
  IUCN_CAT = "II",
  continent = "ABNJ",
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
  group_by(IUCN_CAT, STATUS_YR, continent) %>%
  summarise(
    area = sum(REP_AREA), # Used REP_AREA because there were some NAs in GIS_AREA
    n_PAs = n()
  ) %>%
  ungroup() %>%
  add_row(missing_years) %>%
  tidyr::complete(IUCN_CAT, STATUS_YR, continent, fill = list(area = 0, n_PAs = 0)) %>%
  group_by(IUCN_CAT, continent) %>%
  mutate(
    area = cumsum(area),
    n_PAs = cumsum(n_PAs)
  ) %>%
  mutate(IUCN_CAT = factor(IUCN_CAT, levels = c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Other")))

wdpa_marine <- wdpa_marine_by_IUCN %>%
  group_by(STATUS_YR, continent) %>%
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

ggPA_marine <- ggplot() +
  geom_col(data = wdpa_marine_by_IUCN, aes(x = STATUS_YR, y = n_PAs, fill = IUCN_CAT)) +
  geom_line(
    data = wdpa_marine, aes(x = STATUS_YR, y = area * ratio_nPAs_area),
    linewidth = 1, colour = "black"
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
    expand = c(0, 0), limits = c(0, 10000),
    name = "Total number of Protected Areas",
    sec.axis = sec_axis(
      trans = ~ . / ratio_nPAs_area,
      name = expression(paste("Total area (Million ", km^2, ")"))
    )
  ) +
  ggtitle("Marine Protected Areas") +
  facet_wrap(vars(continent))

ggsave("Figures/PAs_number_marine_by_continent.png",
       width = 12, height = 18, dpi = 300)

wdpa_terrestrial_by_IUCN <- wdpa %>%
  filter(STATUS %in% c("Designated", "Inscribed", "Established")) %>%
  filter(STATUS_YR != 0) %>%
  filter(MARINE == 0) %>%
  arrange(STATUS_YR) %>%
  mutate(IUCN_CAT = case_when(IUCN_CAT %in% c("Not Applicable", "Not Assigned", "Not Reported") ~ "Other",
                              .default = IUCN_CAT
  )) %>%
  group_by(IUCN_CAT, STATUS_YR, continent) %>%
  summarise(
    area = sum(REP_AREA), # Used REP_AREA because there were some NAs in GIS_AREA
    n_PAs = n()
  ) %>%
  ungroup() %>%
  add_row(missing_years) %>%
  tidyr::complete(IUCN_CAT, STATUS_YR, continent, fill = list(area = 0, n_PAs = 0)) %>%
  group_by(IUCN_CAT, continent) %>%
  mutate(
    area = cumsum(area),
    n_PAs = cumsum(n_PAs)
  ) %>%
  mutate(IUCN_CAT = factor(IUCN_CAT, levels = c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Other"))) %>% 
  filter(continent != "ABNJ")

wdpa_terrestrial <- wdpa_terrestrial_by_IUCN %>%
  group_by(STATUS_YR, continent) %>%
  summarise(
    area = sum(area),
    n_PAs = sum(n_PAs)
  ) %>%
  dplyr::mutate(area = area / 1000000)

PAs_2024 <- wdpa_terrestrial %>%
  ungroup() %>%
  filter(STATUS_YR == 2024) %>%
  summarise(
    area = sum(area),
    n_PAs = sum(n_PAs)
  )

ratio_nPAs_area <- PAs_2024$n_PAs / PAs_2024$area

ggPA_terrestrial <- ggplot() +
  geom_col(data = wdpa_terrestrial_by_IUCN, aes(x = STATUS_YR, y = n_PAs, fill = IUCN_CAT)) +
  geom_line(
    data = wdpa_terrestrial, aes(x = STATUS_YR, y = area * ratio_nPAs_area),
    linewidth = 1, colour = "black"
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
    expand = c(0, 0),
    name = "Total number of Protected Areas",
    sec.axis = sec_axis(
      trans = ~ . / ratio_nPAs_area,
      name = expression(paste("Total area (Million ", km^2, ")"))
    )
  ) +
  ggtitle("Terrestrial Protected Areas") +
  facet_wrap(vars(continent))

ggsave("Figures/PAs_number_terrestrial_by_continent.png",
       width = 12, height = 18, dpi = 300)

