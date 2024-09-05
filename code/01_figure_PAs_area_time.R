#Author: Alvise Dabala'
#Date: 2024-09-04

pacman::p_load(tidyverse, wdpar, MoMAColors)

# install.packages("devtools")
devtools::install_github("BlakeRMills/MoMAColors")

wdpa <- read.csv("Data/WDPA_Sep2024_Public_csv/WDPA_Sep2024_Public_csv.csv") %>% 
  as_tibble()

no_year_info <- wdpa %>% 
  filter(STATUS %in% c("Designated", "Inscribed", "Established")) %>% 
  filter(STATUS_YR == 0) %>% 
  filter(MARINE != 0)

wdpa_marine_by_IUCN <- wdpa %>% 
  filter(STATUS %in% c("Designated", "Inscribed", "Established")) %>% 
  filter(STATUS_YR != 0) %>% 
  filter(MARINE != 0) %>% 
  arrange(STATUS_YR) %>% 
  mutate(IUCN_CAT = case_when(IUCN_CAT %in% c("Not Applicable", "Not Assigned", "Not Reported") ~ "Other",
                              .default = IUCN_CAT)) %>% 
  group_by(IUCN_CAT, STATUS_YR
           ) %>% 
  summarise(area = sum(REP_AREA), #Used REP_AREA because there were some NAs in GIS_AREA
            n_PAs = n()) %>% 
  ungroup() %>% 
  tidyr::complete(IUCN_CAT, STATUS_YR, fill = list(area = 0, n_PAs = 0)) %>% 
  group_by(IUCN_CAT) %>% 
  mutate(area = cumsum(area),
         n_PAs = cumsum(n_PAs)) %>% 
  mutate(IUCN_CAT = factor(IUCN_CAT, levels = c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Other")))

PAs_2024 <- wdpa_marine %>% 
  ungroup() %>% 
  filter(STATUS_YR == 2024) %>% 
  summarise(area = sum(area),
            n_PAs = sum(n_PAs))

ratio_nPAs_area <- PAs_2024$n_PAs/PAs_2024$area

wdpa_marine <- wdpa_marine_by_IUCN %>% 
  group_by(STATUS_YR) %>% 
  summarise(area = sum(area),
            n_PAs = sum(n_PAs))

colorblind.friendly.moma("OKeeffe")

ggplot() +
  geom_col(data = wdpa_marine_by_IUCN, aes(x = STATUS_YR, y = n_PAs, fill = IUCN_CAT)) +
  geom_line(data = wdpa_marine, aes(x = STATUS_YR, y = area*ratio_nPAs_area),
            linewidth = 1) +
  scale_fill_moma_d("OKeeffe", -1) +
  labs(fill = "IUCN category") +
  theme_bw() +
  theme(legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(expand = c(0, 0),
                     name = "Year of designation") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, PAs_2024$n_PAs*1.05),
                     name = "Total number of Protected Areas",
                     sec.axis = sec_axis(trans=~./ratio_nPAs_area, 
                                         name = expression(paste("Total area ", (m)^2))))

                                         