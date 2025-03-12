# Author: Alvise Dabalà
# Date: 2024-01-07

pacman::p_load(tidyverse, wdpar, MoMAColors)

source("code/functions/f_create_worldmap.r")

#Not including points geometries
PAs_shp <- st_read("data/WDPA_Jan2025_Public_shp/WDPA_Jan2025_Public_shp_0/WDPA_Jan2025_Public_shp-polygons.shp") %>% 
  rbind(st_read("data/WDPA_Jan2025_Public_shp/WDPA_Jan2025_Public_shp_1/WDPA_Jan2025_Public_shp-polygons.shp")) %>% 
  rbind(st_read("data/WDPA_Jan2025_Public_shp/WDPA_Jan2025_Public_shp_2/WDPA_Jan2025_Public_shp-polygons.shp")) 

PAs_point <- st_read("data/WDPA_Jan2025_Public_shp/WDPA_Jan2025_Public_shp_0/WDPA_Jan2025_Public_shp-points.shp") %>% 
  rbind(st_read("data/WDPA_Jan2025_Public_shp/WDPA_Jan2025_Public_shp_1/WDPA_Jan2025_Public_shp-points.shp")) %>% 
  rbind(st_read("data/WDPA_Jan2025_Public_shp/WDPA_Jan2025_Public_shp_2/WDPA_Jan2025_Public_shp-points.shp")) %>% 
  wdpar::wdpa_clean(crs = 4326) %>% 
  st_wrap_dateline()

dir.create("results")
saveRDS(PAs_point, "results/PAs_point_clean.rds")
  
PAs <- PAs_shp %>% 
  bind_rows(PAs_point) %>% #dropping missing rows from PAs_point 
  filter(STATUS %in% c("Designated", "Inscribed", "Established")) %>%
  filter(DESIG_ENG != "UNESCO-MAB Biosphere Reserve") %>%
  arrange(STATUS_YR) %>%
  mutate(IUCN_CAT = case_when(IUCN_CAT %in% c("Not Applicable", "Not Assigned", "Not Reported") ~ "Other",
                              .default = IUCN_CAT
  ))%>%
  mutate(IUCN_CAT = factor(IUCN_CAT, levels = c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Other"))) 

PAs_terrestrial <- PAs %>% 
  filter(MARINE == 0) 

PAs_marine <- PAs %>% 
  filter(MARINE != 0)
  
world_map <- f_worldmap()
dat <- spatialplanr::splnr_get_boundary(Limits = "Global")

plot_terrestrial <- ggplot() +
  geom_sf(data = world_map, fill = "grey50") +
  geom_sf(data = PAs_terrestrial, aes(fill = IUCN_CAT), colour = NA) + 
  scale_fill_moma_d("OKeeffe", -1) +
  labs(fill = "IUCN category") +
  geom_sf(data = dat, fill = NA) +
  theme_minimal() +
  theme(
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    text = ggplot2::element_text(size = 20, colour = "black"),
    plot.title = ggplot2::element_text(size = 20, colour = "black")
  ) +
  ggtitle("Terrestrial") +
  coord_sf(datum = NA)

ggsave(plot = plot_terrestrial,
       filename = ("Figures/PAs_map_terrestrial.png"),
       width = 18, height = 12, dpi = 300)  

plot_marine <- ggplot() +
  geom_sf(data = world_map, fill = "grey50") +
  geom_sf(data = PAs_marine, aes(fill = IUCN_CAT), colour = NA) + 
  scale_fill_moma_d("OKeeffe", -1) +
  labs(fill = "IUCN category") +
  geom_sf(data = dat, fill = NA) +
  theme_minimal() +
  theme(
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    text = ggplot2::element_text(size = 20, colour = "black"),
    plot.title = ggplot2::element_text(size = 20, colour = "black")
  ) +
  ggtitle("Marine") +
  coord_sf(datum = NA)

ggsave(plot = plot_marine,
       filename = ("Figures/PAs_map_marine.png"),
       width = 18, height = 12, dpi = 300) 

plot_all <- ggplot() +
  geom_sf(data = world_map, fill = "grey50") +
  # geom_sf(data = PAs, aes(fill = IUCN_CAT), colour = NA) + 
  scale_fill_moma_d("OKeeffe", -1) +
  labs(fill = "IUCN category") +
  geom_sf(data = dat, fill = NA) +
  theme_minimal() +
  theme(
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    text = ggplot2::element_text(size = 20, colour = "black"),
    plot.title = ggplot2::element_text(size = 20, colour = "black")
  ) +
  coord_sf(datum = NA)

saveRDS(plot_all, "Figures/PAs_map_all.rds")

ggsave(plot = plot_all,
       filename = ("Figures/PAs_map_all.png"),
       width = 18, height = 12, dpi = 300) 
