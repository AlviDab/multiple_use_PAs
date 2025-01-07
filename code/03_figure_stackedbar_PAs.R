# Author: Alvise Dabalà
# Date: 2024-01-07

wdpa_marine_by_IUCN

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
  mutate(IUCN_CAT = factor(IUCN_CAT, levels = c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Other"))) %>% 
  arrange(STATUS_YR)

plot_stacked_bar <- ggplot(wdpa_land_by_IUCN, aes(fill = IUCN_CAT, y = n_PAs, x = STATUS_YR)) + 
  geom_bar(position = "fill", stat = "identity") +
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
    expand = expansion(0),
    name = "Year of designation"
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    name = "Percentage of designated protected area",
    ) +
  ggtitle("B. Terrestrial")

ggsave(plot = plot_stacked_bar,
       filename = ("Figures/PAs_number_stacked_bar.png"),
       width = 12, height = 11, dpi = 300)
