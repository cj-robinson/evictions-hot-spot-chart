library(tidyverse)

# read in data
housing_df <- read_csv("nyc_evictions.csv",
                       col_types = cols(
                         `Executed Date` = col_date(format = "%m/%d/%Y")))

clev_data <- housing_df %>% 
  mutate(year = lubridate::year(`Executed Date`)) %>%
  rename(neigh = NTA,
         boro = BOROUGH) %>% 
  filter(year %in% c(2023, 2024)) %>% 
  group_by(year, neigh, boro) %>% 
  summarize(count = n()) %>% 
  pivot_wider(names_from = "year", values_from = "count") %>% 
  mutate(diff = `2024`-`2023`) %>% 
  replace_na(list(neigh = "No neighborhood", `2023` =0, `2024`=0))

clev_data %>%
  ungroup() %>% 
  filter(neigh != "No neighborhood") %>% 
  arrange((`2023`)) %>%
  slice_max(`2023`, n = 25) %>% 
  group_by(ifelse(diff>0, "pos", "neg")) %>% tally()

clev_data %>%
  ungroup() %>% 
  filter(neigh != "No neighborhood") %>% 
  arrange(`2023`) %>%
  slice_max(`2023`, n = 25) %>% 
  mutate(
    neigh = factor(neigh, levels = rev(unique(.$neigh))),
    pos_change = ifelse(diff > 0, `2024`, NA_real_),
    neg_change = ifelse(diff < 0, `2024`, NA_real_)
  ) %>% 
  ggplot(aes(x = neigh)) + 
  geom_segment(
    aes(
      xend = neigh, 
      y = `2023`, 
      yend = neg_change, 
      color = "Decrease from 2023 to 2024"
    ), 
    arrow = arrow(length = unit(0.02, "npc"))
  ) +
  geom_segment(
    aes(
      xend = neigh, 
      y = `2023`, 
      yend = pos_change, 
      color = "Increase from 2023 to 2024"
    ), 
    arrow = arrow(length = unit(0.02, "npc"))
  ) +
  geom_point(
    aes(y = `2023`, color = "2023 Evictions"), 
    size = 1
  ) +
  labs(
    title = "2024 Evictions Rose in NYC Eviction Hotspots",
    subtitle = "In the 25 neighborhoods with the highest number of 2023 evictions,\n20 increased in 2024",
    color = "Legend"
  ) +
  coord_flip() +
  theme_minimal() +
  ylab("Number of Evictions") + 
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "right"
  ) +
  scale_color_manual(
    values = c(
      "2023 Evictions" = "black", 
      "Increase from 2023 to 2024" = "#4E94D2", 
      "Decrease from 2023 to 2024" = "#FF295D"
    )
  )

ggsave("evictions.png", height = 6, width = 8)
