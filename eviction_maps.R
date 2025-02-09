# Creates two maps of eviction increases in 2024

# Set-up
library(tidyverse)
library(sf)

# Load original data
housing_df <- read_csv("data/nyc_evictions.csv",
                       col_types = cols(
                         `Executed Date` = col_date(format = "%m/%d/%Y")))

# Load NTA shapefile from NYC Open Data
nta_url <- "https://data.cityofnewyork.us/resource/9nt8-h7nd.geojson"
nyc_nta <- st_read(nta_url)

# Convert points dataframe to sf object
points_sf_2024 <- st_as_sf(housing_df %>% filter(`Executed Date` >= '2024-01-01' & `Executed Date` < '2025-01-01' , 
                                                 !is.na(Longitude) ), 
                           coords = c("Longitude", "Latitude"), 
                           crs = st_crs(nyc_nta))

points_sf_2023 <- st_as_sf(housing_df %>% filter(`Executed Date` >= '2023-01-01' & `Executed Date` < '2024-01-01' , 
                                                 !is.na(Longitude) ), 
                           coords = c("Longitude", "Latitude"), 
                           crs = st_crs(nyc_nta))

# Perform spatial join and count points within each polygon
eviction_count <- nyc_nta %>%
  mutate(points_count_2024 = lengths(st_intersects(., points_sf_2024)),
         points_count_2023 = lengths(st_intersects(., points_sf_2023))) %>% 
  mutate(diff = points_count_2024 - points_count_2023)


# ----
# generate heat map

eviction_count %>% 
  ggplot(aes(fill = diff))+ 
  geom_sf(color = "lightgrey", size = 0.1) +
  scale_fill_gradient2(name = "Increase in Evictions from 2023 to 2024") +
  theme_void() +  
  labs(
    title = "Queens and Upper Manhattan experienced the largest\neviction increases in 2024",
    subtitle = "Evictions across the city were up 12% since 2023",
    caption = "Source: NYC Open Data"
  ) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.title = element_text(vjust = .8, size = 12),
    title =element_text(size=14)
  ) + 
  annotate("text", x = -74.25, y = 40.82, label = "Jamaica, Queens saw a 169% increase\nin evictions, the largest in the city", 
           size = 4, color = "black",
           hjust = 0) +
  # Add arrow with a curve
  geom_curve(aes(x = -74.10, y = 40.80, xend = -73.82, yend = 40.70), 
             arrow = arrow(length = unit(0.1, "cm")), 
             curvature = 0.2, color = "black", linewidth = .5) 

ggsave("img/heat_map.png", height = 8, width = 8)

# ----
# generate election-style arrow map

eviction_count_with_centroids <- eviction_count %>%
  mutate(
    centroid = st_centroid(geometry),
    x = sf::st_coordinates(centroid)[,1],
    y = sf::st_coordinates(centroid)[,2],
    # Calculate arrow start/end points
    # Adjust the scaling factor (0.001) to make arrows longer/shorter
    arrow_length = abs(diff) * 0.0002,
    x_end = case_when(
      diff < 0 ~ x - arrow_length,
      diff > 0 ~ x + arrow_length,
      TRUE ~ x
    ),
    y_end = case_when(
      diff > 0 ~ y + (x_end - x) * tan(pi/6),
      diff < 0 ~ y + 2 *((x_end - x) * tan(pi/12)),
      TRUE ~ y
    ),
    show_arrow = diff != 0
  )

ggplot() +
  # Base map layer
  geom_sf(data = eviction_count_with_centroids, 
          fill = "white", 
          color = "gray80") +
  # Add arrows
  geom_segment(data = filter(eviction_count_with_centroids, show_arrow),
               aes(x = x, 
                   xend = x_end,
                   y = y, 
                   yend = y_end, 
                   color = ifelse(diff > 0, "Increase", "Decrease"), 
                   alpha = abs(diff) + .2),
               arrow = arrow(length = unit(0.05, "cm")),
               size = .5) +
  scale_color_manual(values = c("#009e73","#d55e00"), name = "Change in Evictions (2023 to 2024)") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),  
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    plot.caption = element_text(size = 8),
    plot.title = element_text(hjust = 0.5),  # Center the title
    plot.subtitle = element_text(hjust = 0.5)  # Center the subtitle
  ) +  
  scale_alpha(range = c(0.3, 1), guide = "none") +  # Opacity legend removed for simplicity
  labs(
    title = "Evictions Surge Across New York City,\nwith Queens Seeing the Sharpest Increase",
    subtitle = "Change in number of evictions by neighborhood tabulation area,\nwith longer/darker arrows representing more evictions",
    caption = "Source: NYC Open Data"
  ) +
  coord_sf() 


ggplot() +
  # base map layer
  geom_sf(data = eviction_count_with_centroids,  
          fill = "white", 
          color = "gray80") + 
  # add triangular peaks
  geom_polygon(data = 
                 do.call(rbind, lapply(1:nrow(filter(eviction_count_with_centroids, show_arrow)), function(i) {
    row <- filter(eviction_count_with_centroids, show_arrow)[i,]
    data.frame(
      x = c(row$x - 0.002, row$x, row$x + 0.002),
      y = c(row$y, row$y + (row$diff)/4000, row$y),
      id = i,
      diff = row$diff
    )
  })),
  aes(x = x, y = y, group = id, fill = ifelse(diff > 0, "Increase", "Decrease")),
  size = 0.5, 
  alpha = 1) +
  scale_fill_manual(values = c("#009e73", "#d55e00"), 
                    name = "Change in Evictions (2023 to 2024)",
                    ) +
  theme_void() + 
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    panel.grid = element_blank(), 
    legend.position = "bottom", 
    legend.key.width = unit(2, "cm"),  
    legend.title = element_text(size = 8), 
    legend.text = element_text(size = 8), 
    plot.caption = element_text(size = 8), 
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0)
  ) +  
  scale_alpha(range = c(0.3, 1), guide = "none") +
  labs(
    title = "Evictions Surged Across New York City in 2024,\nwith Queens and Upper Manhattan Seeing the Sharpest Increase", 
    subtitle = "Change in number of evictions by neighborhood tabulation area,\nwith longer/darker arrows representing more evictions", 
    caption = "Source: NYC Open Data"
  ) + 
  coord_sf() +
  annotate("text", x = -74.25, y = 40.82, label = "Jamaica, Queens saw the highest rise in evictions\nwith a 169% increase", 
           size = 4, color = "black",
           hjust = 0) +
  # Add arrow with a curve
  geom_curve(aes(x = -74.11, y = 40.80, xend = -73.80, yend = 40.71), 
             arrow = arrow(length = unit(0.1, "cm")), 
             curvature = 0.2, color = "black", linewidth = .2) 

ggsave("img/arrow_map.png", height = 7, width = 7)

# ----
# generate election-style peak map


ggplot() +
  # base map layer
  geom_sf(data = eviction_count_with_centroids,  
          fill = "white", 
          color = "gray80") + 
  # add triangular peaks
  geom_polygon(data = 
                 do.call(rbind, lapply(1:nrow(filter(eviction_count_with_centroids, show_arrow)), function(i) {
                   row <- filter(eviction_count_with_centroids, show_arrow)[i,]
                   data.frame(
                     x = c(row$x - 0.002, row$x, row$x + 0.002),
                     y = c(row$y, row$y + (row$diff)/4000, row$y),
                     id = i,
                     diff = row$diff
                   )
                 })),
               aes(x = x, y = y, group = id, fill = ifelse(diff > 0, "Increase", "Decrease"), color = ifelse(diff > 0, "Increase", "Decrease" )),
               linewidth = .2,
               alpha = .5,
               show.legend = FALSE) + # Hide the polygon legend
  # Add dummy point layer just for legend
  geom_point(data = data.frame(
    x = NA_real_,
    y = NA_real_,
    change = factor(c("Increase", "Decrease"))
  ),
  aes(x = x, y = y, fill = change, color = change, shape = change),
  size = 3,
  alpha = .5,
  show.legend = TRUE) +
  scale_shape_manual(values = c("Increase" = 24, "Decrease" = 25), 
                     name = "Change in Evictions\n(2023 to 2024)", 
                     guide = "legend") +
  scale_color_manual(values = c("#009e73", "#d55e00"), 
                    name = "Change in Evictions\n(2023 to 2024)") +
  scale_fill_manual(values = c("#009e73", "#d55e00"), 
                    name = "Change in Evictions\n(2023 to 2024)") +
  theme_void() + 
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    panel.grid = element_blank(), 
    legend.position = "bottom", 
    legend.key.width = unit(2, "cm"),  
    legend.title = element_text(size = 8), 
    legend.text = element_text(size = 8), 
    plot.caption = element_text(size = 8), 
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0)
  ) +  
  scale_alpha(range = c(0.3, 1), guide = "none") +
  labs(
    title = "Evictions Surged Across New York City in 2024,\nwith Queens and Upper Manhattan Seeing the Sharpest Increase", 
    subtitle = "Change in number of evictions by neighborhood tabulation area,\nwith longer peaks representing more evictions", 
    caption = "Source: NYC Open Data"
  ) + 
  coord_sf() +
  annotate("text", x = -74.3, y = 40.81, label =  "Jamaica, Queens saw 135 more evictions (a 169%\nincrease) compared to 2023, the highest citywide",
           size = 3, color = "black",
           hjust = 0) +
  # Add arrow with a curve
  geom_curve(aes(x = -74.11, y = 40.79, xend = -73.80, yend = 40.71), 
             arrow = arrow(length = unit(0.1, "cm")), 
             curvature = 0.2, color = "black", linewidth = .1) 



ggsave("img/peak_map.png", height = 7, width = 7)
