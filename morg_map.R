# Mapping City of Morgantown Demographics
# Samuel Workman, Ph.D.
# April 8, 2024

# Load necessary libraries
library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(osmdata)
library(ggtext)


# Provide your Census API key here
census_api_key("0380ba2abdcb944a1024dd66d7e654fe85913889", install = TRUE, overwrite = TRUE)

morgantown_pop_bg <- get_acs(geography = "block group",
                             variables = "B01003_001",  # Total population variable
                             year = 2022,
                             state = "WV",
                             county = "Monongalia",
                             survey = "acs5",
                             geometry = TRUE)

morgantown_age_bg <- get_acs(geography = "block group",
                             variables = "B01002_001",
                             year = 2022,
                             state = "WV",
                             county = "Monongalia",
                             survey = "acs5",
                             geometry = TRUE)

morgantown_inc_bg <- get_acs(geography = "block group",
                             variables = "B19301_001",
                             year = 2022,
                             state = "WV",
                             county = "Monongalia",
                             survey = "acs5",
                             geometry = TRUE)


ggplot(data = morgantown_pop) +
  geom_sf(aes(fill = estimate)) +  # Removed the color = NULL argument
  scale_fill_viridis_c(name = "Population") +  # Use a color scale that's intuitive for population data
  labs(title = "2022 Population Estimates by Census Tract in Monongalia County, WV",
       subtitle = "Data from the American Community Survey 5-Year Estimates") +
  theme_minimal() +
  theme(legend.position = "bottom")  # Adjust legend position

ggplot(data = morgantown_pop_bg) +
  geom_sf(aes(fill = estimate), lwd = 0.1, color = "white") +  # Use a thin white line for borders
  scale_fill_viridis_c(name = "Population", option = "C") +  # Color scale for population
  labs(title = "2022 Population Estimates by Block Group in Monongalia County, WV",
       subtitle = "Data from the American Community Survey 5-Year Estimates") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot the block group data with population estimates, focused on Morgantown
ggplot(data = morgantown_pop_bg) +
  geom_sf(aes(fill = estimate), lwd = 0.1, color = "white") +  # Define aesthetics for the map
  scale_fill_viridis_c(name = "Population", option = "C") +  # Use a color scale for population
  labs(title = "2022 Population Estimates by Block Group in Morgantown, WV",
       subtitle = "Data from the American Community Survey 5-Year Estimates") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_sf(xlim = c(-80.00, -79.90), ylim = c(39.60, 39.70), expand = FALSE)  # Set longitude and latitude bounds

scale_fill_gradient(low = "lightblue", high = "darkblue")
scale_fill_gradientn(colors = c("lightblue", "green", "yellow", "red"))

ggplot(data = morgantown_pop_bg) +
  geom_sf(aes(fill = estimate), lwd = 0.1, color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Population") +
  labs(title = "2022 Population Estimates by Block Group in Morgantown, WV",
       subtitle = "Data from the American Community Survey 5-Year Estimates") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_sf(xlim = c(-80.00, -79.90), ylim = c(39.60, 39.70), expand = FALSE) 

# Define a bounding box around Morgantown (adjust these values as needed)
bbox <- c(-80.00, 39.60, -79.90, 39.70)  # min Longitude , min Latitude, max Longitude, max Latitude

# Query OSM for neighborhoods (or another place type) within the bounding box
morgantown_places <- opq(bbox = bbox) %>%
  add_osm_feature(key = 'place', value = 'neighbourhood') %>%  # You can change 'neighbourhood' to 'suburb', 'locality', etc.
  osmdata_sf()

# Extract place names and their coordinates
place_names <- morgantown_places$osm_points %>%
  st_as_sf() %>%
  select(name, geometry) %>%
  st_transform(crs = st_crs(morgantown_pop_bg))  # Ensure CRS match

# Get OSM data for streets (you can change the query to get rivers or other features)
morgantown_streets <- opq(bbox = bbox) %>%
  add_osm_feature(key = 'highway') %>%  # Use 'waterway' for rivers, for example
  osmdata_sf()

ggplot() +
  geom_sf(data = morgantown_pop_bg, aes(fill = estimate), lwd = 0.1, color = "white") +
  geom_sf(data = morgantown_streets$osm_lines, color = "white", size = 0.2) +  # Plot streets as grey lines
  #scale_fill_viridis_c(option = "C", direction = -1, name = "Population") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Population") +
  labs(title = "2022 Population Estimates by Block Group in Morgantown, WV",
       subtitle = "Data from the American Community Survey 5-Year Estimates") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_sf(xlim = c(-80.00, -79.90), ylim = c(39.60, 39.68), expand = FALSE)

ggplot() +
  geom_sf(data = morgantown_pop_bg, aes(fill = estimate), lwd = 0.1, color = "white") +
  geom_sf(data = morgantown_streets$osm_lines, color = "white", size = 0.2) +  # Plot streets as grey lines
  #scale_fill_viridis_c(option = "C", direction = -1, name = "Population") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Population") +
  geom_text(data = place_names, aes(label = name, x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2]), size = 3, check_overlap = TRUE, color = "blue") +
  scale_fill_viridis_c(option = "C", direction = -1, name = "Population") +
  labs(title = "2022 Population Estimates by Block Group in Morgantown, WV",
       subtitle = "Data from the American Community Survey 5-Year Estimates, with OSM Place Names") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_sf(xlim = c(-80.00, -79.90), ylim = c(39.60, 39.68), expand = FALSE)

# Final Population
pop <- ggplot() +
  geom_sf(data = morgantown_pop_bg, aes(fill = estimate), lwd = 0.1, color = "white") +
  geom_sf(data = morgantown_streets$osm_lines, color = "grey", size = 0.2) +
  geom_label(data = place_names, aes(label = name, x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2]),
             size = 3, fontface = "bold", color = "white",
             label.size = 0, label.padding = unit(0.15, "lines"),
             family = "sans", lineheight = 0.9,
             fill = NA, label.r = unit(0.15, "lines"),
             segment.color = NA,
             check_overlap = TRUE) +
  scale_fill_viridis_c(option = "C", direction = -1, name = "Population") +
  labs(title = "2022 Population Estimates by Block Group in Morgantown, WV",
       subtitle = "Data from the American Community Survey 5-Year Estimates, with OSM Place Names",
       y = "Latitude", x = "Longitude") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  coord_sf(xlim = c(-80.00, -79.90), ylim = c(39.60, 39.68), expand = FALSE)

# Final Median Age
age <- ggplot() +
  geom_sf(data = morgantown_age_bg, aes(fill = estimate), lwd = 0.1, color = "white") +
  geom_sf(data = morgantown_streets$osm_lines, color = "grey", size = 0.2) +
  geom_label(data = place_names, aes(label = name, x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2]),
             size = 3, fontface = "bold", color = "white",
             label.size = 0, label.padding = unit(0.15, "lines"),
             family = "sans", lineheight = 0.9,
             fill = NA, label.r = unit(0.15, "lines"),
             segment.color = NA,
             check_overlap = TRUE) +
  scale_fill_viridis_c(option = "C", direction = -1, name = "Median Age") +
  labs(title = "2022 Median Age Estimates by Block Group in Morgantown, WV",
       subtitle = "Data from the American Community Survey 5-Year Estimates, with OSM Place Names",
       y = "Latitude", x = "Longitude") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  coord_sf(xlim = c(-80.00, -79.90), ylim = c(39.60, 39.68), expand = FALSE)

# Final Per Capita Income
income <- ggplot() +
  geom_sf(data = morgantown_inc_bg, aes(fill = estimate/1000), lwd = 0.1, color = "white") +
  geom_sf(data = morgantown_streets$osm_lines, color = "grey", size = 0.2) +
  geom_label(data = place_names, aes(label = name, x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2]),
             size = 3, fontface = "bold", color = "white",
             label.size = 0, label.padding = unit(0.15, "lines"),
             family = "sans", lineheight = 0.9,
             fill = NA, label.r = unit(0.15, "lines"),
             segment.color = NA,
             check_overlap = TRUE) +
  scale_fill_viridis_c(option = "C", direction = -1, name = "Per Capita Income $1k") +
  labs(title = "2022 Per Capita Income Estimates by Block Group in Morgantown, WV",
       subtitle = "Data from the American Community Survey 5-Year Estimates, with OSM Place Names, Inflation Adjusted",
       y = "Latitude", x = "Longitude") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  coord_sf(xlim = c(-80.00, -79.90), ylim = c(39.60, 39.68), expand = FALSE)

ggsave("morgantown_pop.png", pop, width = 8, height = 11)
ggsave("morgantown_age.png", age, width = 8, height = 11)
ggsave("morgantown_inc.png", income, width = 8, height = 11)

ggsave("morgantown_pop.pdf", pop, width = 8, height = 11)
ggsave("morgantown_age.pdf", age, width = 8, height = 11)
ggsave("morgantown_inc.pdf", income, width = 8, height = 11)
