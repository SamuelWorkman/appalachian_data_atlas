library(sf)
library(tidyverse)
library(elevatr)
library(rayshader)
library(glue)
library(colorspace)
library(tigris)
library(stars)
library(NatParksPalettes)

# Set map name that will be used in file names, and 
# to get get boundaries from master NPS list

map <- "west_virginia"

# Kontur data source: https://data.humdata.org/organization/kontur
d_layers <- st_layers("Data/kontur_population_US_20220630.gpkg")
d_crs <- d_layers[["crs"]][[1]][[2]]

s <- states() |> 
  st_transform(crs = d_crs)

st <- s |> 
  filter(NAME == str_to_title(str_replace_all("west_virginia", "_", " ")))

wkt_st <- st_as_text(st[[1,"geometry"]])

data <- st_read("Data/kontur_population_US_20220630.gpkg",
                wkt_filter = wkt_st)

# data |> 
#   ggplot() +
#   geom_sf()

st_d <- st_join(data, st, left = FALSE)

#  st_d |> 
#   ggplot() +
#   geom_sf()

st_d <- st_intersection(data, st)

bb <- st_bbox(st_d)
yind <- st_distance(st_point(c(bb[["xmin"]], bb[["ymin"]])), 
                    st_point(c(bb[["xmin"]], bb[["ymax"]])))
xind <- st_distance(st_point(c(bb[["xmin"]], bb[["ymin"]])), 
                    st_point(c(bb[["xmax"]], bb[["ymin"]])))

if (yind > xind) {
  y_rat <- 1
  x_rat <- xind / yind
} else {
  x_rat <- 1
  y_rat <- yind / xind
}

size <- 8000
rast <- st_rasterize(st_d |> 
                       select(population, geom),
                     nx = floor(size * x_rat), ny = floor(size * y_rat))
rast_df <- data.frame(rast)


mat <- matrix(rast$population, nrow = floor(size * x_rat), ncol = floor(size * y_rat))

mat_d <- data.frame(mat)

pp <- ggplot(rast_df) +
  geom_raster(data=rast_df,aes(x,y, fill = population))
pp +
  scale_fill_viridis_c(option = "C")

plot_gg(pp, width = 4, height = 4, scale = 300, multicore = TRUE)




pp = ggplot(mat_d, aes(x=x, y=y)) +
  geom_hex(bins = 20, size = 0.5, color = "black") +
  scale_fill_viridis_c(option = "C")
plot_gg(pp, width = 4, height = 4, scale = 300, multicore = TRUE)
