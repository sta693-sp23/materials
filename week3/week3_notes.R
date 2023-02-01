library(tidyverse)
library(sf)
library(stars)

(elev = read_stars("data/easter_island/ei_elev.tif"))
(border = read_sf("data/easter_island/ei_border.gpkg"))
(roads = read_sf("data/easter_island/ei_roads.gpkg"))
(points = read_sf("data/easter_island/ei_points.gpkg"))

## ggplot

ggplot() +
  geom_sf(data=border) +
  scale_fill_distiller(palette="RdYlGn", na.value = "transparent") +
  geom_stars(data=elev) +
  geom_sf(data=roads, aes(linewidth=strokelwd)) +
  scale_linewidth_continuous(range = c(0.1,1)) +
  geom_sf(data=points, aes(color=type, size=elevation), shape=17) +
  scale_size_continuous(range=c(1,4)) +
  theme_minimal() +
  guides(linewidth="none")


## tmap

tm_graticules(alpha=0.25) +
tm_shape(elev) +
  tm_raster(
    style = "cont",
    title = "Elevation (m)",
    palette = "-RdYlGn"
  ) +
tm_shape(border) +
  tm_borders() +
tm_shape(roads) +
  tm_lines(
    lwd="strokelwd", 
    legend.lwd.show = FALSE
  ) +
tm_shape(
  points %>% filter(type == "volcano")
) + 
  tm_symbols(
    shape = 24,
    size = "elevation",
    title.size = "Volcanoes (m)"
  ) +
tm_layout(
  main.title = "Easter Island",
  bg.color="lightblue"
) +
tm_compass(position = c("right", "top")) +
tm_scale_bar() +
tm_credits("Author, 2021") +
tm_add_legend(type = "line", col = "black", title = "Roads")


## Tiles

# https://leaflet-extras.github.io/leaflet-providers/preview/

tm_basemap(
  c( osm  = "OpenStreetMap",
     esri = "Esri.WorldStreetMap")
) +
tm_shape(border, is.master = TRUE) +
  tm_borders() +
tm_shape(roads) +
  tm_lines(
    lwd="strokelwd", 
    legend.lwd.show = FALSE
  ) +
tm_shape(
  points %>% filter(type == "volcano")
) + 
  tm_symbols(
    size = "elevation",
    legend.size.show = FALSE
  )

tm_shape(
  tmaptools::read_osm(border)
) +
  tm_rgb() +
tm_shape(border, is.master = TRUE) +
  tm_borders() +
tm_shape(roads) +
  tm_lines(
    lwd="strokelwd", 
    legend.lwd.show = FALSE
  ) +
tm_shape(
  points %>% filter(type == "volcano")
) + 
  tm_symbols(
    size = "elevation",
    legend.size.show = FALSE
  )
```