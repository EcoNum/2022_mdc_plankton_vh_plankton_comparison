# carte des stations participant time series for the IGMETS.
SciViews::R
library(sf)
library(mapview)

station <- read(here::here("data/station_medit.csv")) %>.%
  mutate(., latitude = as.numeric(latitude), longitude = as.numeric(longitude))

station <- st_as_sf(station, coords = c("longitude", "latitude"), crs = 4326)
mapview(station, zcol = "trait", map.types = "Esri.WorldShadedRelief", layer.name = "Station")


# La carte ----------------------------------------------------------------

# importation des packages pour la création de la carte
library("ggplot2")
theme_set(theme_bw()) # on change le thème pour la carte
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")


library(ggspatial)
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

station <- read(here::here("data/station_medit.csv")) %>.%
  mutate(., latitude = as.numeric(latitude), longitude = as.numeric(longitude))

station_v <- filter(station, trait == "vertical")
station_hv <- filter(station, trait == "horizontal-vertical")
station_do <- filter(station, trait == "Double oblique")

(map <- ggplot(data = world) +
  geom_sf(color = "black", fill = "#C8AD7F") +
  coord_sf(xlim = c(-8, 40), ylim = c(30, 48), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Mer Méditerranée") +
  theme(panel.background = element_rect(fill="#80D0D0")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0, "in"), pad_y = unit(2.6, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl") +
  annotate(geom = "point", x = station_v$longitude, y = station_v$latitude, size = 3, color = "yellow") +
  annotate(geom = "point", x = station_hv$longitude, y = station_hv$latitude, size = 3, color = "red") +
  annotate(geom = "point", x = station_do$longitude, y = station_do$latitude, size = 3, color = "purple"))

map
#ggsave(filename = "figures/r_figures/map.tiff", device = "tiff", units = "cm", width = 12, height = 8)

