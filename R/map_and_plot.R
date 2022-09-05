# Ce script permet de réaliser les graphiques et la carte employé dans le poster
# Il s'agit d'un duplicat du code du document mdc2022_poster.qmd

SciViews::R
# Carte

library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")

world <- ne_countries(scale = "medium", returnclass = "sf")
#class(world)

station <- read(here::here("data/station_medit.csv")) %>.%
  mutate(., latitude = as.numeric(latitude), longitude = as.numeric(longitude))

station_v <- filter(station, trait == "vertical")
station_hv <- filter(station, trait == "horizontal-vertical")
station_do <- filter(station, trait == "Double oblique")

map <- ggplot(data = world) +
  geom_sf(color = "black", fill = "#C8AD7F") +
  coord_sf(xlim = c(-8, 40), ylim = c(30, 48), expand = FALSE) +
  xlab(NULL) + ylab(NULL) +
  #ggtitle("Mer Méditerranée") +
  theme(panel.background = element_rect(fill="#80D0D0")) +
  #annotation_north_arrow(location = "bl", which_north = "true",pad_x = unit(0, "in"), pad_y = unit(2.6, "in"), style = north_arrow_fancy_orienteering) +
  annotation_north_arrow(location = "bl", which_north = "true",pad_x = unit(0, "in"), pad_y = unit(2, "in"), style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl") +
  annotate(geom = "point", x = station_v$longitude, y = station_v$latitude, size = 2, color = "white") +
  annotate(geom = "point", x = station_hv$longitude, y = station_hv$latitude, size = 2, color = "red") +
  annotate(geom = "point", x = station_do$longitude, y = station_do$latitude, size = 2, color = "purple")

map
ggsave(filename = "figures/r_figures/map.tiff", device = "tiff", units = "cm", width = 15, height =15)


# Graph 1

tsh <- read(here::here("data/ts_horiz_abd_tot.rds"))
tsv <- read(here::here("data/ts_verti_abd_tot.rds"))

p <- chart(data = tsh, abd_tot ~ time) +
  geom_line() +
  ylab(expression(paste("Abondance totale [ind/",  m^{3}, "]"))) +
  geom_line(data = tsv, color = "red") +
  theme(text = element_text(size=9), axis.text = element_text(size=9))
p
ggsave("figures/r_figures/adb_tot.tiff",
  units="cm", device = "tiff", width = 11, height = 10)


# Graph2


hv_comp <- read(here::here("data/hv_rel_abd.rds"))
hv_comp <- hv_comp*100
# Phaeodaria ----
max_lab <- max(c(hv_comp$rel_abd_phaeodaria_v, hv_comp$rel_abd_phaeodaria_h))

p <- chart(data = hv_comp, rel_abd_phaeodaria_v ~ rel_abd_phaeodaria_h) +
  geom_abline(intercept = 0, color="black", linetype="dashed", size=1)+
  geom_point(color = "#649B88") +
  scale_x_continuous(
    name="Abondance relative (%) - horizontal",
    limits = c(0, max_lab*1.5)) +
      scale_y_continuous(
        name="Abondance relative (%) - vertical",
        limits = c(0, max_lab*1.5))+
  geom_smooth(method = MASS::rlm, formula = y~x-1, color="red",linetype="dashed", size=0.8, se= FALSE) +
  theme(
    legend.title = element_blank(),
    text = element_text(size = 14),
    plot.title = element_text(hjust=0.5, size=16, face = "italic")) +
  labs(title = "Phaeodaria")
p

ggsave("figures/r_figures/adb_rel_phaeo.tiff",
  units= "cm", device = "tiff", width = 10, height = 10)

max_lab <- max(c(hv_comp$rel_abd_fritillariidae_v, hv_comp$rel_abd_fritillariidae_h))

p <- chart(data = hv_comp, rel_abd_fritillariidae_v ~ rel_abd_fritillariidae_h) +
  geom_abline(intercept = 0, color="black", linetype = "dashed", size = 1)+
  geom_point(color = "#649B88")+
  scale_x_continuous(
    name="Abondance relative (%) - horizontal",
    limits = c(0, max_lab*1.5))+
      scale_y_continuous(
        name="Abondance relative (%) - vertical",
        limits = c(0, max_lab*1.5)) +
  geom_smooth(method = MASS::rlm, formula = y~x-1, color="red",linetype="dashed", size=0.8, se= FALSE) +
  theme(
    legend.title = element_blank(),
    text = element_text(size = 14),
    plot.title = element_text(hjust=0.5, size=16, face = "italic")) +
  labs(title = "Fritillariidae")
p

ggsave("figures/r_figures/adb_rel_fritillaridae.tiff",
  units= "cm", device = "tiff", width = 10, height = 10)


max_lab <- max(c(hv_comp$rel_abd_decapoda_zoea_larva_v, hv_comp$rel_abd_decapoda_zoea_larva_h))

p <- chart(data = hv_comp, rel_abd_decapoda_zoea_larva_v ~ rel_abd_decapoda_zoea_larva_h) +
  geom_abline(intercept = 0, color="black", linetype="dashed", size=1)+
  geom_point(color = "#649B88")+
  scale_x_continuous(
    name="Abondance relative (%) - horizontal",
    limits = c(0, max_lab*1.5))+
      scale_y_continuous(
        name="Abondance relative (%) - vertical",
        limits = c(0, max_lab*1.5)) +
  geom_smooth(method = MASS::rlm, formula = y~x-1, color="red",linetype="dashed", size=0.8, se= FALSE) +
  theme(
    legend.title = element_blank(),
    text = element_text(size = 14),
    plot.title = element_text(hjust=0.5, size=16, face = "italic")) +
  labs(title = "Decapoda")

p
ggsave("figures/r_figures/adb_rel_decapoda.tiff",
  units= "cm", device = "tiff", width = 10, height = 10)

max_lab <- max(c(hv_comp$rel_abd_cladocera_other_v, hv_comp$rel_abd_cladocera_other_h))

p <- chart(data = hv_comp, rel_abd_cladocera_other_v ~ rel_abd_cladocera_other_h) +
  geom_abline(intercept = 0, color="black", linetype="dashed", size=1)+
  geom_point(color = "#649B88")+
  scale_x_continuous(
    name="Abondance relative (%) - horizontal",
    limits = c(0, max_lab*1.5))+
      scale_y_continuous(
        name="Abondance relative (%) - vertical",
        limits = c(0, max_lab*1.5)) +
  geom_smooth(method = MASS::rlm, formula = y~x-1, color="red",linetype="dashed", size=0.8, se= FALSE) +
  theme(
    legend.title = element_blank(),
    text = element_text(size = 14),
    plot.title = element_text(hjust=0.5, size=16, face = "italic")) +
  labs(title = "Cladocera")
p
ggsave("figures/r_figures/adb_rel_cladocera.tiff",
  units= "cm", device = "tiff", width = 10, height = 10)
