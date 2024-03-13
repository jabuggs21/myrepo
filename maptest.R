####Drawing beautiful maps programmatically with R, sf, and ggplot2
###Part 1: Basics
install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel",
                   "ggspatial", "libwgeom", "sf", "rnaturalearth",
                   "rnaturalearthdata"))
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

##NOTE: sf has replaced sp in spatial data in R
##sf stands for "simple feature", ggplot2 allows sf in layers of a map

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#title/subtitle, axis labels, color fill/gradient by pop sqrt
ggplot(data = world) + geom_sf(aes(fill = pop_est))+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")
  +xlab ("Longitude") + ylab ("Latitude")+
  ggtitle("World Map", subtitle = "(251 countries)")

##coord_sf fo coordinate system, includes projection and extent of map
##map will use coordinates on first layer that defines one
##can be overridden with crs argument

#LAEA Lambert Azimuthal Equal Area projection (PROJ4 string)
ggplot(data = world)+geom_sf()+coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")

#can use SRID or EPSG code for the projection instead of PROJ4 (2 ways)
ggplot(data = world)+geom_sf()+coord_sf(crs = "+init=epsg:3035")
ggplot(data = world)+geom_sf()+coord_sf(crs = st_crs(3035))

#extent of map can also be set in coord_sf to zoom in
#limits already expanded, turn off with expand = FALSE
ggplot(data = world)+geom_sf()+coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)

#scale bars on a map with ggspatial
library("ggspatial")
ggplot(data = world) + geom_sf() + annotation_scale(location = "bl", width_hint = 0.5)+
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97))

#annotating maps (sf package is critical)
world_points <- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

ggplot(data = world)+
  geom_sf()+
  geom_text(data = world_points, aes(x=X, y=Y, label=name),
            color = "purple", fontface = "bold", check_overlap = FALSE)+
  annotate(geom = "text", x = -90, y = 26, label = "Gulf of Mexico",
           fontface = "italic", color = "darkblue", size = 6)+
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)

#final map
<<<<<<< HEAD
ggplot(data = world) + geom_sf(fill = "antiquewhite")+ 
  geom_text(data = world_points,aes(x=X, y=Y, label=name), color = "darkblue",
            fontface = "bold", check_overlap = FALSE)+
  annotate(geom = "text", x=-90, y=26, label="Gulf of Mexico", fontface = "italic",
color = "grey22", size = 6) + annotation_scale(location = "bl", width_hint = 0.5)+
  annotation_north_arrow(location = "bl", which_north = "true", pad_x=unit(0.75, "in"),
                         pad_y=unit(0.5, "in"), style = north_arrow_fancy_orienteering)+
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)+
  xlab("Longitude")+ylab("Latitude")+ggtitle("Map of the Gulf of Mexico and the Caribbean Sea")+
  theme(panel.grid.major = element_line(color=gray(.5), linetype = "dashed", linewidth = 0.5),
        panel.background = element_rect(fill = "aliceblue"))

#saving
ggsave("mapone.pdf")
ggsave("mapone_web.png", width = 6, height = 6, dpi = "screen")






>>>>>>> 7933c3db7421068aa13e216eed81afc157cd9251



