####Drawing beautiful maps programmatically with R, sf, and ggplot2
###Part 1: Basics
install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel",
                   "ggspatial", "lwgeom", "sf", "rnaturalearth",
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




###Part 2: Layers
##Define two study sites in a dataframe
(sites <- data.frame(longitude = c(-80.144005, -80.109), latitude = c(26.479005, 26.83)))

#add pt coordinates with geom_point and define aesthetics
ggplot(data = world)+geom_sf()+geom_point(data = sites, aes(x=longitude, y=latitude),
                                          size = 4, shape = 23, fill = "purple")+
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

#convert dataframe to sf object to overcome coordinate system when two objects aren't in same projection
#WGS84 CRS code #4326 projection must be defined in sf object
(sites <- st_as_sf(sites, coords = c("longitude", "latitude"), 
                   crs = 4326, agr = "constant"))
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = sites, size = 4, shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

#adding states/polygon data
install.packages("maps")
library("maps")
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)

#add state names by centroid of each state polygon (not long/lat exact)
sf::sf_use_s2(FALSE)
states <- cbind(states, st_coordinates(st_centroid(states)))

#capitalize state names
library("tools")
states$ID <- toTitleCase(states$ID)
head(states)
ggplot(data = world)+geom_sf()+geom_sf(data = states, fill = NA)+
  geom_text(data = states, aes(X, Y, label = ID), size = 5)+
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

#move state names
state$nudge_y <- -1
state$nudge_y[states$ID == "Florida"] <- 0.5
state$nudge_y[states$ID == "South Carolina"] <- -1.5

#improve readability 
ggplot(data = world)+geom_sf()+geom_sf(data = states, fill = NA)+
  geom_label(data = states, aes(X, Y, label = ID), size = 5,
  fontface = "bold", nudge_y = states$nudge_y)+
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

#add counties
install.packages("lwgeom")
library("lwgeom")
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("florida", counties$ID))
counties$area <- as.numeric(st_area(counties))
head(counties)
ggplot(data = world)+geom_sf()+geom_sf(data = counties, fill = NA, color = gray(.5))+
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

#add color
ggplot(data = world)+geom_sf()+geom_sf(data = counties, aes(fill = area))+
  scale_fill_viridis_c(trans = "sqrt", alpha = .4)+
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

#add cities
flcities <- data.frame(state = rep("Florida", 5), city = c("Miami", 
                                                           "Tampa", "Orlando", "Jacksonville", "Sarasota"), 
                       lat = c(25.7616798, 27.950575, 28.5383355, 30.3321838, 27.3364347), lng = c(-80.1917902, -82.4571776, -81.3792365, -81.655651, -82.5306527))
(flcities <- st_as_sf(flcities, coords = c("lng", "lat"), remove = FALSE, 
                      crs = 4326, agr = "constant"))

#add city locations and names
ggplot(data = world)+geom_sf()+geom_sf(data = counties, fill = NA, color = gray(.5))+
  geom_sf(data = flcities)+
  geom_text(data = flcities, aes(x = lng, y = lat, label = city), 
            size = 3.9, col = "black", fontface = "bold")+
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

#more readability
library("ggrepel")
ggplot(data = world)+geom_sf()+geom_sf(data = counties, fill = NA, color = gray(.5))+
  geom_sf(data = flcities)+
  geom_text_repel(data = flcities, aes(x = lng, y=lat, label = city),
                  fontface = "bold", nudge_x = c(1, -1.5, 2, 2, -1), nudge_y = c(0.25, 
                                                                                 -0.25, 0.5, 0.5, -0.5)) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

#final map: general bg, state/county lines, labels, theme adjustments
library("ggspatial")
ggplot(data = world)+
  geom_sf(fill = "antiquewhite1")+
  geom_sf(data = counties, aes(fill = area))+
  geom_sf(data = states, fill = NA)+
  geom_sf(data = sites, size = 4, shape = 23, fill = "darkred")+
  geom_sf(data = flcities)+
  geom_text_repel(data = flcities, aes(x = lng, y = lat, label = city),
                  fontface = "bold", nudge_x = c(1, -1.5, 2, 2, -1), nudge_y = c(0.25, 
                                                                                 -0.25, 0.5, 0.5, -0.5)) +
  geom_label(data = states, aes(X, Y, label = ID), size = 5, fontface = "bold", 
             nudge_y = states$nudge_y) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4)+
  annotation_scale(location = "bl", width_hint = 0.4)+
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)+
             xlab("Longitude") + ylab("Latitude")+
  ggtitle("Observation Sites", subtitle = "(2 sites in Palm Beach County, FL)")+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype= "dashed",
                                        linewidth = 0.5), panel.background = element_rect(fill = "aliceblue"))

#saving
ggsave("maptwo.pdf")
ggsave("maptwo_web.png", width = 6, height = 6, dpi = "screen")
