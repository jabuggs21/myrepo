library(tidyverse)
library(readxl)
library(here)
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")



here()
bradymod <- read_excel(here("modified_brady_mags_latlon.xlsx"))
bradymod <- st_as_sf(bradymod, coords=c("Lon", "Lat"), crs = 4326)

world <- st_transform(crs=4326)

ggplot()+
  geom_sf(data = world)+
  geom_sf(data = bradymod)+
  theme_bw()
#####
data <- read_excel("modified_brady_mags_latlon.xlsx")
df <- data.frame(data)
head(df)
library("maps")

head(world)

map(database = "world")
points(x = df$Lat[1:500], y = df$Lon[1:500], col = "Red")


