##The terra package for ratser and vector data
#https://www.paulamoraga.com/tutorial-terra/

###1 Raster Data
#rast function creates and reads raster data
#raster data for elevation in Luxembourg in terra package
library(terra)
pathraster <- system.file("ex/elev.tif",
                          package = "terra")
r <- rast(pathraster)
plot(r)

#create an object by specifying rows, columns, etc
r <- rast(ncol = 10, nrow = 10, xmin = -150, xmax = -80,
          ymin = 20, ymax = 60)
r
## class       : SpatRaster 
## dimensions  : 10, 10, 1  (nrow, ncol, nlyr)
## resolution  : 7, 4  (x, y)
## extent      : -150, -80, 20, 60  (xmin, xmax, ymin, ymax)
## coord. ref. : lon/lat WGS 84

#size of raster with various functions
nrow(r)
## [1] 10

#use values to set and access raster values
values(r) <- 1:ncell(r)
plot(r)

#create a multilayer object with c
r2 <- r*r
s <- c(r, r2)

#assemble the layers
plot(s[[2]])
plot(min(s))
plot(r + r + 10)
plot(round(r))
plot(r == 1)


###2 Vector Data
#spat vector allows vector data as points, lines, polygons
#use vect to read a shapefile
#shp is a shapefile
pathshp <- system.file("ex/lux.shp", package = "terra")
v <- vect(pathshp)

#use vect to creat spatvector with long + lat coordinates
#long and lat values
long <- c(-0.118092, 2.349014, -3.703339, 12.496366)
lat <- c(51.509865, 48.864716, 40.416729, 41.902782)
longlat <- cbind(long, lat)

#crs
crspoints <- "+proj=longlat +datum=WGS84"

#point attributes
d <- data.frame(place = c("London", "Paris", "Madrid", "Rome"),
                value = c(200, 300, 400, 500))

#spatvector object
pts <- vect(longlat, atts=d, crs=crspoints)
plot(pts)


###Crop, mask, aggregate raster data
#global avg temperature data for Spain
install.packages("geodata")
library(geodata)
r <- geodata::worldclim_country(country = "Spain", var = "tavg",
                                res = 10, path = tempdir())
plot(r)

