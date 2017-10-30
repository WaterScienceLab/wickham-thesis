#### Stations plot

## State of Nebraska as base map
## Add county boundaries
## Add subbasin (HUC8) or watershed (HUC10) or NRD
## plot sample points

setwd("C:/Users/meglarse/GitHub/wickham-thesis/")

## Attach required packages for maps
library(sp)
library(maps)
library(mapdata)
library(maptools)
library(scales)
library(rgdal)
library(ggplot2)
library(lattice)
library(latticeExtra)
require(raster)
require(rgeos)

## Read in data files
stations <- read.csv("./data/station.csv")
stations <- stations[stations$OrganizationIdentifier == "21NEB001_WQX",]
sta.pts <- SpatialPoints(cbind(stations$LongitudeMeasure,stations$LatitudeMeasure), CRS("+proj=longlat +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))


## Read in spatial files
cities <- readOGR("./spatial-data/cities/citiesx010g.shp")    #https://catalog.data.gov/dataset/usgs-small-scale-dataset-cities-and-towns-of-the-united-states-201403-shapefile
govt.boundaries <- readOGR("./spatial-data/CountyBoundsUTM/CountyUTM.shp")
nrd <- readOGR("./spatial-data/NRDUTM/NRDUTM.shp")

## Hydrography
NHDWaterbody <- readOGR("./spatial-data/NHD_H_Nebraska_Shape/Shape/NHDWaterbody.shp")
NHDArea <- readOGR("./spatial-data/NHD_H_Nebraska_Shape/Shape/NHDArea.shp")
huc6 <- readOGR("./spatial-data/NHD_H_Nebraska_Shape/Shape/WBDHU6.shp")
huc8 <- readOGR("./spatial-data/NHD_H_Nebraska_Shape/Shape/WBDHU8.shp")
huc10 <- readOGR("./spatial-data/NHD_H_Nebraska_Shape/Shape/WBDHU10.shp")

# Transform data to same projections, etc.
cities <- spTransform(cities,CRS("+proj=longlat +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
govt.boundaries <- spTransform(govt.boundaries,CRS("+proj=longlat +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
nrd <- spTransform(nrd,CRS("+proj=longlat +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
NHDWaterbody <- spTransform(NHDWaterbody,CRS("+proj=longlat +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
NHDArea <- spTransform(NHDArea,CRS("+proj=longlat +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
huc6 <- spTransform(huc6,CRS("+proj=longlat +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
huc8 <- spTransform(huc8,CRS("+proj=longlat +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
huc10 <- spTransform(huc10,CRS("+proj=longlat +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

## Maps! ####
plot(govt.boundaries, border = "black", lty = 3)
plot(nrd, border = "grey25", lwd = 1, 
     col = viridis(nrow(nrd@data), alpha = 0.75, begin = 0, end = 1, direction = 1, option = "D"),add=T)
#huc8.crop <- crop(huc8, govt.boundaries)
#plot(huc8.crop, add= T, 
#     border = "grey75", lwd = 1, 
#     col = alpha("grey", 0.25))

#NHDArea.crop <- crop(NHDArea, govt.boundaries)
#plot(NHDArea[NHDArea.crop$FCODE == "46006" | NHDArea.crop$FCODE == "46003",], add = T, col = "#9bbff4", border = "#9bbff4")

#NHDWaterbody.crop <- crop(NHDWaterbody, govt.boundaries)
#plot(NHDWaterbody.crop[NHDWaterbody.crop$AREASQKM > 0.05,], add = T, col = "#a7cdf2", border = "#9bbff4")

plot(sta.pts, cex = 1, pch = 19, add = T)

library(maps) 
maps::map.scale(relwidth = .15, metric = FALSE, ratio = TRUE, lwd = 1.75)
