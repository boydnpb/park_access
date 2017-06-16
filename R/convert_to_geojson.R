# convert gdb to geojson
library(maptools)
library(rgdal)
library(geojsonio)


# Denver
denver_gdb <- "~/Dropbox (Personal)/Parks Health Spatial Regression/UPDATED_GDBs/Denver_Data.gdb"
tracts <- readOGR(dsn = denver_gdb, layer = "Denver_Final") 
parks <- readOGR(dsn = "~/Dropbox (Personal)/Parks Health Spatial Regression/UPDATED_GDBs/Denver_Data.gdb/", layer = "Denver_All_Parks") 

geojson_write(tracts, file = "data/denver_tracts.geojson")
