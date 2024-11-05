#1. Donwload a country boundary
#2. 
########################################################################################
library(terra)
library(geodata)
library(RColorBrewer)
#Define path to climate data


#Donwload Country Boundary
country <- 'ZMB'
shp <- gadm(country, level = 1, path =tempdir(), version = "latest")
plot(shp)
text(shp,shp$NAME_1)

#Load CHRIPS files and select files of defined date range

selectFiles <- function(startdate, enddate) {
  dir <-'D:/temp/climate/'
  files <- list.files(dir, pattern="*chirps|*.tif", full.names=TRUE)
  base_names <- basename(files)
  dates <- as.Date(substr(base_names, 13, 22), format="%Y.%m.%d")
  i <- (dates >= as.Date(startdate)) & (dates <= as.Date(enddate) )
  files[i]
}
startdate <- "1991-01-01"
enddate <- "2020-12-31"

fc <- selectFiles(startdate, enddate)
head(fc)
tail(fc)
#===============================
#Computed average annual rainfail over the period chose
years <- unique(format(as.Date(substr(basename(fc), 13, 22), format="%Y.%m.%d"),"%Y"))

#Define function to compute the average of each raster file
totalPrecip <- function(year){
  ff <- format(as.Date(substr(basename(fc), 13, 22), format="%Y.%m.%d"),"%Y")
  temp <- fc[which(ff==year)]
  prec <- terra::rast(temp)
  prec <- crop(prec, shp)
  total <- sum(prec, na.rm = T)
  names(total) <- year
  rm(temp,ff)
  return(total)
}

# Compute the total rainfall for each year
prec <- do.call(c, lapply(years, totalPrecip))
b_prec <- mean(prec)

#display.brewer.pal(11,'RdYlBu')
plot(b_prec, col = hcl.colors(50, palette = "RdBu"), breaks=seq(300,2500,200))
plot(shp, add=T)
text(shp, shp$NAME_1)
