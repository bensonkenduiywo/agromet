# Download global CHIRPS GEFS and visualize data
# By: Benson Kenduiywo
# November, 2024

# R options
g <- gc(reset = T); rm(list = ls()) # Empty garbage collector
options(warn = -1, scipen = 999)    # Remove warning alerts and scientific notation
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(tidyverse,terra,lubridate,R.utils))
library(lubridate)
# Time frame
startdate <- '2024-11-01'
forecastPeriod <- 10
# Output directory
Out  <- 'D:/temp/chirpsGEFS'
dir.create(Out,F,T)

# Main function
getChirpsGEFS <- function(startdate, period=10, type='forecast'){
  startdate <- as.Date(startdate)
  sdate <- format(as.Date(startdate), "%Y%m%d")
  # CHIRPS base URL
  #https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/05day/precip_mean/
  #https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/10day/precip_mean/
  #https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/15day/precip_mean/
  year <- format(as.Date(startdate, format="%Y-%m-%d"),"%Y")
  month <- format(as.Date(startdate, format="%Y-%m-%d"),"%m")
  day <- format(as.Date(startdate, format="%Y-%m-%d"),"%d")
  if(period==5 & type=='forecast'){
    print('Donwloading 5 day average precipitation forecast')
    edate <- as.Date(startdate)+(period-1)#ceiling_date(startdate %m+% days(period), unit = "day")-1 
    edate <- format(as.Date(edate), "%Y%m%d")
    chrps <- paste0('https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/0',period,
                    'day/precip_mean') 
    tfile <- paste0(chrps,'/', 'data-mean_',sdate,'_',edate,'.tif')
  } else if (period==10 & type=='forecast') {
    print('Donwloading 10 day average precipitation forecast')
    edate <- as.Date(startdate)+(period-1)
    edate <- format(as.Date(edate), "%Y%m%d")
    chrps <- paste0('https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/',period,
                    'day/precip_mean') 
    tfile <- paste0(chrps,'/', 'data-mean_',sdate,'_',edate,'.tif')
  } else if (period==15 & type=='forecast') {
    print('Donwloading 15 day average precipitation forecast')
    edate <- as.Date(startdate)+(period-1)
    edate <- format(as.Date(edate), "%Y%m%d")
    chrps <- paste0('https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/',period,
                    'day/precip_mean') 
    tfile <- paste0(chrps,'/', 'data-mean_',sdate,'_',edate,'.tif')
  } else if (period== 5 & type=='anomaly') {
    print('Donwloading 5 day average forecast precipitation anomaly')
    edate <- as.Date(startdate)+(period-1)
    edate <- format(as.Date(edate), "%Y%m%d")
    chrps <- paste0('https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/0',period,
                     'day/anom_mean') 
    tfile <- paste0(chrps,'/', 'anomaly-mean_',sdate,'_',edate,'.tif')
  } else if (period==10 & type=='anomaly') {
    print('Donwloading 10 day average forecast precipitation anomaly')
    edate <- as.Date(startdate)+(period-1)
    edate <- format(as.Date(edate), "%Y%m%d")
    chrps <- paste0('https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/',period,
                    'day/anom_mean') 
    tfile <- paste0(chrps,'/', 'anomaly-mean_',sdate,'_',edate,'.tif')
  } else if (period==15 & type=='anomaly') {
    print('Donwloading 15 day average forecast precipitation anomaly')
    edate <- as.Date(startdate)+(period-1)
    edate <- format(as.Date(edate), "%Y%m%d")
    chrps <- paste0('https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/',period,
                    'day/anom_mean') 
    tfile <- paste0(chrps,'/', 'anomaly-mean_',sdate,'_',edate,'.tif')
  }
  
  
  else {
    cat('Define a a period e.g 5 or 10 or 15 and type of forecast (forecast or anomaly) \n')
  }
  
  # Destination file
  dfile <- paste0(Out,'/',basename(tfile))
  
  if(!file.exists(dfile)){
    
    # Downloading
    if(!file.exists(dfile)){
      tryCatch(expr = {
        utils::download.file(url = tfile, destfile = dfile, mode = "wb")
      },
      error = function(e){
        cat(paste0(basename(tfile),' failed.\n'))
      })
    }
    
    # Unzip
    #R.utils::gunzip(dfile)
    return(cat(paste0('Image ',basename(dfile),' processed correctly!!!\n')))
  } else {
    return(cat(paste0('Image ',basename(dfile),' already exists!\n')))
  }
  
}

#Download data
getChirpsGEFS(startdate, period = forecastPeriod, type = 'forecast')
getChirpsGEFS(startdate, period = forecastPeriod, type = 'anomaly')
#Visualize the forecasts
#Donwload Country Boundary
country <- 'Zambia'
shp <- geodata::gadm(country, level = 1, path =tempdir(), version = "latest")

afile <- list.files(Out, pattern=paste0("^anomaly-mean.*",".+.",format(as.Date(startdate), "%Y%m%d"),".+.","*.tif"), full.names=TRUE)
ffile <- list.files(Out, pattern=paste0("^data-mean.","*.+.*",format(as.Date(startdate), "%Y%m%d"),"*.+.*.tif"), full.names=TRUE)
# load Anomaly image, clip to country boundary and display
aimg <- terra::rast(afile)
aimg <- crop(aimg, shp)
fimg <- terra::rast(ffile)
fimg <- crop(fimg, shp)
#Visualize plots
par(mfrow=c(2,2))
plot(shp, main=country)
text(shp,shp$NAME_1)
plot(fimg, col = hcl.colors(50, palette = "RdBu"),
     main=paste(country, forecastPeriod, 'forecasted cumulative rainfall' ))
plot(shp, add=T)
plot(aimg, col = hcl.colors(50, palette = "RdBu"),
     main=paste(country, forecastPeriod, 'forecasted cumulative rainfall anomaly' ))
plot(shp, add=T)











