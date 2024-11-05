# Download global CHIRPS GEFS
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

# Output directory
Out  <- 'D:/temp/chirpsGEFS'
dir.create(Out,F,T)

# Main function
getChirpsGEFS <- function(startdate, period=10){
  startdate <- as.Date(startdate)
  sdate <- format(as.Date(startdate), "%Y%m%d")
  # CHIRPS base URL
  #https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/05day/precip_mean/
  #https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/10day/precip_mean/
  #https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/15day/precip_mean/
  year <- format(as.Date(startdate, format="%Y-%m-%d"),"%Y")
  month <- format(as.Date(startdate, format="%Y-%m-%d"),"%m")
  day <- format(as.Date(startdate, format="%Y-%m-%d"),"%d")
  if(period==5){
    edate <- as.Date(startdate)+(period-1)#ceiling_date(startdate %m+% days(period), unit = "day")-1 
    edate <- format(as.Date(edate), "%Y%m%d")
    chrps <- paste0('https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/0',period,
                    'day/precip_mean') 
    tfile <- paste0(chrps,'/', 'data-mean_',sdate,'_',edate,'.tif')
  } else if (period==10) {
    edate <- as.Date(startdate)+(period-1)
    edate <- format(as.Date(edate), "%Y%m%d")
    chrps <- paste0('https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/',period,
                    'day/precip_mean') 
    tfile <- paste0(chrps,'/', 'data-mean_',sdate,'_',edate,'.tif')
  } else if (period==15) {
    edate <- as.Date(startdate)+(period-1)
    edate <- format(as.Date(edate), "%Y%m%d")
    chrps <- paste0('https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/',period,
                    'day/precip_mean') 
    tfile <- paste0(chrps,'/', 'data-mean_',sdate,'_',edate,'.tif')
  } else {
    cat('Define a a period e.g 5 or 10 or 15 \n')
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
getChirpsGEFS(startdate, period = 10)

files <- list.files(Out, full.names = T)
tt <- terra::rast(files)

library(terra)














