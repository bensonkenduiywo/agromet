library(shiny)
library(tidyverse)
library(terra)
library(lubridate)
library(R.utils)
library(geodata)

# Define UI for Shiny App
ui <- fluidPage(
  # App title
  titlePanel("CHIRPS GEFS Precipitation Forecasts and Anomalies"),
  
  # Sidebar layout with input and output
  sidebarLayout(
    sidebarPanel(
      # Input: Start date
      dateInput("startdate", "Start Date", value = Sys.Date(), format = "yyyy-mm-dd"),
      
      # Input: Forecast Period (5, 10, or 15)
      numericInput("forecastPeriod", "Forecast Period (days)", value = 10, min = 5, max = 15),
      
      # Input: Forecast Type (forecast or anomaly)
      selectInput("type", "Forecast Type", choices = c("forecast", "anomaly")),
      
      # Input: Output directory
      textInput("outdir", "Output Directory", value = "D:/temp/chirpsGEFS"),
      
      # Input: Country for visualization
      textInput("country", "Country for Visualization", value = "Zambia"),
      
      # Button: Download and process data
      actionButton("download", "Download CHIRPS GEFS Data")
    ),
    
    # Main panel for displaying plots
    mainPanel(
      h4("Forecast and Anomaly Maps"),
      
      # Output: Plot for forecast and anomaly visualization
      plotOutput("forecastPlot", height = "800px")
    )
  )
)

# Define server logic for Shiny App
server <- function(input, output) {
  
  # Main function to download CHIRPS GEFS data
  getChirpsGEFS <- function(startdate, period = 10, type = 'forecast', outDir) {
    startdate <- as.Date(startdate)
    sdate <- format(as.Date(startdate), "%Y%m%d")
    
    # Define base URLs for CHIRPS GEFS
    base_url <- "https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/"
    
    if (type == "forecast") {
      file_type <- "precip_mean"
    } else {
      file_type <- "anom_mean"
    }
    
    # Construct the correct URL based on the period
    chrps_url <- paste0(base_url, sprintf("%02dday", period), "/", file_type)
    edate <- as.Date(startdate) + (period - 1)
    edate <- format(as.Date(edate), "%Y%m%d")
    tfile <- paste0(chrps_url, '/', ifelse(type == "forecast", "data-mean_", "anomaly-mean_"), sdate, '_', edate, '.tif')
    
    # Destination file in the output directory
    dfile <- file.path(outDir, basename(tfile))
    
    if (!file.exists(dfile)) {
      tryCatch({
        utils::download.file(url = tfile, destfile = dfile, mode = "wb")
      }, error = function(e) {
        cat(paste0(basename(tfile), ' download failed.\n'))
      })
    }
    
    return(dfile)
  }
  
  # Function to load the forecast and anomaly files using pattern matching
  loadFiles <- function(startdate, outDir) {
    sdate <- format(as.Date(startdate), "%Y%m%d")
    
    # Pattern for the forecast and anomaly files
    forecastPattern <- paste0("^data-mean_", sdate, "_.+.*.tif$")
    anomalyPattern <- paste0("^anomaly-mean_", sdate, "_.+.*.tif$")
    
    # Search for files in the provided directory
    forecastFile <- list.files(path = outDir, pattern = forecastPattern, full.names = TRUE)
    anomalyFile <- list.files(path = outDir, pattern = anomalyPattern, full.names = TRUE)
    
    return(list(forecast = forecastFile, anomaly = anomalyFile))
  }
  
  # Observe the "Download" button press
  observeEvent(input$download, {
    
    # Ensure the output directory exists
    if (!dir.exists(input$outdir)) {
      dir.create(input$outdir, recursive = TRUE)
    }
    
    # Download the forecast and anomaly data
    getChirpsGEFS(input$startdate, input$forecastPeriod, 'forecast', input$outdir)
    getChirpsGEFS(input$startdate, input$forecastPeriod, 'anomaly', input$outdir)
    
    # Load the forecast and anomaly files using pattern matching
    files <- loadFiles(input$startdate, input$outdir)
    
    # Check if the files exist
    if (length(files$forecast) == 0 || length(files$anomaly) == 0) {
      showNotification("Forecast or anomaly files not found in the specified directory.", type = "error")
      return(NULL)
    }
    
    # Render the plot once files are found
    output$forecastPlot <- renderPlot({
      
      # Load the country boundary
      shp <- geodata::gadm(input$country, level = 1, path = tempdir(), version = "latest")
      
      # Load the forecast and anomaly rasters
      fimg <- terra::rast(files$forecast)
      aimg <- terra::rast(files$anomaly)
      
      # Clip the rasters to the country boundary
      fimg <- crop(fimg, shp)
      aimg <- crop(aimg, shp)
      
      # Visualize the forecast and anomaly maps
      par(mfrow = c(2, 2))
      plot(shp, main = input$country)
      text(shp, shp$NAME_1)
      plot(fimg, col = hcl.colors(50, palette = "RdBu"), main = paste(input$country, input$forecastPeriod, 'Day Forecasted Rainfall'))
      plot(shp, add = TRUE)
      plot(aimg, col = hcl.colors(50, palette = "RdBu"), main = paste(input$country, input$forecastPeriod, 'Day Forecasted Rainfall Anomaly'))
      plot(shp, add = TRUE)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
