# Download global CHIRPS
# By: Benson Kenduiywo
# November, 2024
library(shiny)
library(pacman)
library(tidyverse)
library(terra)
library(lubridate)
library(R.utils)

# Define UI
ui <- fluidPage(
  
  titlePanel("CHIRPS Data Downloader"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange", 
                     "Select Date Range:", 
                     start = "1991-01-01", 
                     end = "2020-12-31",
                     min = "1981-01-01", 
                     max = Sys.Date()),
      
      textInput("outputDir", "Output Directory:", 
                value = "D:/temp/climate/"),
      
      actionButton("downloadBtn", "Start Download")
    ),
    
    mainPanel(
      verbatimTextOutput("status")
    )
  )
)

# Define Server logic
server <- function(input, output, session) {
  
  # Function to download CHIRPS data
  getChirps <- function(date, outDir) {
    chrps <- 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/p05'
    Day  <- date
    Year <- lubridate::year(Day)
    tfile <- paste0(chrps, '/', Year, '/chirps-v2.0.', gsub('-', '.', Day, fixed = TRUE), '.tif.gz')
    dfile <- paste0(outDir, '/', basename(tfile))
    rfile <- gsub('.gz', '', dfile, fixed = TRUE)
    
    if (!file.exists(rfile)) {
      if (!file.exists(dfile)) {
        tryCatch({
          utils::download.file(url = tfile, destfile = dfile)
          R.utils::gunzip(dfile)
          return(paste0('Image ', basename(rfile), ' processed correctly!!!'))
        }, error = function(e) {
          return(paste0(basename(tfile), ' failed.'))
        })
      } else {
        R.utils::gunzip(dfile)
        return(paste0('Image ', basename(rfile), ' processed correctly!!!'))
      }
    } else {
      return(paste0('Image ', basename(rfile), ' already exists!'))
    }
  }
  
  # Reactive observer for the download button
  observeEvent(input$downloadBtn, {
    # Output directory
    Out <- input$outputDir
    dir.create(Out, showWarnings = FALSE, recursive = TRUE)
    
    # Time frame
    dts <- seq(from = input$dateRange[1], to = input$dateRange[2], by = 'day')
    
    # Loop through the dates
    result <- vector("character", length(dts))  # Initialize a result vector
    for (i in seq_along(dts)) {
      result[i] <- getChirps(date = dts[i], outDir = Out)
      output$status <- renderPrint({
        result[i]
      })
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
