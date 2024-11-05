
# By: Benson Kenduiywo
# November, 2024
# Allow users to choose a country boundary to download.
# Allow users to upload CHIRPS raster files.
# Select the date range for processing.
# Compute and display the average annual rainfall over the chosen period.
# Load required packages
# Load required libraries
library(shiny)
library(terra)
library(geodata)
library(RColorBrewer)

# Define UI
ui <- fluidPage(
  
  titlePanel("Baseline average annual CHRIPS precipittion"),
  
  sidebarLayout(
    sidebarPanel(
      # Input to define the data location (directory)
      textInput("dataDir", "Directory containing CHIRPS raster files:", 
                value = "D:/temp/climate/"),
      
      textInput("country", "Country Code (e.g., ZMB for Zambia):", value = "ZMB"),
      
      # Date range input for selecting the desired period
      dateRangeInput("daterange", "Select Date Range:",
                     start = "1991-01-01",
                     end = "2020-12-31"),
      
      # Button to trigger the computation
      actionButton("process", "Compute Average Rainfall")
    ),
    
    mainPanel(
      plotOutput("rainfallMap"),
      verbatimTextOutput("fileList")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Download and plot the country boundary based on the user input
  observeEvent(input$process, {
    
    # Load country boundary
    country_code <- input$country
    shp <- gadm(country_code, level = 1, path = tempdir(), version = "latest")
    print("Displaying country boundary\n")
    # Display the country boundary
    output$rainfallMap <- renderPlot({
      plot(shp)
      text(shp, shp$NAME_1)
    })
    
    # Reactive function to select raster files from the defined directory based on date range
    print("Selecting relevnt files \n")
    selectFiles <- reactive({
      req(input$dataDir)
      
      # Get all the files in the directory
      dir <- input$dataDir
      files <- list.files(dir, pattern="*chirps|*.tif", full.names=TRUE)
      
      # Extract the dates from the file names (assuming dates are at positions 13-22 in the name)
      base_names <- basename(files)
      dates <- as.Date(substr(base_names, 13, 22), format = "%Y.%m.%d")
      
      # Filter files by date range
      i <- (dates >= as.Date(input$daterange[1])) & (dates <= as.Date(input$daterange[2]))
      files[i]
    })
    
    # Show selected file names
    output$fileList <- renderPrint({
      fc <- selectFiles()
      head(fc)
    })
    
    # Compute average annual rainfall
    observeEvent(input$process, {
      
      fc <- selectFiles()
      if (length(fc) == 0) {
        return(NULL)
      }
      
      # Extract years from the file names
      years <- unique(format(as.Date(substr(basename(fc), 13, 22), format = "%Y.%m.%d"), "%Y"))
      print("Computing average precipitation...\n")
      # Define function to compute total rainfall for each year
      totalPrecip <- function(year) {
        ff <- format(as.Date(substr(basename(fc), 13, 22), format = "%Y.%m.%d"), "%Y")
        temp <- fc[which(ff == year)]
        prec <- terra::rast(temp)
        prec <- crop(prec, shp)  # Crop raster to country boundary
        total <- sum(prec, na.rm = TRUE)
        names(total) <- year
        return(total)
      }
      
      # Compute total rainfall for each year and average over the period
      prec <- do.call(c, lapply(years, totalPrecip))
      b_prec <- mean(prec)
      
      # Plot the average annual rainfall
      output$rainfallMap <- renderPlot({
        plot(b_prec, col = hcl.colors(50, palette = "RdBu"), breaks = seq(300, 2500, 200))
        plot(shp, add = TRUE)
        text(shp, shp$NAME_1)
      })
      
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)


