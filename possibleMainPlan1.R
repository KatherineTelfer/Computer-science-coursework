library(auk)
library(shiny)
library(leaflet)
library(bslib)
library(rlang)
library(bit64)
library(curl)
library(countrycode)
library(readr)
library(lubridate)
library(tidyr)
library(shinyalert)
library(dplyr)
library(gridExtra)
library(htmlwidgets)
#STATUS AND TRENDS KEY FOR EBIRD 5v9jvtg9qfpb

import_eBird <- function(eBird_infile, eBird_outfile) {
  #added to the file
  # Reduce the large number of fields in the eBird dataset to a minimal set
  # resulting in reduced RAM usage.
  eBird_fields <- c(
    "GROUP IDENTIFIER",           # Required
    "SAMPLING EVENT IDENTIFIER",  # Required
    "SCIENTIFIC NAME",            # Required
    "OBSERVATION COUNT",
    "COUNTRY",
    "STATE",
    "LOCALITY",
    "LATITUDE",
    "LONGITUDE",
    "OBSERVATION DATE")
  
  print(paste("Loading",eBird_infile,"writing to",eBird_outfile))
  ebd <- auk_ebd(eBird_infile)
  selected <- auk_select(ebd, select=eBird_fields, file=eBird_outfile)
}

transform_eBird_by_month <- function(eBird_infile, eBird_outfile_root) {
  #added to docs
  # Reduce the large number of fields in the eBird dataset to a minimal set
  # resulting in reduced RAM usage.
  # Write data into yearly and monthly units to make manageable chunks.
  eBird_fields <- c(
    "GROUP IDENTIFIER",           # Required
    "SAMPLING EVENT IDENTIFIER",  # Required
    "SCIENTIFIC NAME",            # Required
    "OBSERVATION COUNT",
    "COUNTRY",
    "STATE",
    "LOCALITY",
    "LATITUDE",
    "LONGITUDE",
    "OBSERVATION DATE")
  month_days = c(31,28,31,30,31,30,31,31,30,31,30,31)
  
  print(paste("Loading",eBird_infile))
  for (year in 2014:2024) {
    for (month in 1:12) {
      eBird_outfile <- paste(eBird_outfile_root,year,month,sep="-")
      print(paste("Writing to",eBird_outfile))
      ebd <- auk_ebd(eBird_infile)
      filters <- auk_year(ebd, year)
      filters <- auk_date(ebd, 
                          date = c(paste(year,month,"1",sep="-"), 
                                   paste(year,month,month_days[month],sep="-")))
      filtered <- auk_filter(filters, file = eBird_outfile, overwrite=TRUE)
    }
  }
}

load_eBird_simplified <- function(eBird_infile) {
  eBird_field_formats <- c(
    "character", # SCIENTIFIC NAME
    "character", # OBSERVATION COUNT
    "character", # COUNTRY
    "character", # STATE
    "character", # LOCALITY
    "numeric",   # LATITUDE
    "numeric",   # LONGITUDE
    "Date",      # OBSERVATION DATE
    "character", # SAMPLING EVENT IDENTIFIER
    "NULL"       # GROUP IDENTIFIER
  )
  eBird_data <- read.table(eBird_infile, header=TRUE, sep="\t", quote="",
                           #added to the file
                           colClasses=eBird_field_formats, 
                           comment.char="", fill=TRUE)
  return(eBird_data)
}

load_eBird <- function(eBird_infile) {
  eBird_field_formats <- c(
    "NULL", # GLOBAL.UNIQUE.IDENTIFIER
    "NULL", # LAST.EDITED.DATE
    "NULL", # TAXONOMIC.ORDER
    "NULL", # CATEGORY
    "NULL", # TAXON.CONCEPT.ID
    "NULL", # COMMON.NAME
    "NULL", # SCIENTIFIC.NAME
    "NULL", # SUBSPECIES.COMMON.NAME
    "NULL", # SUBSPECIES.SCIENTIFIC.NAME
    "NULL", # EXOTIC.CODE
    "character", # OBSERVATION.COUNT - NOTE: mostly integers, occasionally 'X'
    "NULL", # BREEDING.CODE
    "NULL", # BREEDING.CATEGORY
    "NULL", # BEHAVIOR.CODE
    "NULL", # AGE.SEX
    "character", # COUNTRY
    "NULL", # COUNTRY.CODE
    "character", # STATE
    "NULL", # STATE.CODE
    "NULL", # COUNTY
    "NULL", # COUNTY.CODE
    "NULL", # IBA.CODE
    "NULL", # BCR.CODE
    "NULL", # USFWS.CODE
    "NULL", # ATLAS.BLOCK
    "character", # LOCALITY
    "NULL", # LOCALITY.ID
    "NULL", # LOCALITY.TYPE
    "numeric", # LATITUDE
    "numeric", # LONGITUDE
    "Date", # OBSERVATION.DATE
    "NULL", # TIME.OBSERVATIONS.STARTED
    "NULL", # OBSERVER.ID
    "NULL", # SAMPLING.EVENT.IDENTIFIER
    "NULL", # PROTOCOL.TYPE
    "NULL", # PROTOCOL.CODE
    "NULL", # PROJECT.CODE
    "NULL", # DURATION.MINUTES
    "NULL", # EFFORT.DISTANCE.KM
    "NULL", # EFFORT.AREA.HA
    "NULL", # NUMBER.OBSERVERS
    "NULL", # ALL.SPECIES.REPORTED
    "NULL", # GROUP.IDENTIFIER
    "NULL", # HAS.MEDIA
    "integer", # APPROVED
    "integer", # REVIEWED
    "NULL", # REASON
    "NULL", # TRIP.COMMENTS
    "NULL"  # SPECIES.COMMENTS
  )
  eBird_data <- read.table(eBird_infile, header=TRUE, sep="\t", quote="",
                           colClasses=eBird_field_formats, 
                           comment.char="", fill=TRUE)
  return(eBird_data)
}

eBird_accumulate <- function(eBird_data) {
  # Generate a histogram for all latitudes and longitudes
  
  # Create an empty array of all latitudes and longitudes for accumulation
  # Array subscript is shifted by (-90,-180) from (latitude,longitude) values
  world_grid <- array(NA,c(180,360))
  for (i in 1:nrow(eBird_data)) {
    record <- eBird_data[i,]
    lat <- as.integer(record$LATITUDE + 0.5 + 90.0)
    lon <- as.integer(record$LONGITUDE + 0.5 + 180.0) 
    if (is.na(world_grid[lat,lon])) {
      world_grid[lat,lon] <- 1L
    } else {
      world_grid[lat,lon] <- world_grid[lat,lon]+1L
    }
  }
  
  # Convert array to list of coordinates and accumulated values for plotting
  result_points <- data.frame(latitude=numeric(), longitude=numeric(), count=numeric())
  for (grid_lat in 1:180) {
    for (grid_long in 1:360) {
      if (is.na(world_grid[grid_lat,grid_long])) { next }
      grid_record <- c(grid_lat-90,grid_long-180,world_grid[grid_lat,grid_long])
      result_points <- rbind(result_points, grid_record)
    }
  }
  # The rbind appears to overwrite column names
  colnames(result_points) <- c("latitude", "longitude", "count")
  return(result_points)
}

eBird_data <- load_eBird("www/nowimfinallyclean-2014-6")
print(paste("Read",nrow(eBird_data),"rows"))
result_points <- eBird_accumulate(eBird_data)
summary(result_points)


#Importing the libraries used to make my application!!
#Main page
ui <- page_sidebar(
  tags$head(
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'bootstrap.css')
  ),
  title = "main page",
#The of the main page
  sidebar = sidebar(
    title = "interesting toggles - change title later",
#the title of the side bar - to grant a little extra info
    accordion(
      accordion_panel(("data input!!!"),
                      submitButton('fileInfo', 'click for info on compatible file types'),
                      #click button to get info on filetypes using the shiny alert library
                      fileInput("dataInput", label = h3("Weather data input"), accept = '.csv'),
                      #file input
                      submitButton('submit'),
                      #submit button
                      hr(),
                      fluidRow(column(4, verbatimTextOutput("value"))),
                      fileInput("tempInput", label = h3("Temperature data input", accept = '.csv')),     
                      submitButton('submit'),
                      #submit button
                      hr(),
                      fluidRow(column(4, verbatimTextOutput("value")))),
#drop down panel 1
      accordion_panel(("showing what data is on"),
                      sliderInput('year', label = h3('year you want to display'), min = 2000, max = 2025, step = 1, value = 2024),
#slider for the year, goes from 1800-2025 in 1 year increments
                      sliderInput('month', label = h3('month you want to display'), min = 1, max = 12, step = 1, value = 1)),
#slider for the month, goes from 1-12 in 1 year increments and is randomly set to jan
#drop down panel 2
            

      accordion_panel(("temperature input"),
        ("this is the change in temp input not the temp data (thats in the data input section"),
        numericInput("tempInput", label = h3("tempInput"), value = 1), min = -5.00, max = 5.00),
        #numeric input for temperature modelling, I set the range to be -5 - 5 as otherwise if there is a 10ºc temp increase the data would be inaccurate as all the birds would be dead so couldnt migrate
#drop down panel 3
      accordion_panel(("accessibility"),
                      radioButtons('changeFont', label = h3("change Font"),
                                   #radio buttons to change the font
                                   choices = list("normal" = 1, "aria font" = 2),
                                   #options of different fonts available w/ normal being the current font
                                   selected = 1),
                      radioButtons('changeColour', label = h3("change colour"),
                                   # radio buttons to change the screen tint
                                   choices = list("normal" = 1, "yellow" = 2),
                                   selected = 1),),
                            #choices of different tints available w/ normal being the current colour
                      radioButtons('changeFontSize', label = h3('change font size'),
                                   choices = list('normal' = 1, 'bigger' = 2, 'smaller' = 3),
                                   #Options for the different font size
                                   selected - 1) # Which button si currently chosen
    ),),
    leafletOutput(outputId = "mymap"),
    actionButton("print", "Print Map")
#print button for the map to be able to download a screenshot style image of the graph
)
server <- function(input, output) {
  # Pull in an entire year of bird migration data.
  eBird_data <- load_eBird("www/ebird_year-2024")
  print(paste("Read",nrow(eBird_data),"rows"))
  
  correlationYay <- function(tempData) {
    tempFieldFormats <- c(
      'integer',
      'numeric',
      'numeric',
      'numeric',
      'numeric',
      'numeric',
      'numeric',
      'numeric',
      'numeric',
      'numeric',
      'numeric',
      'numeric',
      'numeric' #loads in all of the formats form y temp
    )
    temp <- read.table(tempData, header=TRUE, sep='\t', quote='', colClasses=tempFieldFormats, comment.char='')
    
    longifier <- pivot_longer(temp,!Year,names_to="Month",values_to="temp") #longifiying the data
    ymdTemp <- ymd(paste(longifier$Year,longifier$Month,"1",sep="-")) #converts the TSV into year month date format
    mutationOfYmdTempAndLongifier <- mutate(ymdTemp, longifier)
  }
  
  # Function for processing and accumulating a date range of ebird data.
  eBirdAccumulate <- function(dataset,startDate,days) {
    # Generate a histogram for all latitudes and longitudes
    
    # Create an empty array of all latitudes and longitudes for accumulation
    # Array subscript is shifted by (-90,-180) from (latitude,longitude) values
    world_grid <- array(NA,c(180,360))
    
    #  df <- tibble::tibble(lat = seq(from=-90, to=90, length.out=181),
    #                       lon = seq(from=-180, to=180, length.out=361),
    #                       datetime = seq(from = lubridate::ymd("2014-01-01"),
    #                                      to = lubridate::ymd("2015-01-01"),
    #                                      length.out = 12))
    
    for (i in 1:nrow(dataset)) {
      record <- dataset[i,]
      
      # Select observations in date range
      if (record$OBSERVATION.DATE < startDate) { next }
      if (record$OBSERVATION.DATE > startDate + days) { next }
      
      lat <- as.integer(record$LATITUDE + 0.5 + 90.0)
      lon <- as.integer(record$LONGITUDE + 0.5 + 180.0)
      
      # Some observations are counts, others are 'X' simply to indicate that
      # birds of this species were seen. In that case, assume 1 bird.
      if (record$OBSERVATION.COUNT == "X") {
        #added to docs
        record_count = 1L
      } else {
        record_count = as.integer(record$OBSERVATION.COUNT)
      }
      if (is.na(world_grid[lat,lon])) {
        world_grid[lat,lon] <- record_count
      } else {
        world_grid[lat,lon] <- world_grid[lat,lon]+record_count
      }
    }
    
    # Convert array to list of coordinates and accumulated values for plotting
    result_points <- data.frame(latitude=numeric(), longitude=numeric(), count=numeric())
    for (grid_lat in 1:180) {
      for (grid_long in 1:360) {
        if (is.na(world_grid[grid_lat,grid_long])) { next }
        grid_record <- c(grid_lat-90,grid_long-180,world_grid[grid_lat,grid_long])
        result_points <- rbind(result_points, grid_record)
      }
    }
    # The rbind appears to overwrite column names
    colnames(result_points) <- c("latitude", "longitude", "count")
    return(result_points)
  }
  
  mapDate <- ymd("2014-01-01")
  #converts to the correct format
  result_points <- eBirdAccumulate(eBird_data,mapDate,14)
  tempCorrelationPart1 <- correlationYay('www/tempData.tsv')
  #calls the correlation with my file
  tempCorrelationPart2 <- lm(latitude~longitude~temperature, data = tempCorrelationPart1)
  #CORRELATION between latitude, longitude, and temperature
  tempCorrelationPart3 <- predict(tempCorrelationPart2, tempInput)
  #this brings in the temperature correlation equation (in tempCorrelationPart2) with the user  temperature input to creaate a value which I have stored in another variable to be able to predifct data from the project
  
  
  # define the color palate for the bird count in this area
  pal <- colorQuantile(palette="plasma",domain=result_points$count)
  
  # Mouse click event on the leaflet map area
  observeEvent(input$mymap_click, { 
    mapDate <<- mapDate + 14
    #increments up by a fortnite
    print(mapDate)
    result_points <- eBirdAccumulate(eBird_data,mapDate,14)
    
    proxy <- leafletProxy("mymap", data = data)
    proxy %>% clearMarkers() %>% addRectangles(data = result_points, lng1 = ~longitude-0.5, lat1 = ~latitude-0.5, lng2 = ~longitude+0.5, lat2 = ~latitude+0.5, stroke = FALSE, fillColor = ~pal(count), fillOpacity = 0.4)
    print("Updated")
  })

#new end
  
  
  # output value for file inputs
  observeEvent(input$changeFont, {
    shinyjs::html("font-family: arial")
  })
  observeEvent(input$changeColour, {
    shinyjs::html("background-color: yellow")
  })
  
  
  
  # define the color palate for the bird count in this area
  pal <- colorQuantile(palette="plasma",domain=result_points$count)
  
  #create the map
  output$mymap <- renderLeaflet({
    leaflet(data) %>%
      setView(lng = 2, lat = 51, zoom = 2)  %>% # centred on Bristol, UK
      addTiles() %>%
      #the little square bits on the map
      addRectangles(data = result_points, lng1 = ~longitude-0.5, lat1 = ~latitude-0.5, lng2 = ~longitude+0.5, lat2 = ~latitude+0.5, stroke = FALSE, fillColor = ~pal(count), fillOpacity = 0.3)
    #more info on the squares (Esach square is 1 unit of latitude and Longitude)
      addEasyprint(options = easyprintOptions(
        exportOnly = TRUE
      ))
      #Adds the functionallity to the map when the function is called - t exports a copy of the map (As an image) in the sie of the screen)
  })
  
  observeEvent(input$print, {
 #   leafletProxy("mymap") %>%
      #links to my map functions/ the object in my main page
    })
  #an observe function that links the input of the print button and the server actually print bit together in order for this element of my success crtieria to work
  
  observe({
    #shows latitude and longitude
    click <- input$mymap_shape_click
    print("Click event!")
    if(is.null(click))
      return()
    text<-paste("Latitude: ", click$lat, ", Longtitude: ", click$lng)
    print(text)
    text2<-paste("You've selected point ", text)
    map1_proxy <- leafletProxy("myMap") %>%
      clearPopups() %>%
      addPopups(click$lng, click$lat, text)
    output$Click_text<-renderText({
      text2
    })
  })
  observeEvent(input$month, {
    monthObservation <<- input$month
    print(monthObservation)
  })
  observeEvent(input$year, {
    yearObservation <<- input$year
    print(yearObservation)
  })
  
  observeEvent(input$dataInput, {
    #calls a reactive function that updates whenever a file is called
    eBird_data <<- load_eBird(input$dataInput)
    #stores the data the user uploads to my application (after passing it through the load_ebird function in order to simplify and remove uneccassary data) and stores it under the name ebird_data 
  })
  observeEvent(input$tempInput, {
    temp_data <<- input$tempInput
    #stores the data the user uploads to my application and stores it under the name temp data for use elsewhere 
  })
  
  observeEvent(input$fileInfo, {
    # Show a popup when the button is pressed
    shinyalert("File info!", "filetype must be a CSV, all temperatures must be in ºC, all locations must be in latitude and longitude WHITH LABELED HEADERS", type = "error")
    #INFO for the data input using basic strings
  })
  
  withProgress(expr, min = 0, max = 1, value = min + (max - min) * 0.1,
               message = NULL, detail = NULL, style = getShinyOption("progress.style",
                                                                     default = "notification"), session = getDefaultReactiveDomain(),
               env = parent.frame(), quoted = FALSE)
  #setting up the progress bar as like an entity in my code
  
  setProgress(value = NULL, message = NULL, detail = NULL,
              session = getDefaultReactiveDomain())
  #starting my progress bar w zero values
  
  incProgress(amount = 0.1, message = NULL, detail = NULL,
              session = getDefaultReactiveDomain())
  #incrementing up my progress when things happen
}

shinyApp(ui = ui, server = server)

