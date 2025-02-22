library(shiny)
library(leaflet)
library(bslib)
library(auk)
#STATUS AND TRENDS KEY FOR EBIRD 5v9jvtg9qfpb

import_eBird <- function(eBird_infile, eBird_outfile) {
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
                      fileInput("file", label = h3("Weather data input")),
                      hr(),
                      fluidRow(column(4, verbatimTextOutput("value"))),
                      fileInput("file", label = h3("Temperature data input")),     
                      hr(),
                      fluidRow(column(4, verbatimTextOutput("value")))),
#drop down panel 1
      accordion_panel("showing what data is on"),
#drop down panel 2

      accordion_panel(("temperature input"),
        ("this is the change in temp input not the temp data (thats in the data input section")),
#drop down panel 3
      accordion_panel(("accessibility"),
                      radioButtons('changeFont', label = h3("change Font"),
                                   choices = list("normal" = 1, "aria font" = 2),
                                   selected = 1),
                      radioButtons('changeColouraltTitleTurnIntoArianaGrande', label = h3("change colour"),
                                   choices = list("normal" = 1, "yellow" = 2),
                                   selected = 1),),
    ),),
    leafletOutput(outputId = "mymap")
)
server <- function(input, output) {
  
  # output value for file inputs
#  observeEvent(input$changeFont, {
#    shinyjs::html("font-family: arial")
#  })
#  observeEvent(input$changeColouraltTitleTurnIntoArianaGrande, {
#    shinyjs::html("background-color: yellow")
#  })
  
  # define the color palate for the bird count in this area
  pal <- colorQuantile(palette="plasma",domain=result_points$count)
  
  #create the map
  output$mymap <- renderLeaflet({
    leaflet(data) %>%
      setView(lng = 2, lat = 51, zoom = 2)  %>% # Centred on Bristol, UK
      addTiles() %>%
      addRectangles(data = result_points, lng1 = ~longitude-0.5, lat1 = ~latitude-0.5, lng2 = ~longitude+0.5, lat2 = ~latitude+0.5, stroke = FALSE, fillColor = ~pal(count), fillOpacity = 0.3)
  })
  
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
}

shinyApp(ui = ui, server = server)

