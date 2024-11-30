#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(bslib)
#basically CSS for shiny
library('shiny')
#makes web applications

# Define UI for my application
fluidPage(
  
 
  fileInput("file", label = h3("migration data input")),
  hr(),
  fluidRow(column(4, verbatimTextOutput("value"))),
  
  fileInput("file", label = h3("temperature data input")),
  hr(),
  fluidRow(column(4, verbatimTextOutput("value"))),
    


  # Application title
  titlePanel("How do bird migration patterns vary agains temperature change"),
  #(working title - find a catchier title)

    
)
