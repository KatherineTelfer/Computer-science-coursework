#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.

library('shiny')
# Define server logic required to draw a histogram

function(input, output) {
  
  # You can access the value of the widget with input$file, e.g.
  output$value <- renderPrint({
    str(input$file)
  })
  
}



