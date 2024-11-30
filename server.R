#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.

library('shiny')
# Define server logic required to draw a histogram
library('leaflet')

function(input, output) {
  output$map <- renderLeaflet({ 
    leaflet() |> 
      addTiles()  |> 
      setView(0.34580993652344, 50.6252978589571, zoom = 3, options=list('maxZoom'=0)) 
    
    
  }) 
}


