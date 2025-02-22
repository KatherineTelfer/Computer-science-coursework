library(bslib)
#basically CSS for shiny
library('shiny')
#makes web applications
library(leaflet)
library(bslib)

ui <- page_navbar(
  title = "Application to determine how temperature change impacts bird migration patterns",
  bg = "#0062cc",
  underline = TRUE,
  nav_panel(title = "Welcome page!!", 
            #title of page
            p("This is my computer science coursework dedicated to creating an application that can analyse the correlation between temperature and the migration patterns of a large variety of bird species"),
            #text
            tags$b("please click the main page content at the top to access my project"),
            #bold
            tags$img(src = "world-map.jpg"),
            #image of a map
            ),
  nav_panel(title = "Main page content", p("include map and bird migration tracker shit")),
  nav_panel(title = "evil tab", p("no data here ")),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
  )
)


server <- function(input, output) {
}
shinyApp(ui = ui, server = server)

