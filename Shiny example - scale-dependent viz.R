
# Script description ------------------------------------------------------

# Dummy example of polygons that appear and disappear based on the zoom level


# Packages ----------------------------------------------------------------
library(shiny)
library(leaflet)
library(dplyr)
library(sf)

# Data --------------------------------------------------------------------
# generate some made up polygons
poly <-
  sf::st_polygon(list(matrix(data = c(-2.4, 49.6,-2.4,49.8,-2.1,49.8,-2.1,49.6,-2.4,49.6), ncol = 2, byrow = T))) %>%
  sf::st_sfc(crs = 4326) %>%
  #add fields for the zoom to appear and disappear
  sf::st_sf(data.frame(appears_gte = 1, disappears_gte = 11), geometry = .)

poly_sml <-
  sf::st_polygon(list(matrix(data = c(-2.15,49.75,-2.15,49.76,-2.14,49.76,-2.14,49.75,-2.15, 49.75), ncol = 2, byrow = T))) %>%
  sf::st_sfc(crs = 4326) %>%
  sf::st_sf(data.frame(appears_gte = 10, disappears_gte = 25), geometry = .)

polys <- rbind(poly, poly_sml)


# UI ----------------------------------------------------------------------
ui <- fluidPage(
  leaflet::leafletOutput("scale_vis", width = "100%", height = '100vh')
)


# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  #set up basic leaflet map
  scale_vis <-
    leaflet(polys) %>%
    setView(lng = -2.25, lat = 49.75, zoom=8) %>%
    addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}')
  
  # filter polygons based on whether they should display based on map zoom level
  # n.b. input$MAPNAME_zoom gives you zoom level - in this case the map is called 'scale_vis'
  scale_polys <-
    shiny::reactive(
      polys[which(polys$appears_gte <= input$scale_vis_zoom &
                  polys$disappears_gte >= input$scale_vis_zoom),]
    )
  
  # observe the zoom level and add the relevant polygons, clearing the currently loaded ones
  shiny::observe({
    a <-
      leafletProxy('scale_vis', data = scale_polys()) %>%
      leaflet::addPolygons(
        color = 'cyan',
        opacity = 0.75, 
        weight = 2
      )
    
    })
  
  output$scale_vis <- leaflet::renderLeaflet(scale_vis)
}

shinyApp(ui = ui, server = server)
