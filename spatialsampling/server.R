################################################################################
#
# Server
#
################################################################################

##
function(input, output, session) {
  ## Map base layer
  output$map <- renderLeaflet({
    leaflet() %>%
      addMapboxTiles(style_id = northstar, username = "ernestguevarra") %>%
      setView(lng = 20, lat = 20, zoom = 3)
  })
}
