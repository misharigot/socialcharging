# This scripts handles all information for the map plot
source("src/map/map_data.R")
# 
# # Id for ui.R
# mapId <- "map"
# 
# # This method handles the default content of the map like circles and legends.
# handleDefaultMapCreation <- function(userInput, mapData) {
#   radius <- mapData$total_charged / max(mapData$total_charged) * 300
#   pal <- colorBin("plasma", mapData$total_charged, 5, pretty = FALSE)
# 
#   leaflet() %>%
#     addTiles(
#       urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png"
#     ) %>%
#     setView(lng = 4.32, lat = 52.05, zoom = 12) %>%
#     addCircles(
#       lng = mapData$longitude,
#       lat = mapData$latitude,
#       radius = radius, stroke = FALSE,
#       fillOpacity = 0.8, color = "#03f",
#       layerId = which(mapData$longitude == mapData$longitude & mapData$latitude == mapData$latitude),
#       fillColor = pal(mapData$total_charged)) %>%
#     addLegend("bottomright",
#               pal = pal,
#               values = mapData$total_charged,
#               title = "Total Charged kWh",
#               layerId = "colorLegend"
#               )
# }
# 
# # This method handles the content of the map like circles and legends.
# handleMapCreation <- function(userInput, mapData) {
#   categorySelected <- userInput
#   if (!(categorySelected == "Users per station")) {
# 
#     if (categorySelected == "Charged kWh per station") {
#       radius <- mapData$total_charged / max(mapData$total_charged) * 300
#       pal <-  colorBin("plasma", mapData$total_charged, 5, pretty = FALSE)
#     }
# 
#     if (categorySelected == "Occupation percentage") {
#       radius <- mapData$popularity_score / max(mapData$popularity_score) * 300
#       pal <- colorBin("plasma", mapData$total_charged, 5, pretty = FALSE)
#     }
# 
#     if (categorySelected == "Efficiency percentage") {
#       radius <- mapData$efficiency_score / max(mapData$efficiency_score) * 300
#       pal <- colorBin("plasma", mapData$total_charged, 5, pretty = FALSE)
#     }
# 
#     leafletProxy(mapId, data = mapData) %>%
#       clearShapes() %>%
#       addCircles(
#         lng = mapData$longitude,
#         lat = mapData$latitude,
#         radius = radius, stroke = FALSE,
#         fillOpacity = 0.8, color = "#03f",
#         layerId = which(mapData$longitude == mapData$longitude & mapData$latitude == mapData$latitude),
#         fillColor = pal(mapData$total_charged)) %>%
#       addLegend("bottomright",
#                 pal = pal,
#                 values = mapData$total_charged,
#                 title = "Total Charged kWh",
#                 layerId = "colorLegend"
#                 )
#   } else {
#     radius <- mapData$total_users / max(mapData$total_users) * 300
# 
#     leafletProxy(mapId, data = mapData) %>%
#       clearShapes() %>%
#       addCircles(
#         lng = mapData$longitude,
#         lat = mapData$latitude,
#         radius = radius, stroke = FALSE,
#         fillOpacity = 0.7, color = "#03f",
#         layerId = which(mapData$longitude == mapData$longitude & mapData$latitude == mapData$latitude),
#         fillColor = "red")
#   }
# }
# 
# geom_text(stat = "count", aes(label = as.character(round((..count..) / sum(..count..) * 100), digits = 2), "%"),
#           position = position_stack(vjust = 0.5))
# 
# # This method handles the content of the popup when circles are clicked on the map.
# chargingStationPopup <- function(id, lat, lng, mapData) {
#   selectedChargingPole <- mapData[id, ]
#   content <- as.character(tagList(
#     tags$h4("Location: ", selectedChargingPole$address),
#     sprintf("Total charged kWh: %s", selectedChargingPole$total_charged), tags$br(),
#     sprintf("Total elapsed hours: %s", selectedChargingPole$total_hours_elapsed), tags$br(),
#     sprintf("Total effective hours: %s", selectedChargingPole$total_effective_charging), tags$br(),
#     sprintf("Station outlets: %s", selectedChargingPole$outlets), tags$br(),
#     sprintf("Total sessions: %s", selectedChargingPole$total_sessions), tags$br(),
#     sprintf("Total users: %s", selectedChargingPole$total_users)
#   ))
# 
#   leafletProxy(mapId) %>%
#     addPopups(lng, lat, content, layerId = id)
# }
# 
# # This method handles the popup event
# handlePopupCreation <- function(event, mapData) {
#   leafletProxy(mapId) %>% clearPopups()
#   if (is.null(event))
#     return()
#   isolate({
#     chargingStationPopup(event$id, event$lat, event$lng, mapData)
#   })
# }