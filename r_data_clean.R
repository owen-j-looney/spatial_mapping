library(dplyr)
library(sf)
library(ggplot2)
library(leaflet)

setwd("~/Owen/R_git/Spatial-mapping-shiny-app")
poa_data <- st_read("geometries/POA_2021_AUST_GDA2020.shp") %>%
  filter(!st_is_empty(.)) 

state_data <- st_read("geometries/STE_2021_AUST_GDA2020.shp") %>%
  filter(!st_is_empty(.))

postcode1 <- st_intersection(poa_data, state_data) %>%
  mutate(Area = as.numeric(st_area(.))) %>%
  filter(Area > 100)

saveRDS(postcode1, "postcode_split.rds")

leaflet() %>%
  addProviderTiles(providers$OpenStreetMap, group = "Street map") %>%
  addPolygons(data = postcode1, color="blue", group = "postcode", popup = postcode1$POA_NAME21) %>%
  addPolygons(data = state_data, color = "green", group = "State")%>%
  addLayersControl(baseGroups = c("Street map"),
                   overlayGroups = c("postcode",'State'),
                   options = layersControlOptions(collapsed = FALSE))
  
  