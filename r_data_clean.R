library(dplyr)
library(sf)
library(ggplot2)
library(leaflet)
library(rmapshaper)
library(stringr)

setwd("~/Owen/R_git/Spatial-mapping-shiny-app")
poa_data <- st_read("geometries/POA_2021_AUST_GDA2020.shp") %>%
  filter(!st_is_empty(.)) 

state_data <- st_read("geometries/STE_2021_AUST_GDA2020.shp") %>%
  filter(!st_is_empty(.))

postcode1 <- st_intersection(poa_data, state_data) %>%
  mutate(Area = as.numeric(st_area(.))) %>%
  filter(Area > 100)

postcode.03 <- ms_simplify(postcode1, keep = 0.3)

postcode.state <- postcode.03 %>%
  mutate(State = str_replace_all(STE_NAME21, c("Australian Capital Territory" = "ACT",
                                                "New South Wales" = "NSW",
                                                "Northern Territory" = "NT",
                                                "Other Territories" = "OTHER",
                                                "Queensland" = "QLD",
                                                "South Australia" = "SA",
                                                "Tasmania" = "TAS",
                                                "Victoria"= "VIC",
                                                "Western Australia" = "WA")))

#leaflet() %>%
#  addPolygons(data = postcode.03, color = 'blue', popup = postcode.03$STE_NAME21)

saveRDS(postcode.state, "postcode_split.rds")

leaflet() %>%
  addProviderTiles(providers$OpenStreetMap, group = "Street map") %>%
  addPolygons(data = postcode1, color="blue", group = "postcode", popup = postcode1$POA_NAME21) %>%
  addPolygons(data = state_data, color = "green", group = "State")%>%
  addLayersControl(baseGroups = c("Street map"),
                   overlayGroups = c("postcode",'State'),
                   options = layersControlOptions(collapsed = FALSE))
  
  