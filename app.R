library(shiny)
library(leaflet)
library(leaflegend)
library(crosstalk)
library(dplyr)
library(stringr)
library(readxl)
library(glue)
library(sf)
library(DT)
library(shinydashboard)
library(shinyWidgets)
library(htmlwidgets)
library(waiter)
library(purrr)
library(httr)
library(rsconnect)
library(tableHTML)


setwd("~/Owen/R_git/spatial-mapping")
postcode_data <- st_read("geometries/POSTCODE_2016_STATE.shp") %>%
  filter(!st_is_empty(.)) 
# %>%  as("Spatial")


Commonwelath_electorate_data <- st_read("geometries/CED_2016.shp") %>%  
  filter(!st_is_empty(.))

LGA_data <- st_read("geometries/LGA_2021_AUST_GDA2020.shp") %>%  
  filter(!st_is_empty(.))





ui <- bootstrapPage(useWaiter(),
                    autoWaiter(),
                    use_waitress(),
                    list(tags$head(tags$style(HTML("
                                                   .navbar.navbar-default.navbar-static-top{ color: #fff; 
                                                   font-size: 20px; 
                                                   background-color: #fff ;}
                                                   .navbar-default .navbar-brand { color: #2F3146; 
                                                   font-size: 26px;
                                                   font-family: 'Lato',verdana;
                                                   background-color: #fff ;} 
                                                   #sidebar {background-color: transparent;
                                                   }
                                                   .navbar-default .navbar-nav>li>a:focus, .navbar-default .navbar-nav>li>a:hover {
                                                   color: #FFFFFF;
                                                   background-color: #ED145B;
                                                   }
                                                   .navbar-nav > .active > a {
                                                   color: #fff !important;
                                                   background-color:#21BCBE !important;
                                                   }
                                                   
                                                   
                                                   ")))),
                    setBackgroundImage(
                      src = "images/background1.jpg"), #https://www.yourtown.com.au/sites/all/themes/yourtown/imgs/bg-markers.svg"),
                    navbarPage(
                      windowTitle = "Owen Looney - Mapping example",
                      title = div(img(src = 'images/blue-shiny-background.svg', # https://yourtown.com.au/sites/all/themes/yourtown/yourtown-logo.svg',
                                      style = "margin-top: -14px;
                                      padding-right:10px;
                                      padding-bottom:5px",
                                      height = 55)),
                      fluid = T,
                      tabPanel("Home",
                               sidebarLayout(
                                 sidebarPanel(id = "sidebar",
                                              width = 3,
                                              selectInput("School_Type", 
                                                          label = "School type:",
                                                          choices = NULL, 
                                                          #selected = c("SPECIAL"), 
                                                          multiple = TRUE),
                                              selectInput("State", 
                                                          label = "State:", 
                                                          choices = unique(postcode_data$State), 
                                                          selected = "QLD",
                                                          multiple = TRUE),
                                              selectizeInput("Postcode",
                                                             label = "Postcode", 
                                                             choices = NULL, 
                                                             multiple = TRUE),
                                              setSliderColor(c("#EB008B"),1),
                                              sliderInput("ICSEA_score",
                                                          label = "2020 ICSEA Percentile:",
                                                          min = 0,
                                                          max = 100,
                                                          value = c(0,100)),
                                              actionButton("data_refresh", "Click to load Map",
                                                           style="color: #fff; background-color: #21BCBE",
                                                           width = '100%'),
                                              tags$style(make_css(list('.well', 'border-width', '0px')))
                                 ),
                                 mainPanel(width = 9 ,
                                           tabsetPanel(
                                             tabPanel("Map",leafletOutput("map", height = "800px")),
                                             tabPanel("Data",dataTableOutput("datatable", height = "800px"))
                                           )))),
                      tabPanel("About",
                               p("About this dashboard", style = "font-size: 30px; color: black"),
                               p("This dashboard has been created as a proof of concept.", style = "font-size:15px; color: black"),
                               hr(),
                               p("How to use this dashboard", style = "font-size:30px; color: black"),
                               p("The filters on the side of the Home tab.", style = "font-size:15px; color: black"),
                               hr(),
                               p("Data information", style = "font-size:30px; color: black"),
                               p("The ABS was also used as a data source for the geometries of postcodes and federal electorates, and this data was gathered from ", a("here", 
                                                                                                                                                                       href = "https://www.abs.gov.au/statistics/standards/
                                                                                                                                                                       australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/
                                                                                                                                                                       access-and-downloads/digital-boundary-files",
                                                                                                                                                                       target = "_blank", style = "font-size:15px; color:blue"),style = "font-size:15px; color: black")
                               )))




server <- function(input,output,session) {
  
  updateSelectizeInput(session,'Postcode', choices = unique(postcode_data$Postcode), server = TRUE)
  
  

  postcode_data_react <- reactive({
    postcode_filtered <- postcode_data
    if(!is.null(input$Postcode)) {
      postcode_filtered <- postcode_filtered %>% filter(Postcode %in% input$Postcode)
    }
    if(!is.null(input$State)) {
      postcode_filtered <- postcode_filtered %>% filter(State %in% input$State)
    }
    postcode_filtered
  })
  
  electorate_data_react <- reactive({
    electorate_filtered <- Commonwelath_electorate_data
    if(!is.null(input$State)) {
      electorate_filtered <- electorate_filtered %>% filter(State %in% input$State)
    }
    #if(!is.null(input$Postcode)) {
    #  electorate_filtered <- electorate_filtered %>% st_intersection(postcode_data[postcode_data$Postcode %in% input$Postcode,])
    #}
    electorate_filtered
  })
  
  
  
  observeEvent(input$data_refresh, {
    
    updateActionButton(inputId = "data_refresh", label = "Click to Refresh data")
  })
  
  
  observeEvent(input$State, {
    if (!is.null(input$State)) {
      postcode_data <- postcode_data_react() %>% filter(State %in% input$State)
      
      updateSelectizeInput(session = session, inputId = "Postcode", label = "Postcode:",
                           choices = as.character(postcode_data[order(postcode_data$Postcode),]$Postcode),
                           server = TRUE,
                           selected = input$Postcode)}
    
  }, ignoreInit = FALSE)
  
  
  
  
  output$map <- renderLeaflet({

    postcode_react <- isolate(postcode_data_react())
    electorate_react <- isolate(electorate_data_react())
    
    
    leaflet() %>%
      addProviderTiles("Stamen.TonerLite", group = "Base") %>%
      addProviderTiles(providers$OpenStreetMap, group = "Street map") %>%
      addPolygons(data = postcode_react,
                  fillColor = "#383C57",
                  color= "#383C57",
                  group = "Postcode",
                  weight = 2,
                  stroke = TRUE,
                  fillOpacity = 0.05,
                  popup = paste("Postcode:",postcode_react$Postcode)) %>%
      addPolygons(data = electorate_react,
                  fillColor = NA,
                  color= "blue",
                  weight = 2,
                  group = "Commonwealth Electorate",
                  fillOpacity = 0.25,
                  popup = paste("Electorate:",electorate_react$Electorate)) %>%
      addLayersControl(baseGroups = c("Street map","Base"),
                       overlayGroups = c("Postcode", "Commonwealth Electorate"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Postcode","Commonwealth Electorate"))
  })
  
  observeEvent(input$data_refresh,{
    
    postcode_react <- isolate(postcode_data_react())
    electorate_react <- isolate(electorate_data_react())
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      removeControl("legend") %>%
      #clearShapes() %>%
      clearGroup("Postcode") %>%
      clearGroup("Commonwealth Electorate") %>%
      addPolygons(data = postcode_react,
                  fillColor = "#383C57",
                  color= "#383C57",
                  group = "Postcode",
                  weight = 2,
                  stroke = TRUE,
                  fillOpacity = 0.3,
                  popup = paste("Postcode:",postcode_react$Postcode)) %>%
      addPolygons(data = electorate_react,
                  fillColor = NA,
                  color= "blue",
                  weight = 2,
                  group = "Commonwealth Electorate",
                  fillOpacity = 0.25,
                  popup = paste("Electorate:",electorate_react$Electorate))# %>%
      
    
  }
  )
  
  
  output$datatable <- renderDT(server = FALSE, {
    input$data_refresh
    
    postcode_react <- isolate(postcode_data_react()) %>%
      st_drop_geometry() 
    
    tab1 <-   DT::datatable(postcode_react,
                            filter = "top",  # allows filtering on each column
                            extensions = c("Buttons",  # add download buttons, etc
                                           "Scroller"  # for scrolling down the rows rather than pagination
                            ),
                            fillContainer = TRUE,
                            rownames = FALSE,  # remove rownames
                            style = "bootstrap",
                            class = c("display","cell-border"),
                            width = "100%",
                            height = "100%",
                            options = list(
                              dom = "Blrtip",  # specify content (search box, etc)
                              deferRender = TRUE,
                              scrollY = 100,
                              #scroller = TRUE,
                              columnDefs = list(
                                list(
                                  visible = TRUE,
                                  targets = c(0:1)
                                )),
                              
                              buttons = list(
                                list(
                                  extend = "colvis",
                                  text = "Select columns"),# turn columns on and off
                                list(
                                  extend = "csv",  # download as .csv
                                  text = "Save as CSV file",
                                  exportOptions = list(columns = ":visible",
                                                       modifier = list(page = "all")
                                  )
                                ),
                                list(
                                  extend = "excel",
                                  text = "Save as Excel file",
                                  exportOptions = list(columns = ":visible",
                                                       modifier = list(page = "all")
                                  ))# download as .xlsx
                              ),
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': 'transparent','color': 'black'});",
                                "$(this.api().table().body()).css({'background-color': 'transparent','color': 'black'});",
                                "}")
                            )) %>%
      formatStyle(columns = colnames(postcode_react),backgroundColor = 'white', color = "black")
  })
}



observeEvent(input$data_refresh,
             if(input$data_refresh) {
               postcode_data_react <- reactive({
                 postcode_filtered <- postcode_data
                 if(!is.null(input$Postcode)) {
                   postcode_filtered <- postcode_filtered %>% filter(Postcode %in% input$Postcode)
                 }
                 if(!is.null(input$State)) {
                   postcode_filtered <- postcode_filtered %>% filter(State %in% input$State)
                 }
                 postcode_filtered
               })
             })

observeEvent(input$data_refresh,
             if(input$data_refresh) {
               electorate_data_react <- reactive({
                 electorate_filtered <- Commonwelath_electorate_data
                 if(!is.null(input$State)) {
                   electorate_filtered <- electorate_filtered %>% filter(State %in% input$State)
                 }
                 #if(!is.null(input$Postcode)) {
                 #  electorate_filtered <- electorate_filtered %>% st_intersection(postcode_data[postcode_data$Postcode %in% input$Postcode,]) %>% st_collection_extract(type= "POLYGON") %>% st_cast(to = "MULTIPOLYGON")
                 #}
                 electorate_filtered
               })
             })

shinyApp(ui, server) 