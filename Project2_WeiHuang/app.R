#Project2

library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(shinythemes)
library(plotly)

pal5 <- colorFactor(palette = c( "#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33"), 
                    domain = interM$type)


ckanSQL <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub('NaN', 'NA', c, perl = TRUE)
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}

pdf(NULL)
# 
# Unique values for Resource Field
  ckanUniques <- function(id, field) {
    url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
    c(ckanSQL(URLencode(url)))
  }
 
 # Select unique data from website and turn it to list
 

 playG_name <- sort(ckanUniques("47350364-44a8-4d15-b6e0-5f79ddff9367", "name")$name)
 playG_neighborhood <- sort(ckanUniques("47350364-44a8-4d15-b6e0-5f79ddff9367", "neighborhood")$neighborhood)
 playG_ward <- sort(ckanUniques("47350364-44a8-4d15-b6e0-5f79ddff9367", "ward")$ward)
 interM_policeZ <- sort(ckanUniques("f86f1950-3b73-46f9-8bd4-2991ea99d7c4", "police_zone")$police_zone)

 
 # interM <-  readOGR("https://data.wprdc.org/dataset/31ce085b-87b9-4ffd-adbb-0a9f5b3cf3df/resource/f86f1950-3b73-46f9-8bd4-2991ea99d7c4/download/markingsimg.geojson")
 # 
 
 # waterF_load <- read.csv("water_feature.csv", header = TRUE) 
 # waterF <- waterF_load %>%
 #   mutate(control_type = as.character(control_type),
 #          feater_type = as.character(feature_type))
 

# Define UI for application 
ui <- fluidPage(
  #Application title
  titlePanel("City Wide Revenues and Expenses"), 
  
  #Design sider bar
  sidebarLayout(
    sidebarPanel(
    
      selectInput("ParkNameSelect",
                  "park name:",
                  choices =  playG_name,
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = c("Arlington Playground","Bon Air Playground","Camp David Lawrence Playground")),
      
      selectInput("NeighborhoodSelect",
                  "neighborhood:",
                  choices = playG_neighborhood,
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = c("South Side Slopes","Brookline","Strip District")),
      
      sliderInput("WardAmountSelect",
                  "ward amount:",
                  min = min(as.numeric(playG_ward), na.rm = T),
                  max = max(as.numeric(playG_ward), na.rm = T),
                  value = c(min(as.numeric(playG_ward), na.rm = T), max(as.numeric(playG_ward), na.rm = T)),
                  step = 1),
      
      checkboxGroupInput("PoliceZoneSelect",
                         "police zone:",
                         choices = interM_policeZ,
                         selected = c("3", "4")),
      
      actionButton("reset", "Reset Filters", icon = icon("refresh"))
    ),
    # Tabset Main Panel
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 plotlyOutput("barplot"),
                 plotlyOutput("boxplot"),
                 plotlyOutput("pointsplot"),
                 leafletOutput("map")
        ),
        tabPanel("Table",
                 DT::dataTableOutput("table"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session = session) {
  # Filtered cost and revenue data
  PGInput <- reactive({
    playG <- playG
    
    #Build API Query with proper encodes
    

    
    
    #Load and clean data
     # playG <- ckanSQL(url) %>%
     #   mutate(date = as.Date(general_ledger_date))
    
    #Build API Query with proper encodes
    #Load and clean data
    url_1 <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%2247350364-44a8-4d15-b6e0-5f79ddff9367%22") #https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%2247350364-44a8-4d15-b6e0-5f79ddff9367%22
    ###
    playG <- ckanSQL(url_1) 
    
    return(playG)
  })
  
  IMInput <- reactive({
    
    url_2<- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22f86f1950-3b73-46f9-8bd4-2991ea99d7c4%22")

    interM <- ckanSQL(url_2) 
    
    return(interM)
  })
  
  # Output Map
  output$map <- renderLeaflet({
    sp1 <- PGInput()
    sp2 <- IMInput()
    
    #Call Data and Build Map
    leaflet() %>%
      #create basemaps, the OpenStreet Map and the BalckAndWhite Map, and then create bottoms for choosing between two maps
      addTiles(group = "OpenStreetMap.Mapnik(default)") %>%
      addProviderTiles("OpenStreetMap.BlackAndWhite", group = "BW") %>%
      addLayersControl(baseGroups = c("OpenStreetMap.Mapnik(default)", "BW"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      #Add polygons for Pttsburgh playgrounds
      addMarkers(data = sp1,
                 lng = ~longitude,
                 lat = ~latitude) %>%
      #Add lines for intersction markings in Pittsburgh
      addPolylines(data = sp2, color = ~ pal5(type)) %>%
      #Add legends for types of intersection markings
      addLegend(position = "bottomright" , pal = pal5, values = interM$type, title = "Type") %>%
      
      #Add points depicting water features in Pittsburgh
      #addMarkers(data = sp2) %>%
      #Add legends for types of intersection markings
      # addLegend(position = "bottomright" , pal = pal5, values = interM.load$type, title = "Type") %>%
      setView(-80, 40.45, 12)
   })

  
  
  # Three bars are showing the number of the three chosen parks
  output$barplot <- renderPlotly({
    playG <- PGInput()
    ggplotly(
      ggplot(data = playG, aes(x = name, fill = as.factor(name))) + 
        geom_bar() +
        labs(x = "Park Names", title = "Barplot for Park Name") +
        guides(color = FALSE))
  })
  
  #Using box plots to show the distribution of the three chosen departments
  output$boxplot <- renderPlotly({
    playG <- PGInput()
    ggplotly(
      ggplot(data = playG, aes(x = neighborhood, y = as.numeric(ward))) + 
        geom_boxplot() +
        labs(x = " Neighborhood", y = "Ward Amount", title = "Boxplot for Neighborhood and Ward") +
        guides(color = FALSE))
  })
  
  # Using points plots to show the average amount of cost in each date
  
  output$pointsplot <- renderPlotly({
    playG <- PGInput()
    ggplotly(
      ggplot(data = playG, aes(x = maintenance_responsibility, y = as.numeric(ward))) + 
        labs(x = "Maintenance Responsibility", y = "Ward Amount", title = "Points Plot for Maintenance Responsibility and Ward Amounts") +
        geom_point())
  })
  
  
  
  # Data Table
  output$table <- DT::renderDataTable({
    playG <- PGInput()
    
    subset(playG, select = c(name, neighborhood, ward))
    subset(waterF, select = c(control_type))
  })
  # Updating the URL Bar
  observe({
    print(reactiveValuesToList(input))
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("cost-revenue-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(PGInput(), file)
    }
  )
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session, "ParkNameSelect", selected = c("Arlington Playground","Bon Air Playground","Camp David Lawrence Playground"))
    updateSelectInput(session, "NeighborhoodSelect", selected = c("South Side Slopes","Brighton Heights"))
    updateSliderInput(session, "WardAmountSelect", value = c(min(ward, na.rm = T), max(ward, na.rm = T)))
    updateCheckboxGroupInput(session, "ControlTypeSelect", selected = "ON/OFF")
    showNotification("You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")



