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

# Function to pull ESRI data
getEsri <- function(url) {
  # Make Call
  g <- GET(URLencode(url))
  c <- content(g)
  readOGR(c)
}

getEsriList <- function(url) {
  # Make Call
  g <- GET(URLencode(url))
  fromJSON(content(g))$features %>% 
    unlist() %>% 
    unname()
}

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
#load the polygons, lines and points for the map
# url_1 <- URLencode("https%3A%2F%2Fdata.wprdc.org%2Fdataset%2F37e7a776-c98b-4e08-ad61-a8c8e23ec9ab%2Fresource%2F12d59d62-e86d-4f37-af19-463050496ed6%2Fdownload%2Fplaygrounds_img.geojson")
# cities <- getEsriList(url_1)


# pdf(NULL)
# 
 # Unique values for Resource Field
 # ckanUniques <- function(id, field) {
 #   url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
 #   c(ckanSQL(URLencode(url)))
 # }
 
 #Select unique data from website and turn it to list
 
 # playG_ <- sort(ckanUniques("47350364-44a8-4d15-b6e0-5f79ddff9367", "latitude")$latitude)

 
### method for cleaning the dataset
 playG_load <- read.csv("playground.csv", header = TRUE)
 playG_load$type <- NA
 
 playG <- playG_load %>% 
   mutate(name = as.character(name),
          maintenance = as.character(maintenance_responsibility),
          park = as.character(park),
          neighborhood = as.character(neighborhood),
          ward = as.numeric(ward),
          latitude = as.numeric(latitude),
          longitude = as.numeric(longitude),
          publicWD = as.numeric(public_works_division))
 waterF_load <- read.csv("water_feature.csv", header = TRUE) 
 waterF <- waterF_load %>%
   mutate(control_type = as.character(control_type),
          feater_type = as.character(feature_type))

# Define UI for application 
ui <- fluidPage(
  #Application title
  titlePanel("City Wide Revenues and Expenses"), 
  
  #Design sider bar
  sidebarLayout(
    sidebarPanel(
      # General ledger Date Select
      # dateRangeInput("DateSelect",
      #                "date",
      #                start = Sys.Date()-30,
      #                end = Sys.Date()),
      selectInput("ParkNameSelect",
                  "park name:",
                  choices = sort(unique(playG$name)),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = c("Arlington Playground","Bon Air Playground","Camp David Lawrence Playground")),
      
      selectInput("NeighborhoodSelect",
                  "neighborhood:",
                  choices = sort(unique(playG$neighborhood)),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = c("South Side Slopes","Brookline","Strip District")),
      
      sliderInput("WardAmountSelect",
                  "ward amount:",
                  min = min(playG$ward, na.rm = T),
                  max = max(playG$ward, na.rm = T),
                  value = c(min(playG$ward, na.rm = T), max(playG$ward, na.rm = T)),
                  step = 1),
      
      checkboxGroupInput("ControlTypeSelect",
                         "control type:",
                         choices = sort(unique(waterF$control_type)),
                         selected = c("ON/OFF", "Continuous")),
      
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
    
    #  url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22f61f6e8c-7b93-4df3-9935-4937899901c7%22%20WHERE%20%22general_ledger_date%22%20%3E=%20%27",
    #                input$DateSelect[1],"%27%20AND%20%22general_ledger_date%22%20%3C=%20%27",input$DateSelect[2],
    #                "%27%20AND%20%22amount%22%20%3E%3D", input$AmountSelect[1], "%20AND%20%22amount%22%20%3C%3D", input$AmountSelect[2],"%20;")
    # # 
    
    
    # #Load and clean data
     # playG <- ckanSQL(url) %>%
     #   mutate(date = as.Date(general_ledger_date))
    
    
    return(playG)
    
  })
  WFInput <- reactive({
    waterF <- waterF
    
    return(waterF)
  })
  
  # Output Map
  output$map < renderLeaflet({
    sp1 <- PGInput()
    sp2 <- WFInput()
    #Call Data and Build Map
    leaflet() %>%
      #Increat basemaps, the OpenStreet Map and the BalckAndWhite Map, and then create bottoms for choosing between two maps
      addTiles(group = "OpenStreetMap.Mapnik(default)") %>%
      addProviderTiles("OpenStreetMap.BlackAndWhite", group = "BW") %>%
      addLayersControl(baseGroups = c("OpenStreetMap.Mapnik(default)", "BW"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      #Add polygons for Pttsburgh playgrounds
      addPolygons(data = sp1, color = "#8DD3C7") %>%
      #Add points depicting water features in Pittsburgh
      addMarkers(data = sp2) %>%
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
      ggplot(data = playG, aes(x = maintenance, y = as.numeric(ward))) + 
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



