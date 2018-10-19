#Project2

library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(rgdal)
library(leaflet)
library(shinythemes)
library(plotly)
library(ckanr)

# Set data.wprdc.org as the default URL.
ckanr_setup(url = "https://data.wprdc.org/") 
url <- get_default_url()

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

# Unique values for Resource Field
  ckanUniques <- function(id, field) {
    url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
    c(ckanSQL(URLencode(url)))
  }
 
 # Select unique data from website and turn it to list
 

 playG_name <- sort(ckanUniques("47350364-44a8-4d15-b6e0-5f79ddff9367", "name")$name)
 playG_maintenance <- sort(ckanUniques("47350364-44a8-4d15-b6e0-5f79ddff9367", "maintenance_responsibility")$maintenance)
 playG_ward <- sort(ckanUniques("47350364-44a8-4d15-b6e0-5f79ddff9367", "ward")$ward)
 interM_load <- readOGR("https://data.wprdc.org/dataset/31ce085b-87b9-4ffd-adbb-0a9f5b3cf3df/resource/f86f1950-3b73-46f9-8bd4-2991ea99d7c4/download/markingsimg.geojson")

 pal5 <- colorFactor(palette = c( "#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33"),domain = interM_load$type)

# Define UI for application 
ui <- fluidPage(
  #Application title
  titlePanel("Pittsburgh Playground and Intersection Markings"), 
  
  #Design sider bar
  sidebarLayout(
    sidebarPanel(
    
      selectInput("PlaygroundNameSelect",
                  "playground name:",
                  choices =  playG_name,
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = c("Deer Pit Playground","Arsenal Playground","Baxter Playground","Arlington Playground","Armstrong Playground","Brookline Playground","Dallas Playground")),
      
      selectInput("MaintenanceResponsSelect",
                  "Maintenance Responsibility:",
                  choices = playG_maintenance,
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = c("Parks - Eastern","Parks - Northeast","Parks - Northern","Parks - Southern")),
      
      sliderInput("WardAmountSelect",
                  "ward amount:",
                  min = min(as.numeric(playG_ward), na.rm = T),
                  max = max(as.numeric(playG_ward), na.rm = T),
                  value = c(min(as.numeric(playG_ward), na.rm = T), max(as.numeric(playG_ward), na.rm = T)),
                  step = 1),
      
      selectInput("TypeSelect",
                  "type:",
                  levels(interM_load$type),
                  selected = c("Stop Line", "Crosswalk - Two Lined"),
                  selectize = T,
                  multiple = T),
      
      actionButton("reset", "Reset Filters", icon = icon("refresh"))
    ),
    # Tabset Main Panel
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 plotlyOutput("boxplot"),
                 plotlyOutput("pointsplot")
        ),
        
        tabPanel("Map",
                 tags$style(type = "text/css", ".leaflet {height: calc(100vh - 150px) !important;} body {background-color:#b3cde0;}"),
                 leafletOutput("map")
                 ),
        
        tabPanel("Table",
                 fluidPage(
                   wellPanel(DT::dataTableOutput("table")),
                   wellPanel(downloadButton("downloadData", label = "Download chosen File"))
                 )
                )

        
      )
    )
  )
)

# Define server logic
server <- function(input, output, session = session) {
  #Build API Query for Playground
  PGInput <- reactive({
    #Taken data from API :https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%2247350364-44a8-4d15-b6e0-5f79ddff9367%22
    #Build an api filter for playground name
    PlaygroundName_filter <- ifelse(length(input$PlaygroundNameSelect) > 0, 
                              paste0(" AND name IN ('", paste0(input$PlaygroundNameSelect, collapse = "','"), "')"), "")
    
    #Build an api filter for Maintenance Responsibility
    Maintenance_filter <- ifelse(length(input$MaintenanceResponsSelect) > 0, 
                              paste0(" AND maintenance_responsibility IN ('", paste0(gsub(" " ," ", input$MaintenanceResponsSelect), collapse = "','"), "')"), "")

    url_1 = paste0('https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT * FROM "47350364-44a8-4d15-b6e0-5f79ddff9367" WHERE (CAST("ward" as integer) BETWEEN ', input$WardAmountSelect[1], ' AND ', input$WardAmountSelect[2], ")", PlaygroundName_filter, Maintenance_filter)
    
    url_1 <- gsub("'", "%27", url_1) # Replace ' with %27 
    url_1 <- gsub('"', "%22", url_1)
    url_1 <- gsub(" ", "%20", url_1) # Replace space with % 20
    
    print(url_1)
    
    playG <- ckanSQL(url_1) 
    
    return(playG)
  })
  
  #Build API Query for Intersection Markings
  IMInput <- reactive({
    interM <- interM_load
    if (length(input$TypeSelect > 0)) {
      interM <- subset(interM, type %in% input$TypeSelect)
    }
    return(interM)
  })
  
  
  # Output Map
  output$map <- renderLeaflet({
    interM <- interM_load
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
                 lat = ~latitude,
                 popup = ~paste0("<b>", neighborhood, "<b>", park, "</b>: ", name) ) %>%
      #Add lines for intersction markings in Pittsburgh
      addPolylines(data = sp2, color = ~ pal5(type)) %>%
      #Add legends for types of intersection markings
      addLegend(position = "bottomright" , pal = pal5, values = interM$type, title = "Type") %>%
      setView(-80, 40.45, 12)
   })

  
  # Creating points plot for name
  output$pointsplot <- renderPlotly({
    playG <- PGInput()
    ggplotly(
      ggplot(data = playG, aes(x = name, y = ward, fill = name, colour = name), show.legend = T) + 
        geom_point() +
        theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 0, vjust = 1, color = "#6497b1"),
              axis.text.y = element_text(face = "bold", angle = 45, hjust = 0, vjust = 1, color = "#6497b1")) +
        scale_fill_brewer(palette = "Set3") +
        labs(x = "Playground Names", title = "Pointsplot for Park Name and Ward Numbers") +
        guides(color = FALSE))
  })

  #Using box plots to show the distribution of the three chosen departments
  output$boxplot <- renderPlotly({
    playG <- PGInput()
    ggplotly(
      ggplot(data = playG, aes(x = maintenance_responsibility, y = ward, colour = maintenance_responsibility), show.legend = T) + 
        theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 0, vjust = 1, color = "#6497b1"),
              axis.text.y = element_text(face = "bold", angle = 45, hjust = 0, vjust = 1, color = "#6497b1")) +
        geom_boxplot() +
        scale_fill_brewer(palette = "Set3") +
        labs(x = "Maintenance Responbility", y = "Ward Amount", title = "Boxplot for Maintenance Responsibility and Ward") +
        guides(color = FALSE))
  })
  
  
  
  
  # Data Table
  output$table <- DT::renderDataTable({
    playG <- PGInput()
    
    subset(playG, select = c(name, maintenance_responsibility, ward))
    
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
      paste("playground-intersection-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(PGInput(), file)
    }
  )
  
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session, "PlaygroundNameSelect", selected = c("Arlington Playground"))
    updateSelectInput(session, "MaintenanceResponsSelect", selected = c("Parks - Eastern","Parks - Northeast","Parks - Northern","Parks - Southern"))
    updateSliderInput(session, "WardAmountSelect", value = c(min(ward, na.rm = T), max(ward, na.rm = T)))
    updateSelectInput(session, "TypeSelect", selected = c("Stop Line"))
    showNotification("You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")



