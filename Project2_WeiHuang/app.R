
library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(rgdal)
library(leaflet)
library(shinythemes)

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

# pdf(NULL)
# 
# # Unique values for Resource Field
# ckanUniques <- function(id, field) {
#   url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
#   c(ckanSQL(URLencode(url)))
# }
# 
# #Select unique data from website and turn it to list
# types_date <- sort(ckanUniques("f61f6e8c-7b93-4df3-9935-4937899901c7", "general_ledger_date")$general_ledger_date) 
# types_amount <-sort(ckanUniques("f61f6e8c-7b93-4df3-9935-4937899901c7", "amount")$amount)

cost_rev_load <- read.csv("cost_rev.csv", header = TRUE)
cost_rev_load$ledger_descrpition <- NULL

cost_rev <- cost_rev_load %>% 
  filter(grepl("2018", general_ledger_date) )  %>%
  filter(grepl("GENERAL FUND", fund_description) |
           grepl("CDBG FUND", fund_description) |
           grepl("ARAD-PUBLIC WORKS", fund_description)) %>%
  mutate(types_amount = as.numeric(amount),
         department_name = as.character(department_name),
         date_2018 = as.Date(general_ledger_date, "%Y-%m-%d"),
         cost_center_description = as.character(cost_center_description),
         fund = as.character(fund_description))

# Define UI for application 
ui <- fluidPage(
  #Application title
  titlePanel("City Wide Revenues and Expenses"), 
  
  #Design sider bar
  sidebarLayout(
    sidebarPanel(
      # General ledger Date Select
      dateRangeInput("DateSelect",
                     "date",
                     start = Sys.Date()-30,
                     end = Sys.Date()),
      sliderInput("AmountSelect",
                  "amount:",
                  min = min(cost_rev$types_amount, na.rm = T),
                  max = max(cost_rev$types_amount, na.rm = T),
                  value = c(min(cost_rev$types_amount, na.rm = T), max(cost_rev$types_amount, na.rm = T)),
                  step = 50),
      actionButton("reset", "Reset Filters", icon = icon("refresh"))
    ),
    # Tabset Main Panel
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 plotlyOutput("barplot"),
                 plotlyOutput("boxplot"),
                 plotlyOutput("pointsplot")
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
  crInput <- reactive({
    cost_rev <- cost_rev %>%
      mutate(date = as.Date(general_ledger_date))
    #Build API Query with proper encodes
    
    # url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22f61f6e8c-7b93-4df3-9935-4937899901c7%22%20WHERE%20%22general_ledger_date%22%20%3E=%20%27",
    #               input$DateSelect[1],"%27%20AND%20%22general_ledger_date%22%20%3C=%20%27",input$DateSelect[2],
    #               "%27%20AND%20%22amount%22%20%3E%3D", input$AmountSelect[1], "%20AND%20%22amount%22%20%3C%3D", input$AmountSelect[2],"%20;")
    # 
    
    
    # #Load and clean data
    # cost_rev <- ckanSQL(url) %>%
    #   mutate(date = as.Date(general_ledger_date))
    
    
    return(cost_rev)
    
  })
  
  # Three bars are showing the number of the three chosen department
  output$barplot <- renderPlotly({
    cost_rev <- crInput()
    ggplotly(
      ggplot(data = cost_rev, aes(x = department_name, fill = as.factor(department_name))) + 
        geom_bar() +
        labs(x = "Department Names", title = "Barplot for Department Name") +
        guides(color = FALSE))
  })
  #Using box plots to show the distribution of the three chosen departments
  output$boxplot <- renderPlotly({
    cost_rev <- crInput()
    ggplotly(
      ggplot(data = cost_rev, aes(x = fund_description, y = as.numeric(amount))) + 
        geom_boxplot() +
        labs(x = " Fund Category", y = "Cost Amount", title = "Boxplot for Department Names and Cost Amount") +
        guides(color = FALSE))
  })
  
  # Using points plots to show the average amount of cost in each date
  
  output$pointsplot <- renderPlotly({
    cost_rev <- crInput()
    ggplotly(
      ggplot(data = cost_rev, aes(x = date, y = as.numeric(amount))) + 
        labs(x = "Dates for Geberal Ledgers", y = "Cost Amount", title = "Points Plot for Dates and Cost Amounts") +
        geom_point())
  })
  
  
  
  # Data Table
  output$table <- DT::renderDataTable({
    cost_rev <- crInput()
    
    subset(cost_rev, select = c(department_name, cost_center_description, general_ledger_date, amount))
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
      write.csv(crInput(), file)
    }
  )
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session, "DateSelect", selected = c("2018-10-12","2018-10-11","2018-10-09"))
    updateSliderInput(session, "AmountSelect", value = c(min(types_amount, na.rm = T), max(types_amount, na.rm = T)))
    showNotification("You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")



