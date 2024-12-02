# Load necessary libraries
library(shiny)
library(maps)
library(ggplot2)
library(readxl)
library(countrycode)
library(dplyr)
library(stringr)
library(rnaturalearth)
library(sf)

# Define UI
ui <- fluidPage(
  titlePanel("Geographical Data Mapper"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Data File",
                accept = c(".csv", ".xlsx", ".txt")),
      uiOutput("geoVarSelect"),
      uiOutput("valueVarSelect")
    ),
    mainPanel(
      plotOutput("mapPlot")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive expression to read the uploaded data
  dataInput <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    switch(ext,
           csv = read.csv(input$file$datapath, stringsAsFactors = FALSE),
           xlsx = readxl::read_excel(input$file$datapath),
           txt = read.table(input$file$datapath, header = TRUE, stringsAsFactors = FALSE),
           stop("Invalid file type")
    )
  })
  
  # Reactive expression to automatically detect geographic variables
  geoVars <- reactive({
    data <- dataInput()
    geo_columns <- c()
    state_names <- tolower(state.name)
    state_abbs <- tolower(state.abb)
    country_names <- tolower(countrycode::codelist$country.name.en)
    for (col in names(data)) {
      unique_values <- unique(tolower(data[[col]]))
      unique_values <- unique_values[!is.na(unique_values)]
      # Check if values match US states
      if (all(unique_values %in% state_names) || all(unique_values %in% state_abbs)) {
        geo_columns <- c(geo_columns, col)
      }
      # Check if values match countries
      else if (all(unique_values %in% country_names)) {
        geo_columns <- c(geo_columns, col)
      }
      # Check if column names contain geographic keywords
      else if (grepl("state|country|county|region", tolower(col))) {
        geo_columns <- c(geo_columns, col)
      }
    }
    geo_columns
  })
  
  # Reactive expression to determine geographic type
  geoType <- reactive({
    data <- dataInput()
    geoVar <- input$geoVar
    if (is.null(geoVar)) return(NULL)
    unique_values <- unique(tolower(data[[geoVar]]))
    unique_values <- unique_values[!is.na(unique_values)]
    state_names <- tolower(state.name)
    state_abbs <- tolower(state.abb)
    country_names <- tolower(countrycode::codelist$country.name.en)
    if (all(unique_values %in% state_names) || all(unique_values %in% state_abbs)) {
      return("state")
    } else if (all(unique_values %in% country_names)) {
      return("country")
    } else {
      return(NULL)
    }
  })
  
  # Render UI for selecting geographic variable
  output$geoVarSelect <- renderUI({
    req(geoVars())
    selectInput("geoVar", "Select Geographic Variable:", choices = geoVars())
  })
  
  # Render UI for selecting variable to plot
  output$valueVarSelect <- renderUI({
    req(dataInput())
    selectInput("valueVar", "Select Variable to Plot:", choices = names(dataInput()))
  })
  
  # Render the map plot
  output$mapPlot <- renderPlot({
    req(input$geoVar, input$valueVar)
    data <- dataInput()
    geoVar <- input$geoVar
    valueVar <- input$valueVar
    
    geo_type <- geoType()
    if (is.null(geo_type)) {
      showNotification("Unable to determine the geographic type of the selected variable", type = "error")
      return(NULL)
    }
    
    if (geo_type == "state") {
      # Prepare data for US states
      data[[geoVar]] <- tolower(data[[geoVar]])
      state_names <- tolower(state.name)
      state_abbs <- tolower(state.abb)
      data[[geoVar]] <- ifelse(data[[geoVar]] %in% state_abbs,
                               state_names[match(data[[geoVar]], state_abbs)],
                               data[[geoVar]])
      states_map <- map_data("state")
      states_map$region <- tolower(states_map$region)
      map_data <- left_join(states_map, data, by = c("region" = geoVar))
      ggplot(map_data, aes(long, lat, group = group, fill = .data[[valueVar]])) +
        geom_polygon(color = "black") +
        coord_fixed(1.3) +
        labs(fill = valueVar) +
        scale_fill_continuous(na.value = "grey90")
      
    } else if (geo_type == "country") {
      # Prepare data for countries
      data[[geoVar]] <- tolower(data[[geoVar]])
      world <- ne_countries(scale = "medium", returnclass = "sf")
      world$country <- tolower(world$name_long)
      map_data <- left_join(world, data, by = c("country" = geoVar))
      ggplot(map_data) +
        geom_sf(aes(fill = .data[[valueVar]])) +
        labs(fill = valueVar) +
        scale_fill_continuous(na.value = "grey90")
      
    } else {
      showNotification("Unable to plot the data for the selected geographic variable", type = "error")
      return(NULL)
    }
    
  })
  
}

# Run the application 
shinyApp(ui, server)
