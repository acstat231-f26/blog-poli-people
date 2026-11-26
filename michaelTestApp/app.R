library(shiny)
library(bslib)
library(tidyverse)
library(sf)
library(viridis)

setwd("~/Documents/GitHub/blog-poli-people")
world_map <- readRDS("data/world_map.rds")
final_dataset_map <- readRDS("data/final_dataset_map.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(
              inputId = "map_metric",
              label = "Pick a metric to view:",
              choices = c(
                "Exports",
                "Imports",
                "Population",
                "Democracy Index"
              )
              ),
              sliderInput(
                inputId = "year",
                label = "Choose Map Year to View:",
                min = 2010,
                max = 2024,
                value = 2024, 
                step = 1,
                sep = ""
              )
          ),
        mainPanel(
          plotOutput("output_map")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$output_map <- renderPlot({
    
    if (input$map_metric == "Democracy Index") {
      col_name <- as.character(input$year)
    } else {
      col_name <- paste0(input$year, "_", input$map_metric)
    }
    
    ggplot(final_dataset_map) +
      geom_sf(aes(fill = .data[[col_name]]), linewidth = 0.1) +
      scale_fill_viridis(option = "magma", direction = -1, na.value = "grey80") +
      labs(
        title = paste(input$map_metric, "-", input$year),
        fill = NULL
      ) +
      theme_void()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
