library(shiny)
library(ggplot2)
library(plotly)

source("R/ggplotly_helpers.R")
source("R/ggplotly_geoms.R")
source("R/ggplotly_mapping_module.R")
source("R/ggplotly_module.R")
source("R/ggplotly_theme.R")
source("R/helpers.R")

ui <- shiny::navbarPage("Visualise",
  
        shiny::tabPanel("ggplotly", ggplotlyModuleOutput("ggplotly"))
  
  
)

server <- function(input, output, session) {
  
  rf_data <- reactive({
    d <- datasets::mtcars
    d$car <- rownames(d)
    d$cyl <- as.factor(d$cyl)
    d
  })
  
  
  callModule(ggplotlyModule, "ggplotly", rf_data = rf_data)
  
}

shinyApp(ui = ui, server = server)