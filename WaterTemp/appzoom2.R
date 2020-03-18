library(ggplot2)
library(dplyr)
library(lubridate)
library(shiny)

setwd("C:/Users/cpien/OneDrive - California Department of Water Resources/Work/ClimateChange/R_code/climatechange/")
temp_H <- readRDS("WaterTemp/data/Temp_all_H.rds")

temp_small <- temp_H %>%
  filter(station == "BAC") %>%
  filter(year== "2017")


ui <- fluidPage(
  fluidRow(
    column(width = 4, class = "well",
           h4("Brush and double-click to zoom"),
           plotOutput("plot1", height = 300,
                      dblclick = "plot1_dblclick",
                      brush = brushOpts(
                        id = "plot1_brush",
                        resetOnNew = TRUE
                      )
           )
    ),
    
      
             )
           )

server <- function(input, output) {
# -------------------------------------------------------------------
# Single zoomable plot (on left)
ranges <- reactiveValues(x = NULL, y = NULL)

output$plot1 <- renderPlot({
  
  if (!is.null(ranges$x)) {
    ranges$x <- as.POSIXct(ranges$x, origin = "1970-01-01")
  }
  
  
  ggplot(temp_small, aes(datetime, Temp)) +
    geom_point() +
    coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
})

# When a double-click happens, check if there's a brush on the plot.
# If so, zoom to the brush bounds; if not, reset the zoom.
observeEvent(input$plot1_dblclick, {
  brush <- input$plot1_brush
  if (!is.null(brush)) {
    ranges$x <- c(brush$xmin, brush$xmax)
    ranges$y <- c(brush$ymin, brush$ymax)
    
  } else {
    ranges$x <- NULL
    ranges$y <- NULL
  }
})


}

shinyApp(ui, server)
