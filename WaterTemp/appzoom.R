library(ggplot2)
library(dplyr)
library(lubridate)
library(shiny)
library(plotly)

# Read in file --------------------------------------------------------------
setwd("C:/Users/cpien/OneDrive - California Department of Water Resources/Work/ClimateChange/R_code/climatechange/")
temp_H <- readRDS("WaterTemp/data/Temp_all_H.rds")

temp_small <- temp_H %>%
  filter(station == "BAC") %>%
  filter(year== "2017")

ui <- fluidPage(
  selectInput("var_y", "Y-Axis", choices = names(temp_small)),
  


  fluidRow(
    column(width = 4, class = "well",
           actionButton("submit", "Submit"),
           sliderInput("temprange",
                       "Temperature Cutoffs:",
                       min = -10, max = 100,value = c(0,40)),
           h4("Brush and double-click to zoom"),
    ),
    column(width = 4, class = "well",
           plotOutput("plotX", height = 300,
                      click = "plot1_click",
                      dblclick = "plot1_dblclick",
                      brush = brushOpts(
                        id = "plot1_brush",
                        resetOnNew = TRUE
                      )
           )
    ),
    uiOutput("dynamic"),
    
    
    column(width = 2, class = "well",
           h4("Left plot controls right plot"),
           fluidRow(
             column(width = 4,
                    plotOutput("plot2", height = 300,
                               brush = brushOpts(
                                 id = "plot2_brush",
                                 resetOnNew = FALSE
                               )
                    )
             ),
             column(width = 2,
                    plotOutput("plot3", height = 300)
             )
           )
    )
    
  )
)

server <- function(input, output) {
  
  # -------------------------------------------------------------------
  # Single zoomable plot (on left)
  ranges <- reactiveValues(x = NULL, y = NULL)

  
  output$plotX <- renderPlot({
    input$submit
    
    
    temp_q4 <- temp_small %>%
      mutate(Flag_QC4 = ifelse(Temp<isolate(input$temprange[1]) | Temp>isolate(input$temprange[2]), "Y", "N"))
    
    # # Create a dataframe of all the "delete" rows
    temp_q4_b <- temp_q4 %>%
      filter(Flag_QC4 == "Y")
    
    
    
    if (!is.null(ranges$x)) {
      ranges$x <- as.POSIXct(ranges$x, origin = "1970-01-01")
    }
      req(input$var_y)
  ggplot(temp_q4, aes(x = datetime, y = Temp)) + geom_point() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE)
  
    
    # ggplot() + 
    #   geom_point(data=temp_q4, aes(datetime, Temp)) +
    #   geom_point(data=temp_q4_b, aes(datetime, Temp), color = "goldenrod2", size = 2) +
    #   coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE)
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
  
  ### MOre
  output$dynamic <- renderUI({
    req(input$plot1_click) 
    verbatimTextOutput("vals")
  })
  
  output$vals <- renderPrint({
    hover <- input$plot1_click 
    # print(str(hover)) # list
    y <- nearPoints(temp_small, input$plot1_click)[input$var_y]
    req(nrow(y) != 0)
    y
  })

  # # -------------------------------------------------------------------
  # # Linked plots (middle and right)
  # ranges2 <- reactiveValues(x = NULL, y = NULL)
  # 
  # 
  # output$plot2 <- renderPlot({
  #     if (!is.null(ranges2$x)) {
  #     ranges2$x <- as.POSIXct(ranges2$x, origin = "1970-01-01")
  # }
  #   ggplot(temp_small, aes(datetime, Temp)) +
  #     geom_point() 
  # })
  # 
  # output$plot3 <- renderPlot({
  #   ggplot(temp_small, aes(datetime, Temp)) +
  #     geom_point() +
  #     coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
  # })
  # 
  # # When a double-click happens, check if there's a brush on the plot.
  # # If so, zoom to the brush bounds; if not, reset the zoom.
  # observe({
  #   brush <- input$plot2_brush
  #   if (!is.null(brush)) {
  #     ranges2$x <- c(brush$xmin, brush$xmax)
  #     ranges2$y <- c(brush$ymin, brush$ymax)
  #     
  #   } else {
  #     ranges2$x <- NULL
  #     ranges2$y <- NULL
  #   }
  # })
  
}

shinyApp(ui, server)
