# Water Temperature Shiny App
# Catarina Pien 
# 12/11/2019

# Set up the environment----------------------------------------------------------------------------

# Clear the environment
rm(list=ls(all=TRUE))

# Load packages
library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(TTR)
library(anomalize) # rate of change
library(tibbletime) # rate of change - tables

# Read in file --------------------------------------------------------------
setwd("C:/Users/cpien/OneDrive - California Department of Water Resources/Work/ClimateChange/R_code/climatechange/")
temp_H <- readRDS("WaterTemp/data/Temp_all_H.rds")

# Function to determine whether values are repeating by each station -------------------------------------
# Inputs are data frame and x (number of repeating values you want to check for)
# Check if number is same as previous number. If yes, 1. If no, 0.
# Cumulative sum so each time a value repeats, cumulative sum goes up
# Count the number in a row that are the same
# Flag if that number > threshold 

repeating_vals = function(df, x){
  df$same = ifelse(df$Temp == lag(df$Temp, 1, default = 0), 1L, 0L)
  df = df %>%
    mutate(issame = cumsum(df$same == 0L)) %>%
    group_by(station, issame) %>%
    mutate(flag = sum(same)+1 ) %>%
    ungroup() %>%
    mutate(Flag_repeats = ifelse(flag > x, "Y", "N"))
  return(df)
}

# Define User Interface -----------------------------------------------------------------

ui <- fluidPage(
  
  # Application title
  titlePanel("QC Water temperature data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    position = "left",
    sidebarPanel(
      h4("Filters"),
      uiOutput("station_selector"), # See station_selector in server section
      dateRangeInput("daterange", "Date Range:",
                     start = "2010-01-01", end = "2018-01-01", min = "1980-01-01", startview = "decade"),
      numericInput("missvals",
                   "Missing Values allowed per day:",
                   min = 0, max = 24, value = 5),
      numericInput("repeatvals",
                   "Repeating values allowed:",
                   min = 0, max = 24, value = 18),
      selectInput("trend",
                  "Trend duration",
                  list("2 weeks", "1 month", "2 months", "3 months", "4 months", "5 months","6 months", "1 year", 
                       startview = "4 months")),
      selectInput("seasonal",
                  "Seasonal Decomposition Method (Loess = STL, Median = Twitter):",
                  list("STL", "Twitter")),
      selectInput("remainder",
                  "Remainder Analysis Type (IQR: 3x above, 3x below IQR, GESD: progressively removes critical values, iterative:",
                  list("IQR", "GESD")),
      sliderInput("temprange",
                  "Temperature Cutoffs:",
                  min = -10, max = 100,value = c(0,40)),
      actionButton("submit", "Submit")
    ),
    
    # main panel
    mainPanel(h4("Values removed"),
              tableOutput("vals_removed"),
              h4("Plot: Pre-QC with removed values"),
              h5("Yellow = Missing values, Red = Repeating values, Green = Rate of Change, 
              Bright Blue = Temp Range"),
              plotOutput("preQC", dblclick = "preQC_dblclick",
                         brush = brushOpts(id = "preQC_brush",
                                           resetOnNew = TRUE)),
              h4("Plot: Filtered for easier viewing"),
              plotOutput("postQC_F")
              
    )
  )
)

# Define server logic -------------------------------------------------------------------
server <- function(input, output) {
  
  ## Creates list of stations based on the dataframe -------------------------------------------  
  output$station_selector = renderUI({ #creates station select box object called in ui
    selectInput(inputId = "station", #name of input
                label = "Station Code:", #label displayed in ui
                choices = as.character(unique(temp_H$station)),
                # calls unique values from the station column in the previously created table
                selected = "ANC") #default choice (not required)
  })
  
 
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  
  ### Filter station and dates from input
  temp_sta <-  eventReactive(input$submit, {
    temp_H %>%
      filter(station == input$station) %>%
      filter(date >= input$daterange[1] & date <= input$daterange[2]) 
  })
  
  ### QC Range for acceptable temperatures ###
  temp_q4 <- eventReactive(input$submit, { 
    temp_sta() %>%
      filter(!is.na(datetime)) %>%
      mutate(Flag_QC4 = ifelse((Temp<input$temprange[1] | Temp>input$temprange[2]), "Y", "N"))
    
  })
  
  ### Missing values ###
  temp_q1_a <- eventReactive(input$submit,{
    temp_q4() %>%
      group_by(station, date) %>%
      arrange(station, date, hour) %>%
      summarise(total = length(date)) %>%
      mutate(Flag_QC1 = ifelse(total<(24-(input$missvals)), "Y", "N"))
  })
  
  temp_q1 <- eventReactive(input$submit, {
    dplyr::left_join(temp_q4(), temp_q1_a(), by = c("station", "date"))
  })

  ### Repeating Values
  temp_q2 <- eventReactive(input$submit, {
    repeating_vals(df = temp_q1(), x = input$repeatvals)%>%
    rename(Flag_QC2 = Flag_repeats) })
  
  
  ### Anomalies
  temp_q3_a <- eventReactive(input$submit,{
    as_tbl_time(temp_q2(), index = datetime)})
  
  temp_q3_c <- eventReactive(input$submit, {
    temp_q3_a() %>%
    time_decompose(Temp, method = input$seasonal, trend = input$trend) %>%
    anomalize(remainder, method = input$remainder) %>%
    time_recompose() %>% 
    select(c(datetime, anomaly)) %>%
    as_tibble() })
  
  temp_q3_d <- eventReactive(input$submit, {
    inner_join(temp_q2(), temp_q3_c(), by = c( "datetime")) })
  
  temp_q3 <- eventReactive(input$submit,{
    temp_q3_d() %>%
    mutate(anomaly = factor(anomaly)) %>%
    mutate(anomaly = recode(anomaly, No = "N", Yes = "Y"))  %>%
    rename(Flag_QC3 = anomaly) })
  
   
  ### Filter all delete rows
  temp_q4_b <-  reactive( {
    temp_q4() %>%
     filter(Flag_QC4 == "Y")})  

  temp_q1_b <- reactive({
    temp_q1() %>%
      filter(Flag_QC1 == "Y")})
      
  temp_q2_b <- reactive({ 
    temp_q2() %>%
    filter(Flag_QC2 == "Y") })
  
  temp_q3_b <- reactive({ 
    temp_q3() %>%
    filter(Flag_QC3 == "Y") })
  
  ###################################################################
  ## Plot 1: PreQC ------------------------------------------------------------ 
  ##################################################################
 
  output$preQC <- renderPlot({
    
    # This little if section somehow makes the time datetime plot. Without this it breaks!
    if (!is.null(ranges$x)) {
      ranges$x <- as.POSIXct(ranges$x, origin = "1970-01-01")
    }
    
    # Plot data
    ggplot() +
      geom_point(data = temp_q3(), aes(datetime, Temp), col = "lightsteelblue3") +
      geom_point(data = temp_q1_b(), aes(datetime, Temp), color = "goldenrod2", size = 2) +
      geom_point(data = temp_q2_b(), aes(datetime, Temp), color = "indianred3", size = 2) +
      geom_point(data = temp_q3_b(), aes(datetime, Temp), color = "springgreen4", size = 2) +
      geom_point(data = temp_q4_b(), aes(datetime, Temp), color = "cyan3", size = 2) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE) +
      #annotation_custom(grob = grid::textGrob(paste(deleted_total, "deleted values")),
      #                  xmin = -Inf, xmax = Inf, ymin = max(temp_sta$Temp), ymax = max(temp_sta$Temp)) +
      scale_x_datetime() + #ylim(0,30) +
      theme_bw() +
      theme(axis.title = element_text(size = 16),
            axis.text = element_text(size = 16),
            axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$preQC_dblclick, {
    brush <- input$preQC_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  ############################################################################
  ## Plot2: PostQC Final ---------------------------------------------------
  ##############################################################################
  
  output$postQC_F <- renderPlot({
    # This will initiate the submit button
    input$submit 
    
    ############################ TEMPORARY ########################
    temp_q4_mod <- temp_q3() %>%
      filter(Flag_QC4 == "N")
    ###############################################################
    
    ####################### EDITED ##############################################
    temp_q3_plot <- left_join(temp_q3_b(), temp_q4_mod) %>%
      filter(Flag_QC4 == "N")
    ###############################################################
    
    # Plot the data ########################################################################################
    # Each displays the deleted data in a different color, along with the "cleaned" data. 
    # Annotation custom displays the deleted rows in the center of the plot
    ggplot() +
      geom_point(data = temp_q4_mod, aes(datetime, Temp), col = "lightsteelblue3") +
      geom_point(data = temp_q1_b(), aes(datetime, Temp), color = "goldenrod2", size = 2) +
      geom_point(data = temp_q2_b(), aes(datetime, Temp), color = "indianred3", size = 2) +
      geom_point(data = temp_q3_plot, aes(datetime, Temp), color = "springgreen4", size = 2) +
      #coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      #annotation_custom(grob = grid::textGrob(paste(deleted_total, "deleted values")),
      #                  xmin = -Inf, xmax = Inf, ymin = max(temp_sta$Temp), ymax = max(temp_sta$Temp)) +
      scale_x_datetime() + #ylim(0,30) +
      theme_bw() +
      theme(axis.title = element_text(size = 16),
            axis.text = element_text(size = 16),
            axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
  
  
  
  ###############################################################
  ## Table ---------------------------------------------------------------- 
  ################################################################
  
  output$vals_removed <- renderTable({
    # This will initiate the submit button
    input$submit 
  
    ### Make table
    vals_removed <- temp_q3() %>%
      group_by(station) %>%
      summarize(Init = n(),
                QC1 = sum(Flag_QC1 =="Y"),
                QC2 = sum(Flag_QC2 == "Y"),
                QC3 = sum(Flag_QC3 == "Y", na.rm = TRUE),
                QC4 = sum(Flag_QC4 == "Y"),
                Remaining = sum(Flag_QC1=="N" & Flag_QC2 == "N" & Flag_QC3 =="N" & Flag_QC4 =="N", na.rm = TRUE),
                Prop_remaining = round(Remaining/Init*100,1)) %>%
      arrange(Prop_remaining)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

