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
### Change setwd if you need to:
# setwd("C:/Users/cpien/OneDrive - California Department of Water Resources/Work/ClimateChange/R_code/climatechange/WaterTemp/")
temp_H <- readRDS("data/Temp_all_H.rds")

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
  titlePanel("Water Temperature Synthesis QC App"),
  
  h4("Alter the inputs on the left sidebar to edit the data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    position = "left",
    sidebarPanel(
      h3("Filters"),
      h4("Station and Date"),
      uiOutput("station_selector"), # See station_selector in server section
      dateRangeInput("daterange", "Date Range:",
                     start = "1986-01-01", end = "2019-12-31", min = "1980-01-01", startview = "decade"),
      h4("1. Temperature cutoffs"),
      sliderInput("temprange",
                  "Temperature Cutoffs:",
                  min = -10, max = 100,value = c(1,40)),
      h4("2. Missing Values"),
      numericInput("missvals",
                   "Missing Values allowed per day:",
                   min = 0, max = 24, value = 4),
      h4("3. Repeating Values"),
      numericInput("repeatvals",
                   "Repeating values allowed:",
                   min = 0, max = 24, value = 18),
      h4("4. Anomaly Detection"),
      selectInput("trend",
                  "Trend duration",
                  list("2 weeks", "1 month", "2 months", "3 months", "4 months", "5 months","6 months", "1 year"), 
                  selected = "3 months"),
      selectInput("seasonal",
                  "Seasonal Decomposition Method (Loess = STL, Median = Twitter):",
                  list("STL", "Twitter")),
      selectInput("remainder",
                  "Remainder Analysis Type (IQR: 3x above, 3x below IQR, GESD: progressively removes critical values, iterative, very slow:",
                  list("IQR", "GESD")),
      numericInput("alpha",
                   "Select alpha: smaller value makes it more difficult to be an anomaly (3 x IQR = 0.05, 6 x IQR = 0.025, 1.5 x IQR = 0.1)",
                   min = 0.025, max = 0.1, value = 0.05, step = 0.0125),
      h4("5. Spike"),
      numericInput("spike",
                   "Threshold temperature difference between values:",
                   min = 0, max = 20, value = 5),
      h4("6. Rate of Change"),
      numericInput("nsdev",
                   "Rate of change: Number of standard deviations allowed from time average:",
                   min = 0, max = 10, value = 5),
      numericInput("pasthours", 
                   "Rate of Change: Number of hours averaged for rate of change:",
                   min = 0, max = 720, value = 50),
      actionButton("submit", "Submit"),
      h3("Download Data"),
      p(em(span("Note: Only downloads the data for the selected station and dates.", style = "color:coral"))),
      p(strong("Flagged data contains all data | Filtered data contains only flag-free data")),
      selectInput("dataset", "Choose a dataset:",
                  choices = c("Flagged", "Filtered")),
      
      downloadButton("downloadData", "Download")
      
    ),
    
    # main panel
    mainPanel(h3("Values Flagged"),
              p(em(span("Note: QC1 values (values outside of temperature range) were filtered out prior to conducting tests QC2-QC6", 
                        style = "color:coral"))),
              tableOutput("vals_flagged"),
              h3("Plot 1: Pre-QC with flagged values"),
              p(strong("To zoom in, highlight points, then double click inside box. Zoom works on
              both plot 1 and plot 2, but zoomed output will show up in plot 1. Double click on plot 1 to zoom back out.")),
              
              p(strong(span("Blue x", style = "color:darkturquoise")),
                "= QC1 Temperature limits"),
              p(strong(span("Yellow triangle", style= "color:gold")),
                "= QC2 Missing values"),
              p(strong(span("Red circle", style = "color:firebrick")),
                "= QC3 Repeating values"),
              p(strong(span("Green diamond", style = "color:seagreen")),
                "= QC4 Anomalies"), 
              p(strong(span("Magenta square", style = "color:mediumorchid")),
                "= QC5 Spike Test"),
              p(strong(span("Navy upside down triangle", style = "color:darkslateblue")),
                "= QC6 Rate of Change"),
              
              plotOutput("preQC", 
                         click = "preQC_click",
                         dblclick = "preQC_dblclick",
                         brush = brushOpts(id = "preQC_brush",
                                           resetOnNew = TRUE)),
              p(strong(span("Single click on a point to look at nearby datetimes and temperatures"))),
              verbatimTextOutput("info"),
              h3("Plot 2: Filtered for temperature limits"),
              plotOutput("postQC_F", dblclick = "preQC_dblclick",
                         brush = brushOpts(id = "preQC_brush",
                                           resetOnNew = TRUE)),
              tableOutput("table")
              
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
  
  ### Define reactive values for the zoom function
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  
  ### Filter station and dates from input
  temp_sta <-  eventReactive(input$submit, {
    temp_H %>%
      filter(station == input$station) %>%
      filter(date >= input$daterange[1] & date <= input$daterange[2]) 
  })
  
  ### QC1) Range for acceptable temperatures ###
  temp_q1 <- eventReactive(input$submit, { 
    temp_sta() %>%
      mutate(Flag_QC1 = ifelse((Temp<input$temprange[1] | Temp>input$temprange[2]), "Y", "N"))
    
  })

  ### QC2) Missing values ###
  temp_q2_a <- eventReactive(input$submit,{
    temp_q1() %>%
      filter(Flag_QC1 == "N") %>% # See next comment about removing QC1="Y" values
      group_by(station, date) %>%
      arrange(station, date, hour) %>%
      summarise(total = length(date)) %>%
      mutate(Flag_QC2 = ifelse(total<(24-(input$missvals)), "Y", "N")) %>%
      select(-total)
  })
  
  temp_q2 <- eventReactive(input$submit, {
    left_join(temp_q1(), temp_q2_a(), by = c("station", "date")) %>%
      filter(Flag_QC1 == "N") # This part is important for QC5 and QC6. Basically removes all values that are not within range (QC1) 
    # for BET (maybe other stations) there were some alternately repeating values near 0 that were causing lots of spike QCs to be positive.
  })
  
  ### QC3) Repeating Values
  temp_q3 <- eventReactive(input$submit, {
    repeating_vals(df = temp_q2(), x = input$repeatvals)%>%
      select(-flag, -issame, -same) %>%
      rename(Flag_QC3 = Flag_repeats) })
  
  
  ### QC4) Anomalies
  temp_q4_a <- eventReactive(input$submit,{
    as_tbl_time(temp_q3(), index = datetime)})
  
  temp_q4_c <- eventReactive(input$submit, {
    temp_q4_a() %>%
      time_decompose(Temp, method = input$seasonal, trend = input$trend) %>%
      anomalize(remainder, method = input$remainder, alpha = input$alpha) %>%
      time_recompose() %>% 
      select(c(datetime, anomaly)) %>%
      as_tibble() })
  
  temp_q4_d <- eventReactive(input$submit, {
    inner_join(temp_q3(), temp_q4_c(), by = c( "datetime")) })
  
  temp_q4 <- eventReactive(input$submit,{
    temp_q4_d() %>%
      mutate(anomaly = factor(anomaly)) %>%
      mutate(anomaly = recode(anomaly, No = "N", Yes = "Y"))  %>%
      rename(Flag_QC4 = anomaly) })
  
  
  ### Spike
  temp_q5 <- eventReactive(input$submit, {
    temp_q4() %>%
      group_by(station) %>%
      arrange(station, datetime) %>%
      mutate(QC5 = abs(Temp- 0.5 * (lag(Temp, n = 1, default = 0) + lead(Temp, n=1, default = 0))))%>%
      mutate(Flag_QC5 = ifelse((QC5 > input$spike), "Y", "N"))  %>%
      mutate(Flag_QC5 = replace(Flag_QC5, is.na(Flag_QC5), "N")) %>%
      select(-QC5) %>%
      ungroup()
  })
  
  ### Rate of Change
  temp_q6 <- eventReactive(input$submit, {
    temp_q5() %>%
      group_by(station) %>%
      arrange(station, datetime) %>%
      mutate(QC6 = abs(Temp- lag(Temp, n = 1, default = 0)))%>%
      mutate(sdev_th = input$nsdev * runSD(Temp, input$pasthours))%>%
      mutate(Flag_QC6 = ifelse((QC6 > sdev_th), "Y", "N"))  %>%
      mutate(Flag_QC6 = replace(Flag_QC6, is.na(Flag_QC6), "N")) %>%
      select(-c(QC6, sdev_th)) %>%
      ungroup()
  })
  
  ### Filter all delete rows
  temp_q1_b <-  reactive( {
    temp_q1() %>%
      filter(Flag_QC1 == "Y")})  
  
  temp_q2_b <- reactive({
    temp_q2() %>%
      filter(Flag_QC2 == "Y")})
  
  temp_q3_b <- reactive({ 
    temp_q3() %>%
      filter(Flag_QC3 == "Y") })
  
  temp_q4_b <- reactive({ 
    temp_q4() %>%
      filter(Flag_QC4 == "Y") })
  
  temp_q5_b <- reactive({
    temp_q5() %>%
      filter(Flag_QC5 == "Y")
  })
  
  temp_q6_b <- reactive({
    temp_q6() %>%
      filter(Flag_QC6 == "Y")
  })
  
  
  
  ### Create and select datasets ###---------------------------------------------------------  
  
  ### Merge back in QC1 Flags, since these were removed from the q6 dataset
  temp_q1_table <- reactive({
    temp_q1() %>%
      mutate(Flag_QC2 = "N",
             Flag_QC3 = "N", 
             Flag_QC4 = "N",
             Flag_QC5 = "N",
             Flag_QC6 = "N")
  })
  
  ### AllFlags
  temp_flags <- reactive( {
    rbind(temp_q6(), temp_q1_table()) %>%
      mutate(AllFlags = paste0(Flag_QC1, ",", Flag_QC2, ",", Flag_QC3, ",", Flag_QC4, ",", Flag_QC5, ",", Flag_QC6))
  })
  
  ### Filtered data 
  temp_final <- reactive( {
    temp_flags() %>%
      filter(grepl("N,N,N,N,N,N", AllFlags)) })
  
  ### Have user select which dataset they want
  datasetInput <- reactive({
    switch(input$dataset,
           "Flagged" = temp_flags(),
           "Filtered" = temp_final())
  })
  
  
  ### ----------------------------------------------------------------------------------------
  
  
  
  # Plot 2: Within temperature limits 
  # Filter out temperature range so outliers don't distort the plot to the point that it is 
  # hard to see the actual trends 
  
  # For plot 2
  temp_q6_plot <- eventReactive(input$submit, {
    temp_q6() %>%
      filter(Flag_QC1 =="N")
  })
  
  temp_q2_b_plot <- reactive({
    temp_q6_plot() %>%
      filter(Flag_QC2 == "Y") })
  
  temp_q3_b_plot <- reactive({
    temp_q6_plot() %>%
      filter(Flag_QC3 == "Y") })
  
  temp_q4_b_plot <- reactive({
    temp_q6_plot() %>%
      filter(Flag_QC4 == "Y")})
  
  temp_q5_b_plot <- reactive({
    temp_q6_plot() %>%
      filter(Flag_QC5 == "Y")})
  
  temp_q6_b_plot <- reactive({
    temp_q6_plot() %>%
      filter(Flag_QC6 == "Y")})
  
  
  ###############################################################
  ## Table -------------------------------------------------------
  ################################################################
  
  # Describes the amount of data removed 
  
  output$vals_flagged <- renderTable({
    
    # Make table
    # Counts the number of rows of data being flagged
    temp_flags() %>%
      group_by(station) %>%
      summarize(Init = n(),
                QC1 = sum(Flag_QC1 =="Y"),
                QC2 = sum(Flag_QC2 == "Y"),
                QC3 = sum(Flag_QC3 == "Y", na.rm = TRUE),
                QC4 = sum(Flag_QC4 == "Y"),
                QC5 = sum(Flag_QC5 == "Y"),
                QC6 = sum(Flag_QC6 == "Y"),
                QCTotal = sum(grepl("Y", AllFlags)),
                Percent_Flagged = round(QCTotal/Init * 100,1)) %>%
      arrange(Percent_Flagged)
    
  })
  
  
  ###################################################################
  ## Plot 1: PreQC ---------------------------------------------------
  ##################################################################
  
  # Plot of all data and flags 
  
  output$preQC <- renderPlot({
    
    # This little if section somehow makes the time datetime plot. Without this it breaks!
    if (!is.null(ranges$x)) {
      ranges$x <- as.POSIXct(ranges$x, origin = "1970-01-01")
    }
    
    # Plot data
    ggplot() +
      geom_point(data = temp_q6(), aes(datetime, Temp), col = "lightsteelblue3") +
      geom_point(data = temp_q1_b(), aes(datetime, Temp), color = "cyan3", size = 4, shape = 4) +
      geom_point(data = temp_q2_b(), aes(datetime, Temp), color = "goldenrod2", size = 3, shape = 2) +
      geom_point(data = temp_q3_b(), aes(datetime, Temp), color = "indianred3", size = 3, shape = 20) +
      geom_point(data = temp_q4_b(), aes(datetime, Temp), color = "springgreen4", size = 3.5, shape = 18) +
      geom_point(data = temp_q5_b(), aes(datetime, Temp), color = "mediumorchid", size = 3, shape = 0) +
      geom_point(data = temp_q6_b(), aes(datetime, Temp), color = "darkslateblue", size = 3, shape = 6) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE) +
      labs(y = "Temp (deg C)") +
      scale_x_datetime() + 
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
  
  output$info <- renderPrint({
    d <- nearPoints(temp_flags(), input$preQC_click, xvar = "datetime", yvar = "Temp")[1:5,2:3]
    arrange(d, datetime)
    d
  })
  
  ############################################################################
  ## Plot2: PostQC Final ---------------------------------------------------
  ##############################################################################
  
  # Plot of data within temperature limits - makes it easier to see if there are really high or low values
  
  output$postQC_F <- renderPlot({
    
    
    
    # Plot the data ########################################################################################
    # Each geom_point() plots a different flag
    ggplot() +
      geom_point(data = temp_q6_plot(), aes(datetime, Temp), col = "lightsteelblue3") +
      geom_point(data = temp_q2_b_plot(), aes(datetime, Temp), color = "goldenrod2", size = 3, shape = 2) +
      geom_point(data = temp_q3_b_plot(), aes(datetime, Temp), color = "indianred3", size = 3, shape = 20) +
      geom_point(data = temp_q4_b_plot(), aes(datetime, Temp), color = "springgreen4", size = 3.5, shape = 18) +
      geom_point(data = temp_q5_b_plot(), aes(datetime, Temp), color = "mediumorchid", size = 3, shape = 0) +
      geom_point(data = temp_q6_b_plot(), aes(datetime, Temp), color = "darkslateblue", size = 3, shape = 6) +
      labs(y = "Temp (deg C)") +
      scale_x_datetime() +
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
  
  ##########################################################
  ### Download data
  ###############################################
  # output$table <- renderTable({
  #   temp_q6_b_plot()
  # })
  today = today()
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$station, "_", input$dataset, "_", today, ".csv", sep = "")
    },
    content = function(file) {
      write_csv(datasetInput(), file)
    }
  )
  
}


# Run the application 
shinyApp(ui = ui, server = server)

