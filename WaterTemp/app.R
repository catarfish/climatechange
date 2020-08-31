# Water Temperature Shiny App
# Catarina Pien 
# 06/01/2020

# This app takes integrated real-time water temperature data downloaded from CDEC (http://cdec.water.ca.gov/) and allows you to apply quality control filters on it. 
# Plots will highlight different QC filters and allow you to adjust settings for different filters.
# You can download the flagged or filtered version of each station's dataset.
# Full dataset and metadata published on EDI. 


# Set up the environment---------------------------------------------------------

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

# Read in compiled raw temperature data
temp_H <- readRDS("data/Temp_all_H.rds")
# Optional - to remove non-contiguous stations
# filter(!Station %in% c("CNT", "CPP", "DAR", "DMC", "DYR", "ECD", "HBP", "ROR", "DV7")) 

# Read in station name, lat, lon
latlons <- read.csv("data/StationsMetadata.csv")
# Optional - to remove non-contiguous stations
#  filter(!station %in% c("CNT", "CPP", "DAR", "DMC", "DYR", "ECD", "HBP", "ROR", "DV7")) %>% 

# As.list will allow you to use names() to display station name rather than Station code
latlonmin <- as.data.frame(select(latlons, 1:2)) # For merging at the end
latlons <- as.list(mutate(latlons, staDesc = paste(Station, StationName, sep = " | ")))
names(latlons$Station) <- latlons$staDesc

# Progress bar settings --------------------------------------------------------
info_loading <- "Loading Data"
progress_color <- "#fef0d9"
progress_background <- "#e34a33"

# Function to determine whether values are repeating by each station --------------------------
  # Inputs are data frame and x (number of repeating values you want to check for)
  # Check if number is same as previous number. If yes, 1. If no, 0.
  # Cumulative sum so each time a value repeats, cumulative sum goes up
  # Count the number in a row that are the same
  # Flag if that number > threshold 

repeating_vals = function(df, x){
  df$same = ifelse(df$Temp == lag(df$Temp, 1, default = 0), 1L, 0L)
  df = df %>%
    mutate(issame = cumsum(df$same == 0L)) %>%
    group_by(Station, issame) %>%
    mutate(flag = sum(same)+1 ) %>%
    ungroup() %>%
    mutate(Flag_repeats = ifelse(flag > x, "Y", "N"))
  return(df)
}

# Define User Interface -----------------------------------------------------------------

ui <- fluidPage(
  
  # Application title
  titlePanel(title = div(h2("Continuous Water Temperature Synthesis Data Flagging App", style = "display: inline-block"),
                         a(img(src="IEP_logo_compliant_colors.jpg", height = 130, align="right", style="display: inline-block"), href="https://water.ca.gov/Programs/Environmental-Services/Interagency-Ecological-Program"), 
                         a(img(src="DWR.png", height = 130,  align="right", style="display: inline-block"), href="https://water.ca.gov/"),
                         h5("Version 0.1.0"),
                         h4("Alter the inputs on the left sidebar to edit the data, then press submit to see flagged data."),
                         h5(uiOutput("edi")),
                         h5(uiOutput("contact")))),
  
  # Sidebar with a slider input for number of bins -----------------------
  sidebarLayout(
    position = "left",
    sidebarPanel(
      h3("Filters"),
      
      # Station and Date Filters
      h4("Station and Date"),
      uiOutput("station_selector"), # See station_selector in server section
      dateRangeInput("daterange", "Date Range:",
                     start = "1986-01-01", end = "2019-12-31", min = "1980-01-01", startview = "decade"),
      
      # Q1: Temperature Cutoffs
      h4("1. Temperature cutoffs"),
      sliderInput("temprange",
                  "Temperature Cutoffs (degreesC):",
                  min = -10, max = 100,value = c(1,40)),
      
      # Q2: Missing Values
      h4("2. Missing Values"),
      numericInput("missvals",
                   "Missing Values allowed per day:",
                   min = 0, max = 24, value = 4),
      
      # Q3: Repeating Values
      h4("3. Repeating Values"),
      numericInput("repeatvals",
                   "Repeating values allowed:",
                   min = 0, max = 24, value = 18),
      
      # Q4: Anomalize
      h4("4. Anomaly Detection"),
      h5(em(span(uiOutput("anom"), style = "color:chocolate"))),
        # Trend duration
      selectInput("trend",
                  "Trend duration (natural trend duration of data):",
                  list("2 weeks", "1 month", "2 months", "3 months", "4 months", 
                       "5 months", "6 months", "9 months", "1 year"), 
                  selected = "6 months"),
        # Seasonal Decomposition Method
      selectInput("seasonal",
                  "Seasonal Decomposition Method (Loess = STL, Median = Twitter):",
                  list("STL", "Twitter")),
      
        # Remainder analysis
      p(em(span("Note: GESD progressively removes critical values 
                and is thus very slow.", style = "color:chocolate"))),
      selectInput("remainder",
                  "Remainder Analysis Type (IQR = Interquartile Range, GESD = Generalized Extreme Studentized Deviate Test):",
                  list("IQR", "GESD")),
        # Alpha
  
      numericInput("alpha",
                   "Alpha (adjusts outlier detection sensitivity):",
                   min = 0.025, max = 0.1, value = 0.05, step = 0.0125),
          p(em(span("Note: smaller alpha value makes it more difficult 
                to be an anomaly (3 x IQR = alpha 0.05, 6 x IQR = 0.025, 1.5 x IQR = 0.1)", 
                style = "color:chocolate"))),
      # Q5: Spike (compare to values before and after)
      # Modified from https://github.com/SuisunMarshBranch/wqptools/blob/master/R/rtqc.r
      
      h4("5. Spike"),
      numericInput("spike",
                   "Threshold temperature difference between consecutive values (degreesC):",
                   min = 0, max = 20, value = 5),
      
      # Q6: Rate of change (based on standard deviations from past n hours of data)
      h4("6. Rate of Change"),
      p(em(span("Rate of change between Tn and Tn-1 must be less than user-specified standard deviations. Standard deviation is calculated over user-specified time period.", 
                style = "color:chocolate"))),
      numericInput("pasthours", 
                   "Number of hours averaged for rate of change (1 diurnal/tidal cycle = ~25 hours):",
                   min = 0, max = 720, value = 50),      
      numericInput("nsdev",
                   "Threshold standard deviations allowed for rate of change:",
                   min = 0, max = 10, value = 5),

      
      # Submit button
      actionButton("submit", "Submit"),
      
      # Download button: choose flagged or filtered dataset
      h3("Download Data"),
      p(em(span("Note: Only downloads the data for the selected station 
                and dates. Click submit before downloading data.", style = "color:chocolate"))),
      p(strong("Flagged data"), 
        "contains all data | ", 
        strong("Filtered data"), 
        "contains only flag-free data"),
      selectInput("dataset", "Choose a dataset:",
                  choices = c("Flagged", "Filtered")),
      p(em(span("Note: If you change the filename, add .csv to 
                the end of your filename.", style = "color:chocolate"))),
      downloadButton("downloadData", "Download")
    ),
    
    # main panel ------------------------------------------
    mainPanel(h3("Values Flagged"),
              p(strong("Displays number of values in station dataset, number of values flagged for each QC filter, and percent flagged overall.",
                style = "color:chocolate")),
              p(em(span("Note: Values outside of QC1-specified temperature range
                        were filtered out prior to conducting tests QC2-QC6 for greater effectiveness of tests.", 
                        style = "color:chocolate"))),
              tableOutput("vals_flagged"),
              h3("Plot 1: Raw data with flagged data highlighted"),
              p(strong(span("To zoom in, highlight points, then double click inside box. 
              Zoom works on both plot 1 and plot 2, but zoomed output will show up in plot 1. 
              Double click on plot 1 to zoom back out.", style = "color:chocolate"))),
              
              p(strong("QC1"),
                strong(span("Blue x", style = "color:darkturquoise")),
                "= Temperature limits"),
              p(strong("QC2"),
                strong(span("Yellow triangle", style= "color:gold")),
                "= Missing values"),
              p(strong("QC3"),
                strong(span("Red circle", style = "color:firebrick")),
                "= Repeating values"),
              p(strong("QC4"),
                strong(span("Green diamond", style = "color:seagreen")),
                "= Anomalies"), 
              p(strong("QC5"),
                strong(span("Magenta square", style = "color:mediumorchid")),
                "= Spike Test"),
              p(strong("QC6"),
                strong(span("Navy upside down triangle", style = "color:darkslateblue")),
                "= Rate of Change"),
              
              # Plot 1: Pre-QC values
              plotOutput("preQC", 
                         click = "preQC_click",
                         dblclick = "preQC_dblclick",
                         brush = brushOpts(id = "preQC_brush",
                                           resetOnNew = TRUE)),
              p(strong(span("Single click on a point from Plot 1 to display nearby 
                            date/times and temperatures below.", style = "color:chocolate"))),
              verbatimTextOutput("info"),
              
              # Plot 2: Pre-QC values, flagged, between 1-40 C
              h3("Plot 2: Data filtered for user-specified temperature limits (QC1)"),
              plotOutput("postQC_F", dblclick = "preQC_dblclick",
                         brush = brushOpts(id = "preQC_brush",
                                           resetOnNew = TRUE)),
              
              # Table: Flagged values 
              tableOutput("table")
    )),
  
  
  # Header: Loading Message. From ZoopSynth App.
  tags$head(tags$style(type="text/css",
                       paste0("
                                             #loadmessage {
                                             position: fixed;
                                             top: 0px;
                                             left: 0px;
                                             width: 100%;
                                             padding: 5px 0px 5px 0px;
                                             text-align: center;
                                             font-weight: bold;
                                             font-size: 100%;
                                             color: ", progress_color,";
                                             background-color: ", progress_background,";
                                             z-index: 105;
                                             }
                                             "))),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div(info_loading,id="loadmessage")),
  tags$style(type="text/css", ".recalculating {opacity: 1.0;}")
)

# ---------------------------------------------------------------------------------------
# Define server logic -------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
server <- function(input, output) {
  
  ## Creates list of stations based on the dataframe -------------------------------------------  
  output$station_selector = renderUI({ #creates station select box object called in ui
    selectInput(inputId = "Station", #name of input
                label = "Station Code:", #label displayed in ui
                choices = latlons$Station,
                # calls unique values from the station column in the previously created table
                selected = "ANC") #default choice (not required)
  })
  
  
## Hyperlinks
  # EDI
  ediurl <- a("https://portal.edirepository.org/nis/mapbrowse?packageid=edi.591.2", href="https://portal.edirepository.org/nis/mapbrowse?packageid=edi.591.2")
  output$edi <- renderUI({
    tagList("Integrated dataset is published on the Environmental Data Initiative's Data Portal. Find data and metadata at:", ediurl)
  })
  
  # Contact Info
  contacturl <- a("Catarina.Pien@water.ca.gov", href="mailto:Catarina.Pien@water.ca.gov")
  output$contact <- renderUI({
    tagList("Please email questions to ", contacturl)
  })
  
  # Anomalize
  anomalizeurl <- a("anomalize package", href="https://business-science.github.io/anomalize/articles/anomalize_methods.html")
  output$anom <- renderUI({
    tagList("For more information about this test, see", anomalizeurl)
  })
  
  

  

  ### Define reactive values for the zoom function ---------------------------------
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  #### QC Code ----------------------------------------------------------------------
  # ---------------------------------------------------------------------------------
  
  ### Filter station and dates from input
  temp_sta <-  eventReactive(input$submit, {
    temp_H %>%
      filter(Station == input$Station) %>%
      filter(Date >= input$daterange[1] & Date <= input$daterange[2]) 
  })
  
  ### QC1) Range for acceptable temperatures
    # Flag temperatures that are out of user selected range
  temp_q1 <- eventReactive(input$submit, { 
    temp_sta() %>%
      mutate(Flag_QC1 = ifelse((Temp<input$temprange[1] | Temp>input$temprange[2]), "Y", "N"))
  })

  ### QC2) Missing values
    # Flag days where there are greater than user selected number of missing values (hours out of 24)
  temp_q2_a <- eventReactive(input$submit,{
    temp_q1() %>%
      filter(Flag_QC1 == "N") %>% # See next comment about removing QC1="Y" values
      group_by(Station, Date) %>%
      arrange(Station, Date, Hour) %>%
      summarise(total = n()) %>%
      mutate(Flag_QC2 = ifelse(total<(24-(input$missvals)), "Y", "N")) %>%
      select(-total) 
  })
  
  # Join flagged daily data to the rest of the hourly data
  temp_q2 <- eventReactive(input$submit, {
    
    left_join(temp_q1(), temp_q2_a(), by = c("Station", "Date")) %>%
      filter(Flag_QC1 == "N")# This part is important for QC5 and QC6. Basically removes all values that are not within range (QC1) 
    # for BET (maybe other stations) there were some alternately repeating values near 0 that were causing lots of spike QCs to be positive.
  })
  
  ### QC3) Repeating Values
    # Flag values repeating for user selected number of repeats 
    # Uses function repeating_vals
  temp_q3 <- eventReactive(input$submit, {
    repeating_vals(df = temp_q2(), x = input$repeatvals)%>%
      select(-flag, -issame, -same) %>%
      rename(Flag_QC3 = Flag_repeats) })
  
  ### QC4) Anomalies
    # Use anomalize package to flag anomalies
    # Twitter and GESD for more highly seasonal data (however GESD very slow)
    # STL and IQR if seasonality is not a major factor
    # Trend period depends on personal knowledge of data
    # see https://business-science.github.io/anomalize/articles/anomalize_methods.html
  
  # Convert data frame to table
  temp_q4_a <- eventReactive(input$submit,{
    as_tbl_time(temp_q3(), index = Datetime)})
  
  # Anomaly Detection
  # time_decompose: separates time series into seasonal, trend, and remainder components
  # stl: loess works well when long term trend is present
  # twitter: (removes median rather than fitting smoother) - when long-term trend is less dominant than short-term seasonal component
  # anomalize: applies anomaly detection methods to remainder component
  # time_recompose: calculate limits to separate "normal" data from anomalies
  temp_q4_c <- eventReactive(input$submit, {
    temp_q4_a() %>%
      time_decompose(Temp, method = input$seasonal, trend = input$trend) %>%
      anomalize(remainder, method = input$remainder, alpha = input$alpha) %>%
      time_recompose() %>% 
      select(c(Datetime, anomaly)) %>%
      as_tibble() })
  
  # Join "anomaly" flag with rest of the data
  temp_q4_d <- eventReactive(input$submit, {
    inner_join(temp_q3(), temp_q4_c(), by = c( "Datetime")) })
  
  # Rename "anomaly" Flag_QC4 for consistency, change No to N and Yes to  Y
  temp_q4 <- eventReactive(input$submit,{
    temp_q4_d() %>%
      mutate(anomaly = factor(anomaly)) %>%
      mutate(anomaly = recode(anomaly, No = "N", Yes = "Y"))  %>%
      rename(Flag_QC4 = anomaly) })
  
  
  ### QC5) Spike
    # If |temp(t) - mean(temp(t-1) + temp(t+1))| > 5, it is flagged.
  temp_q5 <- eventReactive(input$submit, {
    temp_q4() %>%
      group_by(Station) %>%
      arrange(Station, Datetime) %>%
      mutate(QC5 = abs(Temp- 0.5 * (lag(Temp, n = 1, default = 0) + lead(Temp, n=1, default = 0))))%>%
      mutate(Flag_QC5 = ifelse((QC5 > input$spike), "Y", "N"))  %>%
      mutate(Flag_QC5 = replace(Flag_QC5, is.na(Flag_QC5), "N")) %>% # Replace NA with No
      select(-QC5) %>%
      ungroup()
  })
  
  ### QC6) Rate of Change
  temp_q6 <- eventReactive(input$submit, {
    temp_q5() %>%
      group_by(Station) %>%
      arrange(Station, Datetime) %>%
      mutate(QC6 = abs(Temp- lag(Temp, n = 1, default = 0)))%>%
      mutate(sdev_th = input$nsdev * runSD(Temp, input$pasthours))%>%
      mutate(Flag_QC6 = ifelse((QC6 > sdev_th), "Y", "N"))  %>%
      mutate(Flag_QC6 = replace(Flag_QC6, is.na(Flag_QC6), "N")) %>% # Replace NA with No
      select(-c(QC6, sdev_th)) %>%
      ungroup()
  })
  
  ### Filter all flagged rows: these are all the flagged values that will show up in the plot
  temp_q1_b <-  reactive( {
    temp_q1() %>% filter(Flag_QC1 == "Y")})  
  
  temp_q2_b <- reactive({
    temp_q2() %>% filter(Flag_QC2 == "Y")})
  
  temp_q3_b <- reactive({ 
    temp_q3() %>% filter(Flag_QC3 == "Y") })
  
  temp_q4_b <- reactive({ 
    temp_q4() %>% filter(Flag_QC4 == "Y") })
  
  temp_q5_b <- reactive({
    temp_q5() %>% filter(Flag_QC5 == "Y")})
  
  temp_q6_b <- reactive({
    temp_q6() %>% filter(Flag_QC6 == "Y") })
  
  ### Create and select datasets ###---------------------------------------------------------  
  
  # Merge back in q1 data since these were removed
  # Data that were filtered out were not subsequently run under other QC tests, so NA
  temp_q1_table <- reactive({
    temp_q1() %>%
      filter(Flag_QC1 == "Y") %>%
      mutate(Flag_QC2 = "NA",
             Flag_QC3 = "NA", 
             Flag_QC4 = "NA",
             Flag_QC5 = "NA",
             Flag_QC6 = "NA") })
  
  ### Combine Flags from QC1 with rest of flags
  # Remove some columns
  temp_flags <- reactive( {
    rbind(temp_q6(), temp_q1_table()) %>%
      ungroup() %>%
      mutate(AllFlags = paste0(Flag_QC1, ",", Flag_QC2, ",", Flag_QC3, ",", 
                               Flag_QC4, ",", Flag_QC5, ",", Flag_QC6)) %>%
      left_join(latlonmin, by = "Station") %>%
      select(Station, StationName, Datetime, Date, Temp, everything())
  })
  
  ### Filtered dataset - no flags
  # Remove some columns 
  temp_final <- reactive( {
    temp_flags() %>%
      filter(grepl("N,N,N,N,N,N", AllFlags)) %>%
      select(-c(contains("Flag"))) %>%
      select(Station, StationName, Datetime, Date, Temp, everything())
      })
  
  ### Have user select which dataset they want
  datasetInput <- reactive({
    switch(input$dataset,
           "Flagged" = temp_flags(),
           "Filtered" = temp_final())
  })
  
  ### Edit temperature data for plot 2 to filter out extreme temps-----------------------
  
  # Plot 2: Create a plot where you only see within temperature limits
  # Filter out temperature range so outliers don't distort the plot to the point that it is 
  # hard to see the actual trends 
  
  # Filter out unwanted temperatures (based on QC1 inputs)
  temp_q6_plot <- eventReactive(input$submit, {
    temp_q6() %>% filter(Flag_QC1 =="N")})
  
  # Filter only flagged values for each filter
  temp_q2_b_plot <- reactive({
    temp_q6_plot() %>% filter(Flag_QC2 == "Y") })
  
  temp_q3_b_plot <- reactive({
    temp_q6_plot() %>% filter(Flag_QC3 == "Y") })
  
  temp_q4_b_plot <- reactive({
    temp_q6_plot() %>% filter(Flag_QC4 == "Y")})
  
  temp_q5_b_plot <- reactive({
    temp_q6_plot() %>% filter(Flag_QC5 == "Y")})
  
  temp_q6_b_plot <- reactive({
    temp_q6_plot() %>% filter(Flag_QC6 == "Y")})
  
  
  ###############################################################
  ## Table -------------------------------------------------------
  ################################################################
  
  # Describes the amount of data removed 
  output$vals_flagged <- renderTable({
    
    # Make table
    # Counts the number of rows of data being flagged
    temp_flags() %>%
      group_by(Station) %>%
      summarize(Init = n(),
                QC1 = sum(Flag_QC1 =="Y"),
                QC2 = sum(Flag_QC2 == "Y"),
                QC3 = sum(Flag_QC3 == "Y", na.rm = TRUE),
                QC4 = sum(Flag_QC4 == "Y"),
                QC5 = sum(Flag_QC5 == "Y"),
                QC6 = sum(Flag_QC6 == "Y"),
                QCTotal = sum(grepl("Y", AllFlags)),
                Percent_Flagged = round(QCTotal/Init * 100,2)) %>%
      arrange(Percent_Flagged) })
  
  ###################################################################
  ## Plot 1: PreQC ---------------------------------------------------
  ##################################################################
  
  # Plot of all data and flags 
  
  output$preQC <- renderPlot({
    # This is for zooming Datetime plots
    if (!is.null(ranges$x)) {
      ranges$x <- as.POSIXct(ranges$x, origin = "1970-01-01")}
    
    # Plot data
    # Each geom_point() plots a different flag
    ggplot() +
      geom_point(data = temp_q6(), aes(Datetime, Temp), col = "lightsteelblue3") +
      geom_point(data = temp_q1_b(), aes(Datetime, Temp), color = "cyan3", size = 4, shape = 4) +
      geom_point(data = temp_q2_b(), aes(Datetime, Temp), color = "goldenrod2", size = 3, shape = 2) +
      geom_point(data = temp_q3_b(), aes(Datetime, Temp), color = "indianred3", size = 3, shape = 20) +
      geom_point(data = temp_q4_b(), aes(Datetime, Temp), color = "springgreen4", size = 3.5, shape = 18) +
      geom_point(data = temp_q5_b(), aes(Datetime, Temp), color = "mediumorchid", size = 3, shape = 0) +
      geom_point(data = temp_q6_b(), aes(Datetime, Temp), color = "darkslateblue", size = 3, shape = 6) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE) +
      labs(y = "Temperature (deg C)") +
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
  
  # Print values of points based on your click
  output$info <- renderPrint({
    d <- nearPoints(temp_flags(), input$preQC_click, xvar = "Datetime", yvar = "Temp")[1:5,c(3,5)]
    arrange(d, Datetime)
    d
  })
  
  ############################################################################
  ## Plot2: PostQC Final ---------------------------------------------------
  ##############################################################################
  
  # Plot of data within temperature limits - 
  # makes it easier to see if there are really high or low values
  
  output$postQC_F <- renderPlot({
    
    # Each geom_point() plots a different flag
    ggplot() +
      geom_point(data = temp_q6_plot(), aes(Datetime, Temp), col = "lightsteelblue3") +
      geom_point(data = temp_q2_b_plot(), aes(Datetime, Temp), color = "goldenrod2", size = 3, shape = 2) +
      geom_point(data = temp_q3_b_plot(), aes(Datetime, Temp), color = "indianred3", size = 3, shape = 20) +
      geom_point(data = temp_q4_b_plot(), aes(Datetime, Temp), color = "springgreen4", size = 3.5, shape = 18) +
      geom_point(data = temp_q5_b_plot(), aes(Datetime, Temp), color = "mediumorchid", size = 3, shape = 0) +
      geom_point(data = temp_q6_b_plot(), aes(Datetime, Temp), color = "darkslateblue", size = 3, shape = 6) +
      labs(y = "Temperature (deg C)") +
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
  today = format(today(), "%Y%m%d")
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$Station, "_", input$dataset, "_", today, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
}


# Run the application 
shinyApp(ui = ui, server = server)

