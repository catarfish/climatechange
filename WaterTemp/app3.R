# Water Temperature Shiny App
# Catarina Pien 
# 12/11/2019


# Set up the environment----------------------------------------------------------------------------

# Clear the environment
rm(list=ls(all=TRUE))

# Load packages
library(shiny)
library(readr)
library(tidyverse)
library(lubridate)
library(scales)
library(TTR)
library(caTools)

# Read in file --------------------------------------------------------------
setwd("C:/Users/cpien/OneDrive - California Department of Water Resources/Work/ClimateChange/R_code/climatechange/")
delta_H <- read_rds("WaterTemp/data/delta_H.rds")

# Function to determine whether values are repeating by each station -------------------------------------
# Inputs are data frame and x (number of repeating values you want to check for)
repeating_values <- function(df, x) {
  df %>% 
    group_by(station) %>%
    arrange(station, date, hour)
  for (i in 1:x) {
    print(i)
    same <- paste0('same', i)
    df[[same]]<- ifelse(df$Temp == lag(df$Temp, n = i), 1, 0)
  }
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
                     start = "2015-01-01", end = "2018-01-01", min = "1980-01-01", startview = "decade"),
      numericInput("missvals",
                   "Missing Values allowed per day:",
                   min = 0, max = 24, value = 5),
      numericInput("repeatvals",
                   "Repeating values allowed:",
                   min = 0, max = 24, value = 18),
      numericInput("nsdev",
                   "Rate of change: Number of standard deviations allowed from time average:",
                   min = 0, max = 5, value = 3),
      numericInput("pasthours", 
                   "Rate of Change: Number of hours averaged for rate of change:",
                   min = 0, max = 720, value = 25),
      sliderInput("temprange",
                  "Temperature Cutoffs:",
                  min = -10, max = 100,value = c(0,40)),
      actionButton("submit", "Submit")
    ),
    
    # main panel
    mainPanel(h4("Plot: Pre-QC with removed values"),
              h5("Yellow = Missing values, Red = Repeating values, Green = Rate of Change, Bright Blue = Temp Range, 
                 Black = Outliers (Median + IQR)"),
              plotOutput("preQC"),
              h4("Plot: Post-QC Final"),
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
                choices = as.character(unique(delta_H$station)),
                # calls unique values from the station column in the previously created table
                selected = "VOL") #default choice (not required)
  })
  
  ## Plot 1: PreQC ------------------------------------------------------------    
  # ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$preQC <- renderPlot({
    # This will initiate the submit button
    input$submit 
    
    # Filter station and dates from input #############################################
    delta_sta <- delta_H %>%
      filter(station == isolate(input$station)) %>%
      filter(date >= input$daterange[1] & date <= input$daterange[2])
    
    # QC1: Remove rows with x number of missing values ########################################
    delta_q1_a <- delta_sta %>%
      group_by(station, date) %>%
      arrange(station, date, hour) %>%
      summarise(total = length(date)) %>%
      mutate(Flag_QC1 = ifelse(total<(24-isolate(input$missvals)), "Y", "N")) 
    
    # Dataset with flags
    delta_q1 <- delta_sta %>%
      left_join(delta_q1_a, by = c("station", "date")) 
    
    # Create a dataframe of all the "delete" rows
    delta_q1_b <- delta_q1 %>%
      filter(Flag_QC1 == "Y")
    
    # QC2: Remove rows with x number of repeating values #############################################
    delta_q1$Temp <- as.numeric(delta_q1$Temp)
    
    # Run function repeating values.
    # Tally up "same" columns (same_total)
    # Create column "delete" - if same_total = x, delete = Y)
    # This does not delete the whole day's data - just when the values start repeating 
    delta_q2_a <- repeating_values(df = delta_q1, x = isolate((input$repeatvals)))%>%
      mutate(same_total = as.numeric(rowSums(select(., contains("same"))))) %>%
      mutate(Flag_QC2_a = ifelse(same_total == isolate((input$repeatvals)),"Y", "N")) %>%
      select(-contains("same"))
    
    # Apply another flag to delete the whole day
    delta_q2 <- delta_q2_a %>%
      group_by(station, date)%>%
      mutate(Flag_QC2_b = ifelse("Y" %in% Flag_QC2_a, "Y", "N")) %>%
      select(-c(Flag_QC2_a))
    
    # Create a dataframe of all the flagged rows
    delta_q2_b <- delta_q2 %>%
      filter(Flag_QC2_b == "Y")
      
    
    # QC3: Filter for rate of change #############################################
    
    delta_q3_a <- delta_q2 %>%
      group_by(station) %>%
      arrange(station, datetime) %>%
      mutate(QC3 = abs(Temp- lag(Temp, n = 1))) %>%
      mutate(sdev_th = isolate(input$nsdev) * runSD(x = Temp, n = isolate(input$pasthours))) %>%
      mutate(Flag_QC3 = ifelse(QC3 > sdev_th | QC3 >5, "Y", "N")) %>%
      ungroup()
    
    # Remove a few columns
    delta_q3 <- delta_q3_a %>%
      select(-c(sdev_th, QC3))
    
    # Create a dataframe of all the "delete" rows
    delta_q3_b <- delta_q3 %>%
      filter(Flag_QC3 == "Y")
    
    # # QC4: Range for acceptable temperatures ###########################################################
    delta_q4 <- delta_q3%>%
      mutate(Flag_QC4 = ifelse(Temp<isolate(input$temprange[1]) | Temp>isolate(input$temprange[2]), "Y", "N"))
    
    # # Create a dataframe of all the "delete" rows
    delta_q4_b <- delta_q4 %>%
      filter(Flag_QC4 == "Y")
    
  
    # Outlier 1: Calculate median, MAD, lower and upper ranges for each day ################################
    delta_q4$week <- week(delta_q4$date)
    delta_q4$yrWk <- paste0(delta_q4$year, "-", delta_q4$week)
    
    delta_q5_a <- delta_q4 %>%
      group_by(station, yrWk) %>%
      summarize(Q1 = quantile(Temp, probs = 0.25),
                Q3 = quantile(Temp, probs = 0.75),
                IQR = Q3-Q1,
                ul = Q3 + 1.5 * IQR,
                ll = Q1 - 1.5 * IQR)
    
    # Add calculated daily median, ranges to original table
    delta_q5_b <- left_join(delta_q4, delta_q5_a, by = c("station", "yrWk"))
    
    # Filter temperatures outside of ul and ll
    delta_q5 <- delta_q5_b %>%
      filter(Temp>ll & Temp<ul)
    
    # Deleted values
    delta_q5_c <- delta_q4 %>%
      anti_join(delta_q5)
    
    # # Calculate number of deleted rows from all QC
    deleted_total <- nrow(delta_q1_b) + nrow(delta_q2_b) + nrow(delta_q3_b) + nrow(delta_q4_b) 
    
    # Plot the data ########################################################################################

    # Each displays the deleted data in a different color, along with the "cleaned" data. 
    # Annotation custom displays the deleted rows in the center of the plot
    ggplot() +
      geom_point(data = delta_q5, aes(datetime, Temp), col = "lightsteelblue3") +
      geom_point(data = delta_q1_b, aes(datetime, Temp), color = "goldenrod2", size = 2) +
      geom_point(data = delta_q2_b, aes(datetime, Temp), color = "indianred3", size = 2) +
      geom_point(data = delta_q3_b, aes(datetime, Temp), color = "springgreen4", size = 2) +
      geom_point(data = delta_q4_b, aes(datetime, Temp), color = "cyan3", size = 2) +
      #geom_point(data = delta_q5_c, aes(date, Temp), color = "black", size = 2) +
      #coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      annotation_custom(grob = grid::textGrob(paste(deleted_total, "deleted values")),
                        xmin = -Inf, xmax = Inf, ymin = max(delta_sta$Temp), ymax = max(delta_sta$Temp)) +
      scale_x_datetime() +
      theme_bw() +
      theme(axis.title = element_text(size = 16),
            axis.text = element_text(size = 16),
            axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
  
  ## Plot2: PostQC Final ----------------------------------------------------------------
  # ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$postQC_F <- renderPlot({
    # This will initiate the submit button
    input$submit 
    
    # Filter station and dates from input ###########################################
    delta_sta <- delta_H %>%
      filter(station == isolate(input$station)) %>%
      filter(date >= isolate(input$daterange[1]) & date <= isolate(input$daterange[2]))
    
    
    # QC1: Remove rows with x number of missing values ########################################
    delta_q1_a <- delta_sta %>%
      group_by(station, date) %>%
      arrange(station, date, hour) %>%
      summarise(total = length(date)) %>%
      mutate(Flag_Q1 = ifelse(total<(24-isolate(input$missvals)), "Y", "N")) 
    
    # Dataset with deletes removed
    delta_q1 <- delta_sta %>%
      left_join(delta_q1_a) %>%
      filter(Flag_Q1 == "N")
    
    # Create a dataframe of all the "delete" rows
    delta_q1_b <- delta_sta %>%
      left_join(delta_q1_a) %>%
      filter(Flag_Q1 == "Y")
    
    # QC2: Remove rows with x number of repeating values #############################################
    delta_q1$Temp <- as.numeric(delta_q1$Temp)
    
    # Run function repeating values.
    # Tally up "same" columns (same_total)
    # Create column "delete" - if same_total = x, delete = Y)
    delta_q2_a <- repeating_values(df = delta_q1, x = isolate((input$repeatvals)))%>%
      mutate(same_total = as.numeric(rowSums(select(., contains("same"))))) %>%
      mutate(Flag_Q2 = ifelse(same_total == isolate((input$repeatvals)),"Y", "N"))
    
    # Create a dataframe of all the "delete" rows
    delta_q2_b <- delta_q2_a %>%
      filter(Flag_Q2 == "Y")
    #
    # # Join original dataframe with "delete" dataframe based on values NOT in common.
    # # based on station and date
    # # This does not only remove rows in "delete" table, but all data that match those dates.
    delta_q2 <- delta_q1 %>%
      anti_join(delta_q2_b,by = c("station", "date"))
    
    # QC3: Filter for rate of change #############################################
    
    delta_q3_a <- delta_q2 %>%
      group_by(station) %>%
      arrange(station, datetime) %>%
      mutate(Q3 = abs(Temp- lag(Temp, n = 1))) %>%
      mutate(sdev_th = isolate(input$nsdev) * runSD(x = Temp, n = isolate(input$pasthours))) %>%
      mutate(Flag_Q3 = ifelse(Q3 > sdev_th | Q3 >5, "Y", "N"))
    
    # Create a dataframe of all the "delete" rows
    delta_q3_b <- delta_q3_a %>%
      filter(Flag_Q3 == "Y")
    
    delta_q3 <- delta_q2 %>%
      anti_join(delta_q3_b, by = c("station", "date")) # remove the whole day if a rate of change is detected?
    
    # # QC4: Range for acceptable temperatures ###########################################################
    delta_q4 <- delta_q3%>%
      filter(Temp>isolate(input$temprange[1]) & Temp<isolate(input$temprange[2]))
    
    # # Create a dataframe of all the "delete" rows
    delta_q4_b <- delta_q3 %>%
      anti_join(delta_q4, by = c("station", "date"))
    
    
    
    # Outlier 1: Calculate median, MAD, lower and upper ranges for each day ################################
    delta_q4$week <- week(delta_q4$date)
    delta_q4$yrWk <- paste0(delta_q4$year, "-", delta_q4$week)
    
    delta_q5_a <- delta_q4 %>%
      group_by(station, yrWk) %>%
      summarize(Q1 = quantile(Temp, probs = 0.25),
                Q3 = quantile(Temp, probs = 0.75),
                IQR = Q3-Q1,
                ul = Q3 + 1.5 * IQR,
                ll = Q1 - 1.5 * IQR)
    
    # Add calculated daily median, ranges to original table
    delta_q5_b <- left_join(delta_q4, delta_q5_a, by = c("station", "yrWk"))
    
    # Filter temperatures outside of ul and ll
    delta_q5 <- delta_q5_b %>%
      filter(Temp>ll & Temp<ul)
    
    # Deleted values
    delta_q5_c <- delta_q4 %>%
      anti_join(delta_q5)
    
    
    # Plot the data ########################################################################################
    delta_q5$date <- as.Date(delta_q5$date, format = "%Y-%m-%d")
    
    ggplot() +
      geom_point(data = delta_q5, aes(datetime, Temp), col = "navyblue") +
      #coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      scale_x_datetime() +
      theme_bw() +
      theme(axis.title = element_text(size = 16),
            axis.text = element_text(size = 16),
            axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

