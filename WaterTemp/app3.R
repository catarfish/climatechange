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
  
  ###################################################################
  ## Plot 1: PreQC ------------------------------------------------------------ 
  ##################################################################
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$preQC <- renderPlot({
    # This will initiate the submit button
    input$submit 
    
    ### Filter station and dates from input #############################################
    temp_sta <- temp_H %>%
      filter(station == isolate(input$station)) %>%
      filter(date >= input$daterange[1] & date <= input$daterange[2])
    
    ### QC1: Remove rows with x number of missing values ########################################
    temp_q1_a <- temp_sta %>%
      group_by(station, date) %>%
      arrange(station, date, hour) %>%
      summarise(total = length(date)) %>%
      mutate(Flag_QC1 = ifelse(total<(24-isolate(input$missvals)), "Y", "N")) 
    
    # Dataset with flags
    temp_q1 <- temp_sta %>%
      left_join(temp_q1_a, by = c("station", "date")) 
    
    # Create a dataframe of all the "delete" rows
    temp_q1_b <- temp_q1 %>%
      filter(Flag_QC1 == "Y")
    
    ### QC2: Remove rows with x number of repeating values #############################################
    temp_q1$Temp <- as.numeric(temp_q1$Temp)
    
    # Run function repeating values
    temp_q2 <- repeating_vals(df = temp_q1, x = isolate((input$repeatvals)))%>%
      #select(-flag, -issame, -same) %>%
      rename(Flag_QC2 = Flag_repeats)
    
    # Create a dataframe of all the flagged rows
    temp_q2_b <- temp_q2 %>%
      filter(Flag_QC2 == "Y")
    
    ### QC3: Filter for rate of change #############################################
    
    # Calculate difference between temp and the previous temp record
    # Calculate sd threshold based on user input (# sdev, # hours)
    # If temp diff > sdev threshold or temp diff > 5, flag Y; otherwise flag N
    # Replace NAs with N
    temp_q3_a <- temp_q2 %>%
      group_by(station) %>%
      arrange(station, datetime) %>%
      mutate(QC3 = abs(Temp- lag(Temp, n = 1, default = 0)))%>%
      mutate(sdev_th = isolate(input$nsdev) * runSD(x = Temp, n = isolate(input$pasthours))) %>%
      mutate(Flag_sdev = ifelse((QC3 > sdev_th | QC3 > 5), "Y", "N"))  %>%
      mutate(Flag_sdev = replace(Flag_sdev, is.na(Flag_sdev), "N")) %>%
      ungroup()
    
    # This would address situations in which there is a single point that is influencing
    # sd calculation for later values - redo calculations based on unflagged data. Will need to edit
    # variable names 
    
    # temp_q3_a2 <- temp_q3_a %>%
    #   filter(Flag_QC3a == "N") %>%
    #   group_by(station) %>%
    #   arrange(station, datetime) %>%
    #   mutate(QC3 = abs(Temp- lag(Temp, n = 1, default = 0)))%>%
    #   mutate(sdev_th = isolate(input$nsdev) * runSD(x = Temp, n = isolate(input$pasthours))) %>%
    #   mutate(Flag_sdev = ifelse((QC3 > sdev_th | QC3 > 5), "Y", "N"))  %>%
    #   mutate(Flag_sdev = replace(Flag_sdev, is.na(Flag_sdev), "N")) %>%
    #   ungroup()
    
    # Remove unnecessary variables
    temp_q3_c <- temp_q3_a %>%
      select(-c(QC3))
    
    # Cumsum every time there is a flagged value to separate data into different groups
    # each time there is a spike
    temp_q3_d = temp_q3_c %>%
      mutate(Flag_spike = cumsum(Flag_sdev == "Y"))
    
    # Compare groups of values with each other 
    # Compare first temp of a group with last temp of previous group (t-1), (t-2)
    # Compare last temp of a group with first temp of subsequent group (t+1)
    # If all of these temp diffs are > sdev_th AND temp diff (t-1) >2 ||| OR temp diff(t-1) >5, flag data.
    # The clause of temp diff (t-1) > 2 helps eliminate much unnecessarily flagged data
    temp_q3_e <- temp_q3_d %>%
      group_by(station, Flag_spike) %>%
      summarize(tempi = first(Temp),
                tempf = last(Temp),
                sdev_thlast = round(last(sdev_th),3),
                n = n()) %>%
      mutate(tempfprev = lag(tempf, 1),
             tempfprev2 = lag(tempf, 2),
             tempinext = lead(tempi, 1),
             temp_diff1 = abs(tempi - tempfprev),
             temp_diff2 = abs(tempi - tempfprev2),
             temp_diffnext = abs(tempinext-tempf),
             sdev_thprev = lag(sdev_thlast, 1),
             sdev_thprev2 = lag(sdev_thlast,2)) %>%
      mutate(Flag_QC3 = ifelse((temp_diff1 > sdev_thprev & temp_diff2 > sdev_thprev2 & 
                                  temp_diffnext > sdev_thlast & temp_diff1>2 & temp_diffnext >2) | temp_diff1 > 5, "Y", "N" ))
    
    # Merge summarized calculations from e back in with rest of the data
    temp_q3 <- left_join(temp_q3_d, temp_q3_e, by = c("station", "Flag_spike"))%>%
      select(-c(tempi:sdev_thprev2)) 
    
    ## Only needed if doing the temp_q3_a_2 flag
    #temp_q3_f_merge <- temp_q3_f %>%select(station, datetime, Flag_spike, Flag_QC3)
    
    #temp_q3 <- left_join(temp_q3_a, temp_q3_f_merge) #%>%
    # mutate(Flag_QC3 = replace(Flag_QC3, Flag_QC3a == "Y", "Y"))
    
    # Flagged values
    temp_q3_b <- temp_q3 %>%
      filter(Flag_QC3 == "Y")
    
    
    # # QC4: Filter for rate of change #############################################
    # 
    # temp_q4_a <- temp_q3 %>%
    #   group_by(station) %>%
    #   arrange(station, datetime) %>%
    #   mutate(QC4 = abs(Temp- lag(Temp, n = 1))) %>%
    #   mutate(sdev_th = isolate(input$nsdev) * runSD(x = Temp, n = isolate(input$pasthours))) %>%
    #   mutate(Flag_QC3 = ifelse(QC3 > sdev_th | QC3 >5, "Y", "N")) %>%
    #   ungroup()
    # 
    # # Remove a few columns
    # temp_q3 <- temp_q3_a %>%
    #   select(-c(sdev_th, QC3))
    # 
    # # Create a dataframe of all the "delete" rows
    # temp_q3_b <- temp_q3 %>%
    #   filter(Flag_QC3 == "Y")
    # 
    
    
    # # QC4: Range for acceptable temperatures ###########################################################
    temp_q4 <- temp_q3%>%
      mutate(Flag_QC4 = ifelse(Temp<isolate(input$temprange[1]) | Temp>isolate(input$temprange[2]), "Y", "N"))
    
    # # Create a dataframe of all the "delete" rows
    temp_q4_b <- temp_q4 %>%
      filter(Flag_QC4 == "Y")
    
    # # Calculate number of deleted rows from all QC
    deleted_total <- nrow(temp_q1_b) + nrow(temp_q2_b) + nrow(temp_q3_b) + nrow(temp_q4_b) 
    
    # This little if section somehow makes the time datetime plot. Without this it breaks!
    if (!is.null(ranges$x)) {
      ranges$x <- as.POSIXct(ranges$x, origin = "1970-01-01")
    }
    
    # Plot data
    ggplot() +
      geom_point(data = temp_q4, aes(datetime, Temp), col = "lightsteelblue3") +
      geom_point(data = temp_q1_b, aes(datetime, Temp), color = "goldenrod2", size = 2) +
      geom_point(data = temp_q2_b, aes(datetime, Temp), color = "indianred3", size = 2) +
      geom_point(data = temp_q3_b, aes(datetime, Temp), color = "springgreen4", size = 2) +
      geom_point(data = temp_q4_b, aes(datetime, Temp), color = "cyan3", size = 2) +
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
  ## Plot2: PostQC Final ----------------------------------------------------------------
  ##############################################################################
  # ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$postQC_F <- renderPlot({
    # This will initiate the submit button
    input$submit 
    
    # Filter station and dates from input ###########################################
    temp_sta <- temp_H %>%
      filter(station == isolate(input$station)) %>%
      filter(date >= isolate(input$daterange[1]) & date <= isolate(input$daterange[2]))
    
    
    ### QC1: Remove rows with x number of missing values ########################################
    temp_q1_a <- temp_sta %>%
      group_by(station, date) %>%
      arrange(station, date, hour) %>%
      summarise(total = length(date)) %>%
      mutate(Flag_QC1 = ifelse(total<(24-isolate(input$missvals)), "Y", "N")) 
    
    # Dataset with flags
    temp_q1 <- temp_sta %>%
      left_join(temp_q1_a, by = c("station", "date")) 
    
    # Create a dataframe of all the "delete" rows
    temp_q1_b <- temp_q1 %>%
      filter(Flag_QC1 == "Y")
    
    ### QC2: Remove rows with x number of repeating values #############################################
    temp_q1$Temp <- as.numeric(temp_q1$Temp)
    
    # Run function repeating values
    temp_q2 <- repeating_vals(df = temp_q1, x = isolate((input$repeatvals)))%>%
      #select(-flag, -issame, -same) %>%
      rename(Flag_QC2 = Flag_repeats)
    
    # Create a dataframe of all the flagged rows
    temp_q2_b <- temp_q2 %>%
      filter(Flag_QC2 == "Y")
    
    ### QC3: Filter for rate of change #############################################
    
    # Calculate difference between temp and the previous temp record
    # Calculate sd threshold based on user input (# sdev, # hours)
    # If temp diff > sdev threshold or temp diff > 5, flag Y; otherwise flag N
    # Replace NAs with N
    temp_q3_a <- temp_q2 %>%
      group_by(station) %>%
      arrange(station, datetime) %>%
      mutate(QC3 = abs(Temp- lag(Temp, n = 1, default = 0)))%>%
      mutate(sdev_th = isolate(input$nsdev) * runSD(x = Temp, n = isolate(input$pasthours))) %>%
      mutate(Flag_sdev = ifelse((QC3 > sdev_th | QC3 > 5), "Y", "N"))  %>%
      mutate(Flag_sdev = replace(Flag_sdev, is.na(Flag_sdev), "N")) %>%
      ungroup()
    
    # This would address situations in which there is a single point that is influencing
    # sd calculation for later values - redo calculations based on unflagged data. Will need to edit
    # variable names 
    
    # temp_q3_a2 <- temp_q3_a %>%
    #   filter(Flag_QC3a == "N") %>%
    #   group_by(station) %>%
    #   arrange(station, datetime) %>%
    #   mutate(QC3 = abs(Temp- lag(Temp, n = 1, default = 0)))%>%
    #   mutate(sdev_th = isolate(input$nsdev) * runSD(x = Temp, n = isolate(input$pasthours))) %>%
    #   mutate(Flag_sdev = ifelse((QC3 > sdev_th | QC3 > 5), "Y", "N"))  %>%
    #   mutate(Flag_sdev = replace(Flag_sdev, is.na(Flag_sdev), "N")) %>%
    #   ungroup()
    
    # Remove unnecessary variables
    temp_q3_c <- temp_q3_a %>%
      select(-c(QC3))
    
    # Cumsum every time there is a flagged value to separate data into different groups
    # each time there is a spike
    temp_q3_d = temp_q3_c %>%
      mutate(Flag_spike = cumsum(Flag_sdev == "Y"))
    
    # Compare groups of values with each other 
    # Compare first temp of a group with last temp of previous group (t-1), (t-2)
    # Compare last temp of a group with first temp of subsequent group (t+1)
    # If all of these temp diffs are > sdev_th AND temp diff (t-1) >2 ||| OR temp diff(t-1) >5, flag data.
    # The clause of temp diff (t-1) > 2 helps eliminate much unnecessarily flagged data
    temp_q3_e <- temp_q3_d %>%
      group_by(station, Flag_spike) %>%
      summarize(tempi = first(Temp),
                tempf = last(Temp),
                sdev_thlast = round(last(sdev_th),3),
                n = n()) %>%
      mutate(tempfprev = lag(tempf, 1),
             tempfprev2 = lag(tempf, 2),
             tempinext = lead(tempi, 1),
             temp_diff1 = abs(tempi - tempfprev),
             temp_diff2 = abs(tempi - tempfprev2),
             temp_diffnext = abs(tempinext-tempf),
             sdev_thprev = lag(sdev_thlast, 1),
             sdev_thprev2 = lag(sdev_thlast,2)) %>%
      mutate(Flag_QC3 = ifelse((temp_diff1 > sdev_thprev & temp_diff2 > sdev_thprev2 & 
                                  temp_diffnext > sdev_thlast & temp_diff1>2 & temp_diffnext >2) | temp_diff1 > 5, "Y", "N" ))
    
    # Merge summarized calculations from e back in with rest of the data
    temp_q3 <- left_join(temp_q3_d, temp_q3_e, by = c("station", "Flag_spike"))%>%
      select(-c(tempi:sdev_thprev2)) 
    
    ## Only needed if doing the temp_q3_a_2 flag
    #temp_q3_f_merge <- temp_q3_f %>%select(station, datetime, Flag_spike, Flag_QC3)
    
    #temp_q3 <- left_join(temp_q3_a, temp_q3_f_merge) #%>%
    # mutate(Flag_QC3 = replace(Flag_QC3, Flag_QC3a == "Y", "Y"))
    
    # Flagged values
    temp_q3_b <- temp_q3 %>%
      filter(Flag_QC3 == "Y") 
    
    
    
    
    # # QC4: Filter for rate of change #############################################
    # 
    # temp_q4_a <- temp_q3 %>%
    #   group_by(station) %>%
    #   arrange(station, datetime) %>%
    #   mutate(QC4 = abs(Temp- lag(Temp, n = 1))) %>%
    #   mutate(sdev_th = isolate(input$nsdev) * runSD(x = Temp, n = isolate(input$pasthours))) %>%
    #   mutate(Flag_QC3 = ifelse(QC3 > sdev_th | QC3 >5, "Y", "N")) %>%
    #   ungroup()
    # 
    # # Remove a few columns
    # temp_q3 <- temp_q3_a %>%
    #   select(-c(sdev_th, QC3))
    # 
    # # Create a dataframe of all the "delete" rows
    # temp_q3_b <- temp_q3 %>%
    #   filter(Flag_QC3 == "Y")
    # 
    
    # # QC4: Range for acceptable temperatures ###########################################################
    temp_q4 <- temp_q3%>%
      mutate(Flag_QC4 = ifelse(Temp<isolate(input$temprange[1]) | Temp>isolate(input$temprange[2]), "Y", "N"))
    
    ############################ TEMPORARY ########################
    temp_q4_mod <- temp_q4 %>%
      filter(Flag_QC4 == "N")
    ###############################################################
    
    # # Create a dataframe of all the "delete" rows
    temp_q4_b <- temp_q4 %>%
      filter(Flag_QC4 == "Y")
    
    
    
    ####################### EDITED ##############################################
    temp_q3_plot <- left_join(temp_q3_b, temp_q4) %>%
      filter(Flag_QC4 == "N")
    ###############################################################
    
    # Plot the data ########################################################################################
    # Each displays the deleted data in a different color, along with the "cleaned" data. 
    # Annotation custom displays the deleted rows in the center of the plot
    ggplot() +
      geom_point(data = temp_q4_mod, aes(datetime, Temp), col = "lightsteelblue3") +
      geom_point(data = temp_q1_b, aes(datetime, Temp), color = "goldenrod2", size = 2) +
      geom_point(data = temp_q2_b, aes(datetime, Temp), color = "indianred3", size = 2) +
      geom_point(data = temp_q3_plot, aes(datetime, Temp), color = "springgreen4", size = 2) +
      #coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      #annotation_custom(grob = grid::textGrob(paste(deleted_total, "deleted values")),
      #                  xmin = -Inf, xmax = Inf, ymin = max(temp_sta$Temp), ymax = max(temp_sta$Temp)) +
      scale_x_datetime() + #ylim(0,30) +
      theme_bw() +
      theme(axis.title = element_text(size = 16),
            axis.text = element_text(size = 16),
            axis.text.x = element_text(angle = 90, hjust = 1))
    
    ############################ Add this back in later ###########################################
    # ## Version that does not include data cut
    # temp_qc <- temp_q4 %>%
    #   filter(Flag_QC1 == "N") %>%
    #   filter(Flag_QC2 == "N") %>%
    #   filter(Flag_QC3 == "N") %>% 
    #   filter(Flag_QC4 == "N")
    # 
    # 
    # # Plot the data ###############################################################
    # temp_q4$date <- as.Date(temp_q4$date, format = "%Y-%m-%d")
    # 
    # ggplot() +
    #   geom_point(data = temp_qc, aes(datetime, Temp), col = "navyblue") +
    #   #coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
    #   scale_x_datetime() +
    #   theme_bw() +
    #   theme(axis.title = element_text(size = 16),
    #         axis.text = element_text(size = 16),
    #         axis.text.x = element_text(angle = 90, hjust = 1))
    ######################################################################################################
  })
  
  
  
  ###############################################################
  ## Table ---------------------------------------------------------------- 
  ################################################################
  
  output$vals_removed <- renderTable({
    # This will initiate the submit button
    input$submit 
    
    # Filter station and dates from input ###########################################
    temp_sta <- temp_H %>%
      filter(station == isolate(input$station)) %>%
      filter(date >= isolate(input$daterange[1]) & date <= isolate(input$daterange[2]))
    
    
    # QC1: Remove rows with x number of missing values ########################################
    temp_q1_a <- temp_sta %>%
      group_by(station, date) %>%
      arrange(station, date, hour) %>%
      summarise(total = length(date)) %>%
      mutate(Flag_QC1 = ifelse(total<(24-isolate(input$missvals)), "Y", "N")) 
    
    # Dataset with flags
    temp_q1 <- temp_sta %>%
      left_join(temp_q1_a, by = c("station", "date")) 
    
    # Create a dataframe of all the "delete" rows
    temp_q1_b <- temp_q1 %>%
      filter(Flag_QC1 == "Y")
    
    # QC2: Remove rows with x number of repeating values #############################################
    temp_q1$Temp <- as.numeric(temp_q1$Temp)
    
    # Run function repeating values.
    # Tally up "same" columns (same_total)
    # Create column "delete" - if same_total = x, delete = Y)
    # This does not delete the whole day's data - just when the values start repeating 
    # temp_q2_a <- repeating_values(df = temp_q1, x = isolate((input$repeatvals)))%>%
    #   mutate(same_total = as.numeric(rowSums(select(., contains("same"))))) %>%
    #   mutate(Flag_QC2_a = ifelse(same_total == isolate((input$repeatvals)),"Y", "N")) %>%
    #   select(-flag, -issame, -same)
    
    temp_q2 <- repeating_vals(df = temp_q1, x = isolate((input$repeatvals)))%>%
      #select(-flag, -issame, -same) %>%
      rename(Flag_QC2 = Flag_repeats)
    
    # Apply another flag to delete the whole day
    # temp_q2 <- temp_q2_a %>%
    #   group_by(station, date)%>%
    #   mutate(Flag_QC2_b = ifelse("Y" %in% Flag_QC2_a, "Y", "N")) %>%
    #   select(-c(Flag_QC2_a))
    
    # Create a dataframe of all the flagged rows
    temp_q2_b <- temp_q2 %>%
      filter(Flag_QC2 == "Y")
    
    
    # QC3: Filter for rate of change #############################################
    
    temp_q3_a <- temp_q2 %>%
      group_by(station) %>%
      arrange(station, datetime) %>%
      mutate(QC3 = abs(Temp- lag(Temp, n = 1))) %>%
      mutate(sdev_th = isolate(input$nsdev) * runSD(x = Temp, n = isolate(input$pasthours))) %>%
      mutate(Flag_QC3 = ifelse(QC3 > sdev_th | QC3 >5, "Y", "N")) %>%
      ungroup()
    
    # Remove a few columns
    temp_q3 <- temp_q3_a %>%
      select(-c(sdev_th, QC3))
    
    # # QC4: Range for acceptable temperatures ###########################################################
    temp_q4 <- temp_q3%>%
      mutate(Flag_QC4 = ifelse(Temp<isolate(input$temprange[1]) | Temp>isolate(input$temprange[2]), "Y", "N"))
    
    vals_removed <- temp_q4 %>%
      group_by(station) %>%
      summarize(Init = n(),
                QC1 = sum(Flag_QC1=="Y"),
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

