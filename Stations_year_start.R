# 10/23/2018
# Catarina Pien
# Plot showing the start dates for CDEC data gathered, 
# divided into hourly and 15-minute intervals

rm(list=ls(all=TRUE))

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

setwd("P:/ClimateChange/R_code")
start <- read_csv("Stations_year_start.csv")

str(start)

start$Event <- as.Date(start$Event, format = "%m/%d/%Y")
start$Hourly <- as.Date(start$Hourly, format = "%m/%d/%Y")
start$Six <- as.Date(start$Six, format = "%m/%d/%Y")

start$fifteenmin <- year(start$Event)
start$hourly <- year(start$Hourly)
start$sixmin <- year(start$Six)

years <- gather(start[,c(1,5,6,7)], timescale, year, -STA)

sum <- years %>%
  group_by(year, timescale)%>%
  summarize(length = n())

ggplot(sum, aes(x = year, y = length, fill = timescale)) +
  geom_col() +
  scale_fill_manual(values = c('#456ba8',"#1a2b47",'#a4c5f9'))+
  labs(x = "Station Start Date", y = "Number of stations")+
  scale_x_continuous(breaks = c(1986, 1988, 1990, 1992, 1994, 
                                1996, 1998, 2000, 2002, 2004, 
                                2006, 2008, 2010, 2012, 2014, 2016, 2018))+
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size=18, vjust = 1),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.8))
