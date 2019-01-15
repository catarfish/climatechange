# 10/23/2018
# Catarina Pien
# Plot showing the agency affiliations for data collected

rm(list=ls(all=TRUE))

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

setwd("P:/ClimateChange/R_code")
affil <- read_csv("Stations_agency.csv")

sum <- affil %>%
  group_by(Organization)%>%
  summarize(length = n())

ggplot() +
  geom_col(data = sum, aes(x = as.factor(Organization), y = length), fill = '#5bc0de') +
  geom_text(data = sum, aes(x = Organization, y = length, label = length), size = 6, vjust = -0.2) +
  labs(y = "Number of stations")+
  scale_y_continuous(limits=c(0,90), breaks = c(0,10,20,30,40,50,60,70,80,90)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=18, vjust = 1),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 16))
