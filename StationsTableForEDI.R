library(tidyverse)
library(lubridate)


latlons <- read.csv("WaterTemp/data/latlonsTomerge.csv")
latlons <- latlons %>% 
  select(c(1, 5:7)) %>%
  rename(Station = station)
metadata <- read.csv("CDEC_MetadataToMerge.csv")
datafile <- readRDS("TempData/QC/Temp_flagged.rds")
meta0 <- datafile %>%
  select(c(1,4)) %>%
  group_by(Station) %>%
  summarize(StartDateDataset = first(Date),
            EndDateDataset = last(Date))

# This one has info about the different data types, but decided not to put it in the metadata.
# cdectemp <- read.csv("cdec_temp_metadata_from_downloader.csv") %>%
#   rename(Station = station) %>%
#   select(c(2, 4:8)) 
#   mutate(year = year(end)) %>%
#   mutate(EndDateDataset = ifelse(year > 2019, as.Date("2019-12-31"), as.Date(end))) %>%
#   rename(StartDateDataset = start,
#          EndDateDataset = end)

meta1 <- left_join(latlons,metadata, by = "Station")
meta2 <- left_join(meta0, meta1, by = "Station") %>%
  select(c(Station, StationName, StartDateDataset, EndDateDataset, Agency, Latitude, Longitude, Hydro.Area, Basin.Name, County.Name, HabitatType)) %>%
  rename(HydrologicArea = Hydro.Area, 
         Basin = Basin.Name,
         County = County.Name) %>%
  mutate(Basin = str_to_title(Basin),
         HydrologicArea = str_to_title(HydrologicArea),
         County = str_to_title(County))

write.csv(meta2, "TempData/StationsMetadata.csv")
