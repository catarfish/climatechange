Sys.timezone()
11:57
temps$Datetime[1]
11:57
Sys.setenv(TZ='GMT')
11:57
temps$Datetime[1]
11:58
#try reloading again just to make sure
11:58
temps = readRDS("Temp_filtered (1).rds")
tz(temps$Datetime[1])