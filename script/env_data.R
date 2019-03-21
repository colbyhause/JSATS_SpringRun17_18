# Wed Aug 08 12:04:08 2018 ------------------------------
library(here)
library(tidyverse)

# Get and clean CDEC Data -------------------------------------------------


# Source Ryan Peek's get CDEC function ------------------------------------
source('script/get_cdec.R')


# Pull SJR data from 2017 Study Period ------------------------------------

### Station is 3 letter abbreviation
### Duration is E=event, D=Daily
### sensor is number
### format start and end as "YYYY-MM-DD"

# List of Real-Time Stations: http://cdec.water.ca.gov/misc/realStations.html
# List of Daily Stations: http://cdec.water.ca.gov/misc/dailyStations.html 
# List of sensors:  http://cdec.water.ca.gov/misc/senslist.html

# SENSORS MOST COMMONLY USED
### 1  river stage (feet)
### 4  air temp (F)
### 6  reservoir elevation (feet)
### 20 flow (cfs)
### 25 water temp (F)
### 27 turbidity (NTU)
### 41 flow, mean daily (cfs)
### 61 DO
### 150 dissolved nitrate


#Stations to pull from and sensors available: 
#     SJR (Delta): DOC, stage, Nitrate (event), DO, water temp (event), air temp (hourly), EC, turbidity, 
#                 solar radiation, pH, Chlorophyll, wind ***no flow
#     VER (co-located with SJR, run byu USBR): EC, water temp (hourly and daily)
#     VNS (Delta, just upstream from SJR & VER): flow CFS (hourly, mean daily, and event), stage, water temp (C)
#     ELN (Eastside Bypass near El Nido Rd): stage (event and hour), flow (mean daily), flow cfs (hour and event) 
#     SWA (SJR near Washington Rd): water temp (D), pH (E), EC (D), turb (D), pH (D & E), DO (E), chlorophyll (D)

#SJR temp
get.CDEC('SJR', "E", 25, "2017-03-01", "2017-05-31")
#SJR DO
get.CDEC('SJR', "E", 61, "2017-03-01", "2017-05-31")
#SJR turb
get.CDEC('SJR', "E", 27, "2017-03-01", "2017-05-31")

#VNS flow
get.CDEC('VNS', "E", 20, "2017-03-01", "2017-05-31")


#ELN flow
get.CDEC('ELN', "E", 20, "2017-03-01", "2017-05-31")

#SWA temp
get.CDEC('SWA', "E", 25, "2017-03-01", "2017-05-31")
#SWA flow
get.CDEC('SWA', "E", 20, "2017-03-01", "2017-05-31")


# SWA turb
get.CDEC('SWA', "D", 27, "2017-03-01", "2017-05-31")

# clean and convert flows from CFS to cubic meters per second
source("script/clean_flow.R")
clean.cdec.flow(file.path = "data/VNS_sensor-20_2017-03-01_to_2017-05-31.csv", file.name = 'VNS_flow')



# Temp in 2018 ------------------------------------------------------------
#SJR temp
get.CDEC('SJR', "E", 25, "2018-03-01", "2018-05-31") #spike in temp above optimal temp at the end of april (Myrick and Cech 2000)
get.CDEC('SWA', "E", 25, "2018-03-01", "2018-05-31") #above optimal temp nearly all of April
get.CDEC('SMN', "E", 146, "2018-03-01", "2018-05-31") #sensor 146 is temp in C; spike in mid-march above optimal temp, and spike at end of march/beginning of april

ggplot(cdec.dat) + 
  geom_line(aes(datetime, sensor_25)) + 
  geom_hline(yintercept = 63.5) +
  geom_hline(yintercept = 50)

ggplot(cdec.dat) + 
  geom_line(aes(datetime, sensor_146)) + 
  geom_hline(yintercept = 10) +
  geom_hline(yintercept = 17.5)
  

ggplot(cdec.dat) + 
  geom_line(aes(datetime, sensor_27))
