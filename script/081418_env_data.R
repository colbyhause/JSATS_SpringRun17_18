# Wed Aug 14 2018 ------------------------------
library(here)
library(tidyverse)

# Get and clean CDEC Data -------------------------------------------------


# Source Ryan Peek's get CDEC function ------------------------------------
source('script/get_cdec.R')


# Pull SJR data from 2017 Study Period ------------------------------------

#function: get.cdec(station,duration,sensor,start,end)

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

# 2017 VNS flow: Delta Release
get.CDEC('VNS', 'E', 20 , "2017-03-01", "2017-04-30")

ggplot(cdec.dat) + 
  geom_line(aes(datetime, sensor_20))


# 2018 VNS flow: Delta Release
get.CDEC('VNS', 'E', 20, "2018-03-01", "2018-05-31")

ggplot(cdec.dat) + 
  geom_line(aes(datetime, sensor_20))

# 2017 SWA flow: Upper Release
get.CDEC('SWA', 'E', 20, "2017-03-01", "2017-04-30")

ggplot(cdec.dat) + 
  geom_line(aes(datetime, sensor_20))

# 2018 SWA flow: Upper Release
get.CDEC('SWA', 'E', 20, "2018-03-01", "2018-04-30")

ggplot(cdec.dat) + 
  geom_line(aes(datetime, sensor_20))


flow_VNS_2017 <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/VNS_sensor-20_2017-03-01_to_2017-05-31.csv")
flow_VNS_2018 <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/VNS_sensor-20_2018-03-01_to_2018-05-31.csv")

flow_VNS_17_18 <- NULL
flow_VNS_17_18 <- rbind(flow_VNS_17_18, flow_SWA_2017, flow_VNS_2018)

write.csv(flow_VNS_17_18, file= ("data/flow_VNS_17_18.csv"))

flow_VNS_17_18_forPlot <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/flow_VNS_17_18_adjusted.csv")


ggplot(flow_VNS_17_18_adjusted_test) + 
  geom_line(aes(datetime, sensor_20, color = year, group = 1)) 


  




