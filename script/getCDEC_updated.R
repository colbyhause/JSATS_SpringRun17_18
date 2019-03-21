

# add Ryan Peek's cdec scraping function

#### FUNCTION TO PULL DATA FROM CDEC STATIONS
#### RYAN PEEK, 2013, 
#### CENTER FOR WATERSHED SCIENCES UC DAVIS


#### Updated for new cdec website 2018-08-28, by Gabe Singer
#must set up a data folder in you working directory inorder to save cdec file 

## Sensor Values for Lisbon:
# 20: CFS
# 25: Water temp
# 61: DO
# Others: http://cdec.water.ca.gov/misc/senslist.html

get.CDEC<-function(station,duration,sensor,start,end){
  # Function to pull CDEC data
  ### Station is 3 letter abbreviation
  ### Duration is E=event, D=Daily
  ### sensor is number
  ### format start and end as "YYYY-MM-DD"
  
  # List of Real-Time Stations: http://cdec.water.ca.gov/misc/realStations.html
  # List of Daily Stations: http://cdec.water.ca.gov/misc/dailyStations.html 
  # List of sensors:  http://cdec.water.ca.gov/misc/senslist.html
  
  # SENSORS MOST COMMONLY USED
  ### 1  stage 
  ### 2  rain accum
  ### 3  snow water content
  ### 4  air temp 
  ### 6  reservoir elevation
  ### 16 precip tippingbucket 
  ### 20 flow cfs 
  ### 25 water temp 
  ### 45 ppt incremental
  
  ## EXAMPLE URL:  
  # http://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=LIS&SensorNums=25&dur_code=E&Start=2018-05-28&End=2018-08-28
  
  data <- read.table(paste("http://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=", station,"&SensorNums=", sensor, "&dur_code=", 
                           duration,"&Start=", start, "&End=",end, sep=""), header = F, sep = ",", skip = 1)

  
  names(data) <- c("station", "duration", "sensor", "sesnor_type", "actual_date", "obs_date", "value", "data_flag", "units")
  data$value <- as.numeric(as.character(data$value))
  
  ## format date and time
    data$actual_date <- as.POSIXct(data$actual_date,format="%Y-%m-%d %H:%M") # convert to datetime from whatever format
    data$obs_date <- as.POSIXct(data$obs_date,format="%Y-%m-%d %H:%M") # convert to datetime from whatever format

  summary(data)
  str(data)
  
  #   ## ask if user wants to change the directory for save purposes    
  #   cat("\n","Use current directory? Y or N","\n\n") # prompt 
  #   y<-scan(what="character",n=1)
  #   ifelse(y=="N",setwd(choose.dir()),getwd())
  
  ## ask if user wants to save to csv or use in dataframe
  cat("\n","Write file to csv? Y or N","\n\n") # prompt 
  z<-scan(what="character",n=1)
  if(toupper(z)=="Y"){write.csv(data, file=paste("data/", station,"_sensor-",sensor,"_",start,"_to_",end,".csv",sep=""),row.names=FALSE)
    print(paste("file downloaded and saved here: ",getwd(), "/data", sep="")) # show message
  } else{
    cat("No csv written...output to dataframe only\n")}
  assign("cdec.dat",data,envir = .GlobalEnv) # print to workspace
  cat("All Finished! Available in current dataframe...\n")
}


# test (Fremont Ford)
#get.CDEC("FFB", "E", "20", "2017-03-01", "2017-05-30")