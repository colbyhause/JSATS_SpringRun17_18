#Attempt to work around issue with transit code where code will only pull transit rate when a 
#fish is detected at one receiver and then immediately at the next receiver. 
#Ex. If I want to look at transit speed of fish from the Upstream Release to DF (so detected at release
#and also detected at DF). The way code is set up now it would only grab fisht that were detected at 
#release and then not detected until DF

#2017 DATA 
#FIRST: run functions from 2017TransitRate.Rmd *** THIS IS IMPORTANT

# Bring in detection data, set routes, and location csv

group1_dets_16 <- read_csv("Z:/Shared/Projects/JSATS/DSP_Spring-Run Salmon/JSATS analysis/Queries/Fish detection queries with FIXED data/071818_group1_filtered_16km_upperRel.csv", 
                           col_types = cols(RecSN = col_character(), 
                                            dtf = col_datetime(format = "%m/%d/%Y %H:%M:%S"), 
                                            tag_activation_date = col_datetime(format = "%m/%d/%Y %H:%M:%S")))
group2_dets_16 <- read_csv("Z:/Shared/Projects/JSATS/DSP_Spring-Run Salmon/JSATS analysis/Queries/Fish detection queries with FIXED data/071818_group2_filtered_16km_durhamRel.csv", 
                           col_types = cols(RecSN = col_character(), 
                                            dtf = col_datetime(format = "%m/%d/%Y %H:%M:%S"), 
                                            tag_activation_date = col_datetime(format = "%m/%d/%Y %H:%M:%S")))

group1routes_byReach <- c("2017 Upper Release Mainstem Route") #, "Upper Release Paradise Cut Route", "Upper Release Old River Route")
group2routes_byReach <- c("2017 Lower Release Mainstem Route") #, "Lower Release Paradise Cut Route", "Lower Release Old River Route")

group1_locations_byReach <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/Transit2017_UpperRel_Locations_by_Group.csv")

group2_locations_byReach<- read_csv("~/GitHub/JSATS_SpringRun17_18/data/Transit2017_LowerRel_Locations_by_Group.csv")


#change rec names

group1_dets_16<- change_receiver_names(group1_dets_16) 
group2_dets_16 <- change_receiver_names(group2_dets_16)

#Make detection file of just the groups you want:

group1_dets_16 <- subset(group1_dets_16, group1_dets_16$`GPS Names` == "Upstream_Release" |  group1_dets_16$`GPS Names` == "165" 
                         | group1_dets_16$`GPS Names` == "Hills Ferry" 
                         | group1_dets_16$`GPS Names` == "Durham Ferry" |  group1_dets_16$`GPS Names` == "BCA" 
                         | group1_dets_16$`GPS Names` == "HOR" |  group1_dets_16$`GPS Names` == "SJG"
                         | group1_dets_16$`GPS Names` == "MAC" | group1_dets_16$`GPS Names` == "Chipps"
                         | group1_dets_16$`GPS Names` == "Benicia" |  group1_dets_16$`GPS Names` == "Golden Gate")

group2_dets_16 <- subset(group2_dets_16, group2_dets_16$`GPS Names` == "Durham_Release" | group2_dets_16$`GPS Names` == "Durham Ferry" 
                         | group2_dets_16$`GPS Names` == "BCA" #| group2_dets_16$`GPS Names` == "SJ Paradise Cut" 
                         | group2_dets_16$`GPS Names` == "HOR" 
                         | group2_dets_16$`GPS Names` == "Howard" | group2_dets_16$`GPS Names` == "MAC" 
                         #| group2_dets_16$`GPS Names` == " Medford"
                         | group2_dets_16$`GPS Names` == "Chipps" | group2_dets_16$`GPS Names` == "Benicia"
                         | group2_dets_16$`GPS Names` == "Golden Gate")

#now run Transit code like normal

df_transit_group1<- get_transit_rate(group1_dets_16, 0, 500)
df_transit_group2<- get_transit_rate(group2_dets_16, 0, 500)

group1_plots <- order_route(group1routes_byReach, df_transit_group1, group1_locations_byReach, pdf_name = "2017 group1 plots by reach PDFs" )
group2_plots <- order_route(group2routes_byReach, df_transit_group2, group2_locations_byReach, pdf_name = "2017 group2 plots by reach PDFs" )



dev.off()

#2018 Data: run functions from 2018Transit_rate.Rmd ***THIS IS IMPORTANT 

# Bring in detection data, set routes, and location csv

group1_2018_dets <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/UpperRel_072018_group1_filtered_16km_2018_FINAL.csv", 
                             col_types = cols(dtf = col_datetime(format = "%m/%d/%Y %H:%M:%S")))

group2_2018_dets <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/DurhamRel_072018_group2_filtered_16km_2018_FINAL.csv", 
                             col_types = cols(dtf = col_datetime(format = "%m/%d/%Y %H:%M:%S")))


group1routes_byReach <- c("2018 Upper Release Mainstem Route") #, "Upper Release Paradise Cut Route", "Upper Release Old River Route")
group2routes_byReach <- c("2018 Lower Release Mainstem Route") #, "Lower Release Paradise Cut Route", "Lower Release Old River Route")


group1_2018_loc_byReach <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/Transit2018_UpperRel_Locations_by_Group.csv")
group2_2018_loc_byReach <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/Transit2018_LowerRel_Locations_by_Group.csv")

#Make detection file of just the groups you want:

group1_2018_dets <- subset(group1_2018_dets, group1_2018_dets$Gen_Name == "Upstream_Release" | group1_2018_dets$Gen_Name == "Blw Newman"
                           | group1_2018_dets$Gen_Name == "Hills Ferry" | group1_2018_dets$Gen_Name == "Durham Ferry" 
                           | group1_2018_dets$Gen_Name == "BCA" | group1_2018_dets$Gen_Name == "HOR"
                           | group1_2018_dets$Gen_Name == "SJG" | group1_2018_dets$Gen_Name == "MAC")




group2_2018_dets <- subset(group2_2018_dets, group2_2018_dets$Gen_Name == "Durham_Release" 
                           | group2_2018_dets$Gen_Name == "Durham Ferry" | group2_2018_dets$Gen_Name == "BCA" 
                           | group2_2018_dets$Gen_Name == "HOR" | group2_2018_dets$Gen_Name == "Howard"
                           | group2_2018_dets$Gen_Name == "MAC") # | group2_2018_dets$Gen_Name == " Medford")

#now run 2018 transit rate functions

df_transit2018_group1<- get_transit_rate(group1_2018_dets, 0, 500)
df_transit2018_group2<- get_transit_rate(group2_2018_dets, 0, 500)

group1_plots <- order_route(group1routes_byReach, df_transit2018_group1, group1_2018_loc_byReach, pdf_name = "2018 group1 plots by reach PDFs" )
group2_plots <- order_route(group2routes_byReach, df_transit2018_group2, group2_2018_loc_byReach, pdf_name = "2018 group2 plots by reach PDFs" )




