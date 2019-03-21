#this code makes it easy to plot muliple years worth of environmental data on one plot

#SAMPLE CODE:
get.CDEC('SJR', "E", 25, "2017-01-01", "2018-12-31") #download data for both years


cdec.dat %>% 
  select(-date) %>%                                                           #remove problem column
  filter(sensor_25 <= 7500 & !is.na(datetime)) %>%                            #filter date = na, and absurd values
  mutate(dummydate = ymd(paste(2017, month(datetime), day(datetime), sep = "-")),  
         year = year(datetime)) %>%                                           #pull out year for grouping variable, and make 
  #new 'dummydate column replacing with all the same years
  group_by(dummydate, year) %>%                                               #group by dummydate and year
  summarize(mdt = mean(sensor_25)) %>%                                        #calculate mean daily temp
  filter(dummydate >= "2017-03-01" & dummydate <= "2017-05-31") %>%  
  ggplot() + 
  geom_line(aes(dummydate, ((mdt-32)*(5/9)), color  = as.factor(year)),       #convert temp(F) to temp(C)
            size = 1.1) + 
  scale_x_date(date_labels = "%b") + 
  scale_color_manual(name = "Water Year", values = c("navy", "aquamarine")) +
  labs(title = "San Joaquin River Temperature at Vernalis", y = expression(paste( "Mean Daily Temperature (", ~degree~C, ")")), 
       x = "Date") +
  theme_bw() + 
  theme(text = element_text(size=16))


#######################For plots:
#Flow, Delta:
get.CDEC('VNS', "E", 20,"2017-03-01", "2018-08-31")

pdf("delta_flow_rel_throughApril.pdf")
cdec.dat %>% 
  select(-date) %>%                                                           #remove problem column
  filter(!is.na(datetime)) %>%                                                #filter date = na, and absurd values
  mutate(dummydate = ymd(paste(2017, month(datetime), day(datetime), sep = "-")),  
         year = year(datetime)) %>%                                           #pull out year for grouping variable, and make 
  #new 'dummydate column replacing with all the same years
  group_by(dummydate, year) %>%                                               #group by dummydate and year
  summarize(mdf = mean(sensor_20)) %>%                                        #calculate mean daily flow
  filter(dummydate >= "2017-03-01" & dummydate <= "2017-04-30") %>%  
  ggplot() + 
  geom_line(aes(dummydate, mdf, color  = as.factor(year)),       
            size = 1.1) + 
  #scale_x_date(date_labels = "%b") + 
  scale_color_manual(name = "Year", values = c("navy", "orange")) +
  labs(title = "San Joaquin River Discharge at Vernalis", y = "Mean Daily Flow (cfs)", 
       x = "Date") +
  theme_bw() + 
  theme(text = element_text(size=16))
dev.off()





#Flow, Upriver 
get.CDEC('FFB', "E", 20,"2017-03-01", "2018-08-31")

pdf("Upriver_FFB_flow_rel_thruApril.pdf")
cdec.dat %>% 
  select(-date) %>%                                                           #remove problem column
  filter(!is.na(datetime)) %>%                                                #filter date = na, and absurd values
  mutate(dummydate = ymd(paste(2017, month(datetime), day(datetime), sep = "-")),  
         year = year(datetime)) %>%                                           #pull out year for grouping variable, and make 
  #new 'dummydate column replacing with all the same years
  group_by(dummydate, year) %>%                                               #group by dummydate and year
  summarize(mdf = mean(sensor_20)) %>%                                        #calculate mean daily flow
  filter(dummydate >= "2017-03-01" & dummydate <= "2017-04-30") %>%  
  ggplot() + 
  geom_line(aes(dummydate, mdf, color  = as.factor(year)),       
            size = 1.1) + 
  #scale_x_date(date_labels = "%b") + 
  scale_color_manual(name = "Year", values = c("navy", "orange")) +
  labs(title = "San Joaquin River Discharge At Freemont Ford", y = "Mean Daily Flow (cfs)", 
       x = "Date") +
  theme_bw() + 
  theme(text = element_text(size=16))

dev.off()

# Upriver Flow, near 2017 Release location 
get.CDEC('SWA', "E", 20,"2017-03-01", "2018-08-31")

pdf("Upriver_SWA_flow_rel.pdf")
cdec.dat %>% 
  select(-date) %>%                                                           #remove problem column
  filter(!is.na(datetime)) %>%                                                #filter date = na, and absurd values
  mutate(dummydate = ymd(paste(2017, month(datetime), day(datetime), sep = "-")),  
         year = year(datetime)) %>%                                           #pull out year for grouping variable, and make 
  #new 'dummydate column replacing with all the same years
  group_by(dummydate, year) %>%                                               #group by dummydate and year
  summarize(mdf = mean(sensor_20)) %>%                                        #calculate mean daily flow
  filter(dummydate >= "2017-03-01" & dummydate <= "2017-03-31") %>%  
  ggplot() + 
  geom_line(aes(dummydate, mdf, color  = as.factor(year)),       
            size = 1.1) + 
  #scale_x_date(date_labels = "%b") + 
  scale_color_manual(name = "Water Year", values = c("navy", "orange")) +
  labs(title = "San Joaquin River Discharge Near Harmon Road", y = "Mean Daily Flow (cfs)", 
       x = "Date") +
  theme_bw() + 
  theme(text = element_text(size=16))

dev.off()

#Temp ########

#Temp, Harmon Rd:
get.CDEC('SWA', "E", 25, "2017-01-01", "2018-12-31") #download data for both years

pdf("Upriver_SWA_Temp_thruApril.pdf")
cdec.dat %>% 
  select(-date) %>%                                                           #remove problem column
  filter(sensor_25 <= 7500 & !is.na(datetime)) %>%                            #filter date = na, and absurd values
  mutate(dummydate = ymd(paste(2017, month(datetime), day(datetime), sep = "-")),  
         year = year(datetime)) %>%                                           #pull out year for grouping variable, and make 
  #new 'dummydate column replacing with all the same years
  group_by(dummydate, year) %>%                                               #group by dummydate and year
  summarize(mdt = mean(sensor_25)) %>%                                        #calculate mean daily temp
  filter(dummydate >= "2017-03-01" & dummydate <= "2017-04-30") %>%  
  ggplot() + 
  geom_line(aes(dummydate, ((mdt-32)*(5/9)), color  = as.factor(year)),       #convert temp(F) to temp(C)
            size = 1.1) + 
  #scale_x_date(date_labels = "%b") + 
  scale_color_manual(name = "Year", values = c("navy", "orange")) +
  labs(title = "San Joaquin River Temperature near Harmon Rd", y = expression(paste("Mean Daily Temperature (", ~degree~C, ")")), 
       x = "Date") +
  theme_bw() + 
  theme(text = element_text(size=16))

dev.off()



#Upriver Freemont Ford, Temp
get.CDEC('FFB', "E", 25, "2017-01-01", "2018-12-31") #download data for both years

pdf("Upriver_FFB_Temp_thruApril.pdf")
cdec.dat %>% 
  select(-date) %>%                                                           #remove problem column
  filter(sensor_25 <= 7500 & !is.na(datetime)) %>%                            #filter date = na, and absurd values
  mutate(dummydate = ymd(paste(2017, month(datetime), day(datetime), sep = "-")),  
         year = year(datetime)) %>%                                           #pull out year for grouping variable, and make 
  #new 'dummydate column replacing with all the same years
  group_by(dummydate, year) %>%                                               #group by dummydate and year
  summarize(mdt = mean(sensor_25)) %>%                                        #calculate mean daily temp
  filter(dummydate >= "2017-03-01" & dummydate <= "2017-04-30") %>%  
  ggplot() + 
  geom_line(aes(dummydate, ((mdt-32)*(5/9)), color  = as.factor(year)),       #convert temp(F) to temp(C)
            size = 1.1) + 
  #scale_x_date(date_labels = "%b") + 
  scale_color_manual(name = "Year", values = c("navy", "orange")) +
  labs(title = "San Joaquin River Temperature at Freemont Ford", y = expression(paste("Mean Daily Temperature (", ~degree~C, ")")), 
       x = "Date") +
  theme_bw() + 
  theme(text = element_text(size=16))

dev.off()


#Temp, Delta

get.CDEC('SJR', "E", 25, "2017-01-01", "2018-12-31") #download data for both years

pdf("Delta_SJR_Temp_thruApril.pdf")
cdec.dat %>% 
  select(-date) %>%                                                           #remove problem column
  filter(sensor_25 <= 7500 & !is.na(datetime)) %>%                            #filter date = na, and absurd values
  mutate(dummydate = ymd(paste(2017, month(datetime), day(datetime), sep = "-")),  
         year = year(datetime)) %>%                                           #pull out year for grouping variable, and make 
  #new 'dummydate column replacing with all the same years
  group_by(dummydate, year) %>%                                               #group by dummydate and year
  summarize(mdt = mean(sensor_25)) %>%                                        #calculate mean daily temp
  filter(dummydate >= "2017-03-01" & dummydate <= "2017-04-30") %>%  
  ggplot() + 
  geom_line(aes(dummydate, ((mdt-32)*(5/9)), color  = as.factor(year)),       #convert temp(F) to temp(C)
            size = 1.1) + 
  #scale_x_date(date_labels = "%b") + 
  scale_color_manual(name = "Year", values = c("navy", "orange")) +
  labs(title = "San Joaquin River Temperature at Vernalis", y = expression(paste("Mean Daily Temperature (", ~degree~C, ")")), 
       x = "Date") +
  theme_bw() + 
  theme(text = element_text(size=16))

dev.off()





