# plot time series without weird workaround (kinda) -----------------------

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
