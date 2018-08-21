# Aug 14 2018
#Updated: Aug 21 This script will produce initial movement plots, plotting 2018 and 2017 Upper relase on one gaph, and 2017 and 2018 lower release on another
#you should be able to just click through all the code 



library(lubridate)
library(ggridges)
library(ggplot2)

####### Group 1 initial movement 2017 & 2018#####################

group1_2017 <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/071818_group1_filtered_16km_2017.csv", 
                        col_types = cols(dtf = col_datetime(format = "%m/%d/%Y %H:%M:%S")))

group1_2018 <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/072018_group1_filtered_16km_2018_FINAL.csv", 
                        col_types = cols(dtf = col_datetime(format = "%m/%d/%Y %H:%M:%S")))

# truncate detections at 290 (just above 165) bc it looks like fish are hanging in ESB, so lots of early dets there
#we can also tell fish are just hanging in ESB bc when we truncate to just above 165, we lose about 100,000 detections
group1_2017_truncated <- group1_2017 %>%  
  filter(Genrkm <= 290)


#group1 2017: make df of initial movement 
alltags<- unique(group1_2017_truncated$Hex)
print(alltags)
df_det <- NULL

for(tag in alltags) {
  ind_tag <- subset(group1_2017_truncated, group1_2017_truncated$Hex == tag)
  min_det <- min(ind_tag$dtf)
  print(min_det)
  tag_id <- tag
  df_det <- rbind(df_det, data.frame(min_det, tag_id))
}
#make a column of themin dets called rounded_det, set to a date 
df_det$rounded_det <- as.Date(df_det$min_det)
class(df_det$rounded_det)

#add proportion of fish moving calculation
data_g1_2017 <- df_det %>% 
  group_by(rounded_det) %>% 
  tally() %>% 
  mutate(proportion = n/(sum(n)))

#add column for group
data_g1_2017$group <- "2017 Upper Release"

#make dummy date column
data_g1_2017 <-  data_g1_2017 %>% 
  mutate(dummydate = ymd(paste(2017, month(rounded_det), day(rounded_det), sep = "-")))



# group1 2018: make df of initial movement 
group1_2018 <- group1_2018 %>%   # truncate detections just above Newman to test and make sure fish arrive early to this site too 
  filter(Genrkm <= 265)

df_det <- NULL

alltags<- unique(group1_2018$Hex)
print(alltags)
for(tag in alltags) {
  ind_tag <- subset(group1_2018, group1_2018$Hex == tag)
  min_det <- min(ind_tag$dtf)
  print(min_det)
  tag_id <- tag
  df_det <- rbind(df_det, data.frame(min_det, tag_id))
}

#make a column of the min dets called rounded_det, set to a date 
df_det$rounded_det <- as.Date(df_det$min_det)
class(df_det$rounded_det)

#add proportion of fish moving calculation
data_g1_2018<- df_det %>% 
  group_by(rounded_det) %>% 
  tally() %>% 
  mutate(proportion = n/(sum(n)))

# add column for group
data_g1_2018$group <- "2018 Upper Release"

#make dummy date column
data_g1_2018 <-  data_g1_2018 %>% 
  mutate(dummydate = ymd(paste(2017, month(rounded_det), day(rounded_det), sep = "-")))

#now plot both groups:
ggplot() + 
  geom_density(data = data_g1_2017, aes(x = dummydate, fill = group), alpha = 0.3) +
  geom_density(data = data_g1_2018, aes(x = dummydate, fill = group), alpha = 0.3) +
  labs(title = "Upper Release Initial Migration", y = "Proportion of fish" , 
       x = "Date") +
  theme_classic() + 
  theme(text = element_text(size=16))+
  theme( title = element_text(size=18), 
         legend.text= element_text(size=12),
         legend.title = element_text(size =14)) +
  guides(fill= guide_legend("Group")) +
  scale_fill_manual( values = c("navy","orange"))+
  ggsave("figure_output/upper_17_18_init.pdf", width = 11, height = 8)






####### Group 2 initial movement 2017 & 2018
group2_2018 <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/072018_group2_filtered_16km_2018_FINAL.csv", 
                        col_types = cols(dtf = col_datetime(format = "%m/%d/%Y %H:%M:%S")))

group2_2017 <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/071818_group2_filtered_16km_durhamRel_2017.csv", 
                        col_types = cols(dtf = col_datetime(format = "%m/%d/%Y %H:%M:%S")))


group2_2017_truncated <- group2_2017 %>% 
  filter(Genrkm <= 175)

#group2 2017: make df of initial movement 
alltags<- unique(group2_2017_truncated$Hex)
print(alltags)
df_det <- NULL

for(tag in alltags) {
  ind_tag <- subset(group2_2017_truncated, group2_2017_truncated$Hex == tag)
  min_det <- min(ind_tag$dtf)
  print(min_det)
  tag_id <- tag
  df_det <- rbind(df_det, data.frame(min_det, tag_id))
}
#make a column of themin dets called rounded_det, set to a date 
df_det$rounded_det <- as.Date(df_det$min_det)
class(df_det$rounded_det)

#add proportion of fish moving calculation
data_g2_2017 <- df_det %>% 
  group_by(rounded_det) %>% 
  tally() %>% 
  mutate(proportion = n/(sum(n)))

#add column for group
data_g2_2017$group <- "2017 Lower Release"

#makedummy date column:
g2_2017_init <-  data_g2_2017 %>% 
  mutate(dummydate = ymd(paste(2017, month(rounded_det), day(rounded_det), sep = "-")))


# group2 2018: make df of initial movement 
group2_2018_truncated <- group2_2018 %>% 
  filter(Genrkm <= 175) 

df_det <- NULL

  alltags<- unique(group2_2018_truncated$Hex)
  print(alltags)
  for(tag in alltags) {
    ind_tag <- subset(group2_2018, group2_2018$Hex == tag)
    min_det <- min(ind_tag$dtf)
    print(min_det)
    tag_id <- tag
    df_det <- rbind(df_det, data.frame(min_det, tag_id))
  }
  
  #make a column of themin dets called rounded_det, set to a date 
  df_det$rounded_det <- as.Date(df_det$min_det)
  class(df_det$rounded_det)
  
  #add proportion of fish moving calculation
  data_g2_2018<- df_det %>% 
    group_by(rounded_det) %>% 
    tally() %>% 
    mutate(proportion = n/(sum(n)))
  
  # add column for group
  data_g2_2018$group <- "2018 Lower Release"
  
  #make dummy date column
  g2_2018_init <-  data_g2_2018 %>% 
    mutate(dummydate = ymd(paste(2017, month(rounded_det), day(rounded_det), sep = "-")))
  
  #now plot both dfs
  ggplot() + 
    geom_density(data = g2_2017_init, aes(x = dummydate, fill = group), alpha = 0.3) +
    geom_density(data = g2_2018_init, aes(x = dummydate, fill = group), alpha = 0.3) +
    labs(title = "Lower Release Initial Migration", y = "Proportion of fish" , 
         x = "Date") +
    theme_classic() + 
    theme(text = element_text(size=16))+
    theme( title = element_text(size=18), 
           legend.text= element_text(size=12),
           legend.title = element_text(size =14)) +
    guides(fill= guide_legend("Group")) +
    scale_fill_manual( values = c("navy","orange"))+
    ggsave("figure_output/lower_17_18_init.pdf", width = 11, height = 8)
  
  
 

  
  