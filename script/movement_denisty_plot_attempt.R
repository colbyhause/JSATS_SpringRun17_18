# Aug 14 2018
library(lubridate)
library(ggridges)
library(ggplot2)

df_group1_2017 <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/071818_group1_filtered_16km_2017.csv", 
                           col_types = cols(dtf = col_datetime(format = "%m/%d/%Y %H:%M:%S")))


group1_2017_truncated <- df_group1_2017 %>% 
  filter(Genrkm <= 290)

alltags<- unique(group1_2017$Hex)
print(alltags)
df_det <- NULL



for(tag in alltags) {
  ind_tag <- subset(df_group1_2017, df_group1_2017$Hex == tag)
  min_det <- min(ind_tag$dtf)
  print(min_det)
  tag_id <- tag
  df_det <- rbind(df_det, data.frame(min_det, tag_id))
}
df_det$rounded_det <- as.Date(df_det$min_det)
class(df_det$rounded_det)

data <- df_det %>% 
group_by(rounded_det) %>% 
  tally() %>% 
  mutate(proportion = n/187)  #187 = the number of fish that survived downstream 

row.has.na <- apply(data, 1, function(x){any(is.na(x))})

data <- data[!row.has.na,]

ggplot(data, aes(rounded_det, proportion, group = 1)) +
  geom_line()

ggplot(data) + 
  geom_density(aes(x = rounded_det))
  
class(df_det$min_det)


################### truncating dets up til 165 #############

group1_2017_truncated <- group1_2017_truncated %>% 
  filter(Genrkm <= 290)

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


df_det$rounded_det <- as.Date(df_det$min_det)
class(df_det$rounded_det)

data <- df_det %>% 
  group_by(rounded_det) %>% 
  tally() %>% 
  mutate(proportion = n/(sum(n)))

data$group <- "rel1_2017"

#get rid of NAs if necessary
row.has.na <- apply(data, 1, function(x){any(is.na(x))})
data <- data[!row.has.na,]


ggplot(data, aes(rounded_det, proportion, group = 1)) +
  geom_line()

ggplot(data, aes(rounded_det, proportion, group = 1)) +
  geom_density_ridges()

ggplot(data) + 
  geom_density(aes(x = rounded_det))

ggplot(data, aes(x= rounded_det, y = proportion, height = proportion)) +
  geom_ridgeline()



######group 2 2017
group2_2017 <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/071818_group2_filtered_16km_durhamRel_2017.csv", 
                        col_types = cols(dtf = col_datetime(format = "%m/%d/%Y %H:%M:%S")))

group2_2017_truncated <- group2_2017 %>% 
  filter(Genrkm <= 175)

df_det <- NULL

init_movement <- function(df) { 
  alltags<- unique(df$Hex)
  print(alltags)
  for(tag in alltags) {
    ind_tag <- subset(df, df$Hex == tag)
    min_det <- min(ind_tag$dtf)
    print(min_det)
    tag_id <- tag
    df_det <- rbind(df_det, data.frame(min_det, tag_id))
  }
  return()
}

init_movement(group2_2017)

df_det$rounded_det <- as.Date(df_det$min_det)
class(df_det$rounded_det)

data_g2_2017<- df_det %>% 
  group_by(rounded_det) %>% 
  tally() %>% 
  mutate(proportion = n/(sum(n)))

df_det <- NULL

alltags<- unique(group2_2017_truncated$Hex)
print(alltags)
for(tag in alltags) {
  ind_tag <- subset(group2_2017_truncated, group2_2017_truncated$Hex == tag)
  min_det <- min(ind_tag$dtf)
  print(min_det)
  tag_id <- tag
  df_det <- rbind(df_det, data.frame(min_det, tag_id))
}

df_det$rounded_det <- as.Date(df_det$min_det)
class(df_det$rounded_det)

data_g2_2017<- df_det %>% 
  group_by(rounded_det) %>% 
  tally() %>% 
  mutate(proportion = (n/(sum(n))))

data_g2_2017$group <- "2017 Durham Release Group"


ggplot(data_g2_2017, aes(rounded_det, proportion, height = proportion)) +
  geom_ridgeline()

ggplot(data_g2_2017) + 
  geom_density(aes(x = rounded_det))


write.csv(data, "data/group1_2017_moventment_df.csv" )
write.csv(data_g2_2017, "data/group2_2017_moventment_df.csv" )

g1_g2_movement_2017 <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/2017_g1_g2_movement.csv")

g1_g2_movement_2017$rounded_det <- as.Date(g1_g2_movement_2017$rounded_det)
class(g1_g2_movement_2017$rounded_det)

ggplot(g1_g2_movement_2017, aes(x = rounded_det, y = n)) + 
  geom_density_ridges()


ggplot(g1_g2_movement_2017, aes(x = rounded_det, fill = group)) + 
  geom_density(alpha = 0.3)



################# trying using days after release##########

data$release_date<- "2017-03-05"

data <- data %>% 
  mutate(days_after_rel = difftime(rounded_det, release_date, units = "days"))

class(data$days_after_rel)
data$days_after_rel <- as.numeric(data$days_after_rel)

class(data$days_after_rel)

ggplot(data, aes(x = rounded_det, y = group)) + 
  geom_density_ridges()


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
data_g2_2017$group <- "2017 Durham Release Group"

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
  data_g2_2018$group <- "2018 Durham Release Group"
  
  #make dummy date column
  g2_2018_init <-  data_g2_2018 %>% 
    mutate(dummydate = ymd(paste(2017, month(rounded_det), day(rounded_det), sep = "-")))
  
  #now plot both dfs
  ggplot() + 
    geom_density(data = g2_2017_init, aes(x = dummydate, fill = group), alpha = 0.3) +
    geom_density(data = g2_2018_init, aes(x = dummydate, fill = group), alpha = 0.3) +
    labs(title = "Lower Release Initial Migration", y = "Proportion of fish" , 
         x = "Date") +
    theme_bw() + 
    theme( title = element_text(size=14), 
           legend.text= element_text(size=9),
           legend.title = element_text(size =12)) +
    guides(fill= guide_legend("Group")) +
    scale_fill_manual( values = c("navy","orange"))
  dev.off()
  
  
 
  #for truncated dataset:
  g2_17_18_init <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/group2_2018_2017_intitMovement_BothTruncated.csv", 
                            col_types = cols(rounded_det = col_date(format = "%m/%d/%Y")))
  
  #have to make a dummydate and set to the same year
  g2_17_18_init<-  g2_17_18_init %>% 
    mutate(dummydate = ymd(paste(2017, month(rounded_det), day(rounded_det), sep = "-")))
  
  #plot both years
  pdf("Group2_initMovement_bothTrunc.pdf", width = 7, height = 4)
  ggplot(g2_17_18_init, aes(x = dummydate, fill = group)) + 
    geom_density(alpha = 0.3) +
    labs(title = "Lower Release Initial Migration", y = "Proportion of fish" , 
         x = "Date") +
    theme_bw() + 
    theme( title = element_text(size=14), 
           legend.text= element_text(size=9),
           legend.title = element_text(size =12)) +
    guides(fill= guide_legend("Group")) +
    scale_fill_manual( values = c("navy","orange"))
  dev.off()
  
  
  #trying to plot wiht 2 files:
 g2_2017_init <-  data_g2_2017 %>% 
    mutate(dummydate = ymd(paste(2017, month(rounded_det), day(rounded_det), sep = "-")))
 
 g2_2018_init <-  data_g2_2018 %>% 
   mutate(dummydate = ymd(paste(2017, month(rounded_det), day(rounded_det), sep = "-")))
 
 ggplot() + 
   geom_density(data = g2_2017_init, aes(x = dummydate, fill = group), alpha = 0.3) +
   geom_density(data = g2_2018_init, aes(x = dummydate, fill = group), alpha = 0.3) +
   labs(title = "Lower Release Initial Migration", y = "Proportion of fish" , 
        x = "Date") +
   theme_bw() + 
   theme( title = element_text(size=14), 
          legend.text= element_text(size=9),
          legend.title = element_text(size =12)) +
   guides(fill= guide_legend("Group")) +
   scale_fill_manual( values = c("navy","orange"))
 dev.off()
 
  
  ####### Group 1 initial movement 2017 & 2018#####################
  
  
  group1_2017 <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/071818_group1_filtered_16km_2017.csv", 
                          col_types = cols(dtf = col_datetime(format = "%m/%d/%Y %H:%M:%S")))
  
  group1_2018 <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/072018_group1_filtered_16km_2018_FINAL.csv", 
                          col_types = cols(dtf = col_datetime(format = "%m/%d/%Y %H:%M:%S")))
  
  
  group1_2017_truncated <- group1_2017 %>%   # truncate detections at 290 (just above 165) bc it looks like fish are hanging in ESB, so lots of early dets there
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
  data_g1_2017$group <- "2017 Upper Release Group"
  
  
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
  data_g1_2018$group <- "2018 Upper Release Group"
  
  
  #write seperate csvs to merge
  write.csv(data_g1_2018, "data/group1_2018_intitMovement_NEWMANtest.csv")
  write.csv(data_g1_2017, "data/group1_2017_intitMovement.csv")
  
  #merged csv for plot:
  g1_17_18_initMove <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/group1_2018_2017_initmovement.csv", 
                                col_types = cols(rounded_det = col_date(format = "%m/%d/%Y")))
  
  #have to make a dummydate and set to the same year
  g1_17_18_initMove <-  g1_17_18_initMove %>% 
    mutate(dummydate = ymd(paste(2017, month(rounded_det), day(rounded_det), sep = "-")))
  
  #plot both years
  
  pdf("Group1_initMovement.pdf", width = 6, height = 4)
  ggplot(g1_17_18_initMove, aes(x = dummydate, fill = group)) + 
    geom_density(alpha = 0.3) +
    labs(title = "Initial Migration of Fish after Release", y = "Proportion of fish" , 
         x = "Date") +
    theme_bw() +
    theme( title = element_text(size=14), 
          legend.text= element_text(size=9),
          legend.title = element_text(size =12)) +
    guides(fill= guide_legend("Group")) +
    scale_fill_manual( values = c("navy","orange"))
  
  dev.off()
    
  
  
  #Testing out to see when fish were first detected from Newman on down 
  
  pdf("group12018_newmantest.pdf", width = 6, height = 4)
  ggplot(data_g1_2018, aes(x = rounded_det)) + 
    geom_density(alpha = 0.3) +
    labs(title = "Newman Test G1 2018", y = "Proportion of fish" , 
         x = "Date") +
    theme_bw() +
    theme( title = element_text(size=14), 
           legend.text= element_text(size=9),
           legend.title = element_text(size =12)) +
    #guides(fill= guide_legend("Group")) +
    scale_fill_manual( values = c("navy","orange"))
  
  dev.off()
  
  
  
  
  
