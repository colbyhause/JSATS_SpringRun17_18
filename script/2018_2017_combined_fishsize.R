#Aug 20, 2018 Combining 2017 and 2017 Fish Size Histos into 1 graph 

library(tidyverse)
library(tibble)
library(readr)
install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device="win")
fonts()


fishsize_2017 <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/2017allfish_size.csv")

fishsize_2018 <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/2018_allfish_size.csv")

fishsize_2017_2018 <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/2017_2018allfish_size.csv")




ggplot(data = fishsize_2017_2018, aes(x = length, fill = year)) +
  geom_density(alpha = 0.3)+
  theme_bw()+
  labs(title = "2017 and 2018 Fork Lengths", y = "Proportion of fish" , 
       x = "Date") +
  theme_classic() +
  xlab("Length (mm)") +
  scale_fill_manual(values=c("blue","yellow")) +
  theme( title = element_text(size=18), 
         text = element_text(size=16),
         legend.text= element_text(size=12),
         legend.title = element_text(size =14)) +
  guides(fill= guide_legend("Group")) +
  ggsave("~/GitHub/JSATS_SpringRun17_18/figure_output/FishSizeHistos/2017_2018_fish_size_histos.pdf", width = 12, height = 7)

  
  

#Stats:

v_size_2018 <- fishsize_2017_2018 %>% 
  filter(year == "2018 Release Fish")

v_size_2017 <- fishsize_2017_2018 %>% 
  filter(year == "2017 Release Fish")


wilcox.test(v_size_2017$length, v_size_2018$length)


