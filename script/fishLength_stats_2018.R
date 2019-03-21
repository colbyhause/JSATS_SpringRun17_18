# performing Mann_Whitney U test on 2018 Scarf 1 vs 2 fish

fishsize_2018 <- read_csv("Z:/Shared/Projects/JSATS/DSP_Spring-Run Salmon/2018/2018Analysis/Fish Size Histos/2018_allfish.csv")


us_scarf <- fishsize_2018 %>% 
  filter(group == "sjscarf1")

v_size_us <- us_scarf$Length

ds_scarf <- fishsize_2018 %>% 
  filter(group == "sjscarf2")

v_size_ds <- ds_scarf$Length

wilcox.test(v_size_us, v_size_ds)
