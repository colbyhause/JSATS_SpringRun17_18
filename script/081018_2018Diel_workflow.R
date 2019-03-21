# Fixing 2018 Group1 through Restoration plot axis

group1_16rkm_2018<- read_csv("~/GitHub/JSATS_SpringRun17_18/data/072018_group1_filtered_16km_2018_FINAL.csv", 
                        col_types = cols(dtf = col_datetime(format = "%m/%d/%Y %H:%M:%S")))

resto_dets_1<- subset(group1_16rkm_2018, group1_16rkm_2018$Gen_Name == "Mud SL Br" | 
                        group1_16rkm_2018$Gen_Name == "Mud SL Conf" | group1_16rkm_2018$Gen_Name == "Blw Newman") 

make_circular_plots_multi(resto_dets_1, pdf_name = "2018 Group 1 Restoration Area_axis", " 2018 Group 1 Fish through Restoration Area")


dev.off()
