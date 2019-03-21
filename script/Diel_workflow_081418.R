#Aug 14, 2018
#pairing down Diel plots
#version of code used: Diel Analysis Code_FINAL_071918_CH.Rmd

#2017

group1_2017_Rel <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/071818_group1_filtered_16km_upperRel_2017.csv", 
                            col_types = cols(dtf = col_datetime(format = "%m/%d/%Y %H:%M:%S")))

group2_2017_Rel <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/071818_group2_filtered_16km_durhamRel_2017.csv", 
                            col_types = cols(dtf = col_datetime(format = "%m/%d/%Y %H:%M:%S")))

#Change rec names because code loops throughrec names, not Genrkm
diel_group1_renamed<- change_receiver_names(group1_2017_Rel)
diel_group2_renamed<- change_receiver_names(group2_2017_Rel)

#changing up grouping:

resto_dets_1<- subset(diel_group1_renamed, diel_group1_renamed$`GPS Names` == "Sandy Mush" | diel_group1_renamed$`GPS Names` == "Mariposa" | 
                        diel_group1_renamed$`GPS Names` == "Greenhouse" | diel_group1_renamed$`GPS Names` == "165") 
length(unique(resto_dets_1$Hex))

lower_SJ_1 <- subset(diel_group1_renamed, diel_group1_renamed$`GPS Names` == "Hills Ferry" | diel_group1_renamed$`GPS Names` == "Grayson" | 
                         diel_group1_renamed$`GPS Names` == "Crows")
length(unique(lower_sj_1$Hex))



delta_all_1 <- subset(diel_group1_renamed, diel_group1_renamed$`GPS Names` == "BCA"| diel_group1_renamed$`GPS Names` == "Paradise Cut" | 
                        diel_group1_renamed$`GPS Names` == "SJ Paradise Cut" | diel_group1_renamed$`GPS Names` == "Mossdale" |
                        diel_group1_renamed$`GPS Names` ==  "Head OR" | diel_group1_renamed$`GPS Names` == "OR Head OR" |
                        diel_group1_renamed$`GPS Names` ==  " OR MR" | diel_group1_renamed$`GPS Names` ==  "CC_CVP/RG" |
                        diel_group1_renamed$`GPS Names` == "Howard" | diel_group1_renamed$`GPS Names` ==  "Mid R Hwy" |
                        diel_group1_renamed$`GPS Names` ==  "OR Hwy" | diel_group1_renamed$`GPS Names` ==  "SJG" |
                        diel_group1_renamed$`GPS Names` == "SJC" | diel_group1_renamed$`GPS Names` == " Turner Cut" |
                        diel_group1_renamed$`GPS Names` ==  "MAC" | diel_group1_renamed$`GPS Names` == "Medford" |
                        diel_group1_renamed$`GPS Names` ==  "Jersey Point" | diel_group1_renamed$`GPS Names` == "Antioch" |
                        diel_group1_renamed$`GPS Names` == "Decker"| diel_group1_renamed$`GPS Names` ==  "Chipps" |
                        diel_group1_renamed$`GPS Names` == "Durham Ferry") 
length(unique(delta_all_1$Hex))


bay_1 <- subset(diel_group1_renamed, diel_group1_renamed$`GPS Names` == "Benicia"| diel_group1_renamed$`GPS Names` == "Golden Gate")
length(unique(bay_1$Hex))


#GROUP2 FISH


delta_all_2 <- subset(diel_group2_renamed, diel_group2_renamed$`GPS Names` == "BCA"| diel_group2_renamed$`GPS Names` == "Paradise Cut" | 
                        diel_group2_renamed$`GPS Names` == "SJ Paradise Cut" | diel_group2_renamed$`GPS Names` == "Mossdale" |
                        diel_group2_renamed$`GPS Names` ==  "Head OR" | diel_group2_renamed$`GPS Names` == "OR Head OR" |
                        diel_group2_renamed$`GPS Names` ==  " OR MR" | diel_group2_renamed$`GPS Names` ==  "CC_CVP/RG" |
                        diel_group2_renamed$`GPS Names` == "Howard" | diel_group2_renamed$`GPS Names` ==  "Mid R Hwy" |
                        diel_group2_renamed$`GPS Names` ==  "OR Hwy" | diel_group2_renamed$`GPS Names` ==  "SJG" |
                        diel_group2_renamed$`GPS Names` == "SJC" | diel_group2_renamed$`GPS Names` == " Turner Cut" |
                        diel_group2_renamed$`GPS Names` ==  "MAC" | diel_group2_renamed$`GPS Names` == "Medford" |
                        diel_group2_renamed$`GPS Names` ==  "Jersey Point" | diel_group2_renamed$`GPS Names` == "Antioch" |
                        diel_group2_renamed$`GPS Names` == "Decker" |diel_group2_renamed$`GPS Names` ==  "Chipps") #diel_group2_renamed$`GPS Names` == "Durham Ferry")
length(unique(delta_all_2$Hex))


bay_2 <- subset(diel_group2_renamed, diel_group2_renamed$`GPS Names` == "Benicia" | diel_group2_renamed$`GPS Names` == "Golden Gate")
length(unique(bay_2$Hex))

#GROUP1 FISH
make_circular_plots_multi(resto_dets_1, pdf_name = "Group 1 Restoration Area_fixed", " Group 1 Fish through Restoration Area")
#make_circular_plots_multi(exit_resto_1, pdf_name = "Group 1 Exit Resto_fixed", "Group 1 Fish exiting Restoration Area")
make_circular_plots_multi(lower_SJ_1, pdf_name = "Group 1 Lower SJ_fixed", "Group 1 Fish through the Lower San Joaquin")
#make_circular_plots_multi(enter_delta_1, pdf_name = "Group 1 Entering Delta_fixed", "Group 1 Fish entering Delta")
make_circular_plots_multi(delta_all_1, pdf_name = "Group 1 through Delta_fixed", "Group 1 Fish through Delta")    #arrow on delta_1 plot is not correct
#make_circular_plots_multi(exit_delta_1, pdf_name = "Group 1 Exit Delta_fixed", "Group 1 Fish exiting Delta")
make_circular_plots_multi(bay_1, pdf_name = "Group 1 Bay_fixed", "Group 1 Fish through Bay")
#make_circular_plots_multi(enter_ocean_1, pdf_name = "Group 1 Enter Ocean_fixed", "Group 1 Fish entering Ocean")


#GROUP 2 FISH
#make_circular_plots_multi(enter_delta_2, pdf_name = "Group 2 Entering Delta_fixed", "Group 2 Fish entering Delta")
make_circular_plots_multi(delta_all_2, pdf_name = "2017 Group 2 through Delta_fixed", "Group 2 Fish through Delta")
#make_circular_plots_multi(exit_delta_2, pdf_name = "Group 2 Exit Delta_fixed", "Group 2 Fish exiting Delta")
make_circular_plots_multi(bay_2, pdf_name = "Group 2 Bay_fixed", "Group 2 Fish through Bay")
#make_circular_plots_multi(enter_ocean_2, pdf_name = "Group 2 Enter Ocean_fixed", "Group 2 Fish entering Ocean")


#2018

group1_16rkm_2018 <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/UpperRel_072018_group1_filtered_16km_2018_FINAL.csv", 
                            col_types = cols(dtf = col_datetime(format = "%m/%d/%Y %H:%M:%S")))

group2_16rkm_2018 <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/DurhamRel_072018_group2_filtered_16km_2018_FINAL.csv", 
                            col_types = cols(dtf = col_datetime(format = "%m/%d/%Y %H:%M:%S")))

resto_dets_1<- subset(group1_16rkm_2018,  group1_16rkm_2018$Gen_Name == "Mud SL Br" | 
                        group1_16rkm_2018$Gen_Name == "Mud SL Conf" | group1_16rkm_2018$Gen_Name == "Blw Newman") 
length(unique(resto_dets_1$Hex))


lower_SJ_1 <- subset(group1_16rkm_2018, group1_16rkm_2018$Gen_Name == "Grayson" | group1_16rkm_2018$Gen_Name == "Crows" |
                       group1_16rkm_2018$Gen_Name == "Hills Ferry" | group1_16rkm_2018$Gen_Name == "Hills RT")
length(unique(lower_SJ_1$Hex))


delta_all_1 <- subset(group1_16rkm_2018, group1_16rkm_2018$Gen_Name == "BCA" | group1_16rkm_2018$Gen_Name == "Mossdale" |
                        group1_16rkm_2018$Gen_Name ==  "Head OR" | group1_16rkm_2018$Gen_Name == "OR Head OR" |
                        group1_16rkm_2018$Gen_Name == "MR OR" | 
                        group1_16rkm_2018$Gen_Name ==  " OR MR" | group1_16rkm_2018$Gen_Name ==  "CC_CVP/RG" |
                        group1_16rkm_2018$Gen_Name == "Howard" | group1_16rkm_2018$Gen_Name ==  "Mid R Hwy" |
                        group1_16rkm_2018$Gen_Name ==  "OR Hwy" | group1_16rkm_2018$Gen_Name ==  "SJG" |
                        group1_16rkm_2018$Gen_Name == "SJC" | group1_16rkm_2018$Gen_Name == " TC " |
                        group1_16rkm_2018$Gen_Name ==  "MAC" | group1_16rkm_2018$Gen_Name == "Medford" |
                        group1_16rkm_2018$Gen_Name ==  "Jersey Point" | group1_16rkm_2018$Gen_Name == "Durham Ferry" |
                        group1_16rkm_2018$Gen_Name ==  "Chipps") 
# | group1_16rkm_2018$Gen_Name == "Antioch" | #group1_16rkm_2018$Gen_Name == "Decker")
length(unique(delta_all_1$Hex))

#exit_delta_1 <- subset(group1_16rkm_2018, group1_16rkm_2018$Gen_Name ==  "Chipps")
#length(unique(exit_delta_1$Hex))

#bay_1 <- subset(group1_16rkm_2018, group1_16rkm_2018$Gen_Name == "Benicia")
#length(unique(bay_1$Hex))

#enter_ocean_1<- subset(group1_16rkm_2018, group1_16rkm_2018$Gen_Name == "Golden Gate")
#length(unique(enter_ocean_1$Hex))

#GROUP2 FISH


delta_all_2 <- subset(group2_16rkm_2018, group2_16rkm_2018$Gen_Name == "BCA" | group2_16rkm_2018$Gen_Name == "Mossdale" |
                        group2_16rkm_2018$Gen_Name ==  "Head OR" | group2_16rkm_2018$Gen_Name == "OR Head OR" |
                        group2_16rkm_2018$Gen_Name == "MR OR" | 
                        group2_16rkm_2018$Gen_Name ==  " OR MR" | group2_16rkm_2018$Gen_Name ==  "CC_CVP/RG" |
                        group2_16rkm_2018$Gen_Name == "Howard" | group2_16rkm_2018$Gen_Name ==  "Mid R Hwy" |
                        group2_16rkm_2018$Gen_Name ==  "OR Hwy" | group2_16rkm_2018$Gen_Name ==  "SJG" |
                        group2_16rkm_2018$Gen_Name == "SJC" | group2_16rkm_2018$Gen_Name == " TC " |
                        group2_16rkm_2018$Gen_Name ==  "MAC" | group2_16rkm_2018$Gen_Name == "Medford" |
                        group2_16rkm_2018$Gen_Name ==  "Jersey Point")  # group2_16rkm_2018$Gen_Name == "Durham Ferry") 
# | group2_16rkm_2018$Gen_Name == "Antioch" |
#group2_16rkm_2018$Gen_Name == "Decker")
length(unique(delta_all_2$Hex))

#exit_delta_2 <- subset(group2_16rkm_2018, group2_16rkm_2018$Gen_Name ==  "Chipps")
#length(unique(exit_delta_2$Hex))

#bay_2 <- subset(group2_16rkm_2018, group2_16rkm_2018$Gen_Name == "Benicia")
length(unique(bay_2$Hex))

#enter_ocean_2 <- subset(group2_16rkm_2018, group2_16rkm_2018$Gen_Name == "Golden Gate")
#length(unique(enter_ocean_2$Hex))

#GROUP1 FISH
make_circular_plots_multi(resto_dets_1, pdf_name = "2018 Group 1 Restoration Area_fixed", " 2018 Group 1 Fish through Restoration Area")
#make_circular_plots_multi(exit_resto_1, pdf_name = "2018 Group 1 Exit Resto_fixed", "2018 Group 1 Fish exiting Restoration Area")
make_circular_plots_multi(lower_SJ_1, pdf_name = "2018 Group 1 Lower SJ_fixed", "2018 Group 1 Fish through the Lower San Joaquin")
#make_circular_plots_multi(enter_delta_1, pdf_name = "2018 Group 1 Entering Delta_fixed", "2018 Group 1 Fish entering Delta")
make_circular_plots_multi(delta_all_1, pdf_name = "2018 Group 1 through Delta_fixed", "2018 Group 1 Fish through Delta")
#make_circular_plots_multi(exit_delta_1, pdf_name = "2018 Group 1 Exit Delta", "2018 Group 1 Fish exiting Delta")
#make_circular_plots_multi(bay_1, pdf_name = "2018 Group 1 Bay", "2018 Group 1 Fish through Bay")
#make_circular_plots_multi(enter_ocean_1, pdf_name = "2018 Group 1 Enter Ocean", "2018 Group 1 Fish entering Ocean")


#GROUP 2 FISH
#make_circular_plots_multi(enter_delta_2, pdf_name = "2018 Group 2 Entering Delta", "2018 Group 2 Fish entering Delta")
make_circular_plots_multi(delta_all_2, pdf_name = "2018 Group 2 through Delta_wDF", "2018 Group 2 Fish through Delta")
#make_circular_plots_multi(exit_delta_2, pdf_name = "2018 Group 2 Exit Delta", "2018 Group 2 Fish exiting Delta")
#make_circular_plots_multi(bay_2, pdf_name = "2018 Group 2 Bay", "2018 Group 2 Fish through Bay")
#make_circular_plots_multi(enter_ocean_2, pdf_name = "2018 Group 2 Enter Ocean", "2018 Group 2 Fish entering Ocean")




dev.off()
