#tHE CODE BELOW WAS cOLBY'S FIRST ATTEMPT AT GETTING SURVIVAL PLOTS, use "2017_survivalcode_final.R" for plot code
library(tidyverse)
library(ggplot2)
library(readr)

#Aug 16 2018 Modeling Standardized cumulative survival with CI
surv_s10_2017 <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/2017surv_s10_CH.csv")
surv_s10_2017_delta <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/2017_DeltaSurv_forPlot_CH.csv")

surv_s10_2017_model <- surv_s10_2017[1:16, ]

pdf("HarmonSSurv_2017.pdf")
ggplot(data = surv_s10_2017_model, aes(x = dist_from_rel, y = standardized)) +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = upper, ymax = lower ))+
  labs(title = "Harmon Rd Release Standardized Survival", y = "Survival (per 10 km)", x = "Distance from Release")

dev.off()


pdf("DeltaSSurv_2017.pdf")
ggplot(data = surv_s10_2017_delta, aes(x = dist_from_rel, y = standardized)) +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = upper, ymax = lower ))+
  labs(title = "Delta Release Standardized Survival", y = "Survival (per 10 km)", x = "Distance from Release")

dev.off()

#the error bars on lambda are not correct for either plot ^^

#Aug 17, 2018 Plotting Cumulative surivival ROUGH CODE

# Adding cumul survival from the other routes : Delta
cumulsurv_PC_2017 <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/2017_PC_phiB1A7_cumulsurv.csv")
cumulsurv_OR_2017 <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/2017_OR_phiC1A7_cumulsurv.csv")

cumulsurv_OR_2017 <- cumulsurv_OR_2017[1:5,]
cumulsurv_PC_2017 <- cumulsurv_PC_2017[1:3, ]

pdf("2017_delta_cumulsurv_allroutes_fromReltochipps.pdf")
ggplot(data = surv_s10Delta_2017_fixingto1, aes(x= rkm, y = cumul)) +
  geom_smooth(mapping = NULL, data = NULL, stat = "smooth",
              formula = y ~ x, se = FALSE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, 
              colour="black", fill= "white") +
  geom_smooth(data = cumulsurv_PC_2017, aes(x= rkm, y = cumul), color = "black", linetype="dotted") +
  geom_smooth(data = cumulsurv_OR_2017, aes(x= rkm, y = cumul), linetype = "3313", color  = "black") +
  scale_x_reverse() +
  theme_classic()+
  geom_vline(xintercept = 72.24, linetype="dashed", 
             color = "red", size=0.5)
   

dev.off()


#cumul surivival using sA1-sA7 *BCAtoChipps con func*sA14*lambda: Upper Release Model 

upper_cumulsurv_2017_convFunc <- read_csv("~/GitHub/JSATS_SpringRun17_18/data/2017_harmon_cumulsurv_WconvFunction.csv")

spline_int <- as.data.frame(spline(upper_cumulsurv_2017_convFunc$rkm, upper_cumulsurv_2017_convFunc$cumul))

ggplot(data = upper_cumulsurv_2017_convFunc, aes(x= rkm, y = cumul)) +
  geom_smooth(mapping = NULL, data = NULL, stat = "smooth",
              formula = y ~ x, se = FALSE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, 
              colour="black", fill= "white") +
  scale_x_reverse() +
  theme_bw()


ggplot(spline_int) +
  geom_line(aes(x = x, y = y)) +
  scale_x_reverse() +
  theme_bw()

ggplot(data = upper_cumulsurv_2017_convFunc, aes(x= rkm, y = cumul)) +
  geom_line()+
  scale_x_reverse() +
  theme_bw()

#Gabes code for Full model:

cumul_est <- read_csv("data/CumulativeSurvival/datafor_GS_surival_code/cumul_estimates.csv")

ggplot(cumul_est) + 
  stat_smooth(aes(x = rkm, y = cumul), se = F) +
  scale_x_reverse()


spline_int <- as.data.frame(spline(cumul_est$rkm, cumul_est$cumul))
spline_int2 <- as.data.frame(spline(cumul_est$rkm, cumul_est$cumul2))

pdf("upper_cumulativesurv_2017.pdf")
ggplot(spline_int) + 
  geom_vline(xintercept = 257.64, lty = 3, color = "gray40") + 
  geom_vline(xintercept = 181.72, lty = 3, color = "gray40") + 
  geom_vline(xintercept = 72.24, lty = 3, color = "gray40") +
  geom_line(aes(x = x, y = y)) +
  #geom_line(data = spline_int2, aes(x = x, y =y)) +                # remove comment to plot cumul surv after initial reach in resto are
  #annotate("text", label = lam, x = 5, y = 0.1, parse = T, size = 4) +
  annotate("text", label = "Restoration \nArea", x = 300, y = 0, size = 5, lineheight = 0.8, color = "gray") +
  annotate("text", label = "Lower \nRiver", x = 220, y = 0, size = 5, lineheight = 0.8, color = "gray") +
  annotate("text", label = "Delta", x = 125, y = 0, size = 5, color = "gray") +
  annotate("text", label = "Estuary", x = 20, y = 0, size = 5, color = "gray") +
  scale_x_reverse() + 
  labs(title = "Upper Release Cumulative Survival Estimate", y = "Survival", 
       x = "River Kilometers from Golden Gate") + 
  theme_classic() +
  theme(text = element_text(size = 16)) 

dev.off()
