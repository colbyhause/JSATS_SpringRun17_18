# Wed Aug 15 09:54:59 2018 ------------------------------
library(readxl)
library(tidyverse)
library(ggrepel)
library(here)

# Standarzied Survival Plot ------------------------------------------------
  s10 <- read_csv("data/Survival/datafor_GS_surival_code/s10_estimates.csv")
  
  
  s10_all <- s10 %>% 
    filter( type %in% c("s10phiB", "s10phiC", "s10"))
  
  s10_filtered <- s10 %>%
    filter(type == "s10") 
    
    
  s10_s10phiB <- s10 %>% 
    filter(type == "s10phiB")
  
  s10_s10phiC <- s10 %>% 
    filter(type == "s10phiC")
  
  s10_lambda <- s10 %>% 
    filter(type == 'lambda')
  
  pd <- position_dodge(width = .8)
  
  lam <- paste("lambda == S", "*p")
  
  ggplot(s10_filtered) + 
    geom_vline(xintercept = 257.64, lty = 3, color = "gray40") + 
    geom_vline(xintercept = 181.72, lty = 3, color = "gray40") + 
    geom_vline(xintercept = 72.24, lty = 3, color = "gray40") + 
    
    geom_point(data = s10_s10phiB, aes(x = rkm, y = Estimate,  group = type, color = 'steelblue'), size = 4, show.legend = T) + 
    geom_errorbar(data = s10_s10phiB, aes(x = rkm, ymin= `Lower Bound`, ymax=`Upper Bound`), colour="black",  
                  width = 1.2) +
    geom_line(data = s10_s10phiB, aes(x = rkm, y = Estimate), color = 'black', lty = 2,  size = 1.2, alpha = 0.2) +
    
    geom_point(data = s10_s10phiC, aes(x = rkm, y = Estimate,  group = type, color = 'coral'), size = 4, show.legend = T) + 
    geom_errorbar(data = s10_s10phiC, aes(x = rkm, ymin= `Lower Bound`, ymax=`Upper Bound`), colour="black",  
                  width = 1.2) +
    geom_line(data = s10_s10phiC, aes(x = rkm, y = Estimate), color = 'black', lty = 3, size = 1.2, alpha = 0.2) +
    
    geom_point(data = s10_lambda, aes(x = rkm, y = Estimate),  color = 'black', shape = 1, size = 4) + 
    geom_line(data = s10_lambda, aes(x = rkm, y = Estimate),  color = 'black', size = 1.2, alpha = 0.2) + 
    geom_errorbar(data = s10_lambda, aes(x = rkm, ymin= `Lower Bound`, ymax=`Upper Bound`), colour="black",  
                  width = 1.2) +
    
    geom_point(aes(x = rkm, y = Estimate,  group = type, color = 'black'),size = 4, show.legend = T) + 
    geom_errorbar(aes(x = rkm, ymin= `Lower Bound`, ymax=`Upper Bound`), width = 1.2) +
    geom_line( aes(x = rkm, y = Estimate), color = 'black', size = 1.2, alpha = 0.2) +
    
    annotate("text", label = lam, x = 20, y = 0.4, parse = T, size = 4) +
    annotate("text", label = "Restoration \nArea", x = 300, y = 0, size = 5, lineheight = 0.8, color = "black") +
    annotate("text", label = "Lower \nRiver", x = 220, y = 0, size = 5, lineheight = 0.8, color = "black") +
    annotate("text", label = "Delta", x = 125, y = 0, size = 5, color = "black") +
    annotate("text", label = "Estuary", x = 20, y = 0, size = 5, color = "black") +
    
    scale_color_manual(values = c("black", "coral", "steelblue" ), labels = c("Mainstem SJ", "Old River", "Paradise Cut"), 
                       name = "Route") + 
    scale_x_reverse() +
    labs(title = bquote("Standardzied Reach Specific Survival Estimate (" ~S[10]~ ")"), y = expression(S[10]), 
         x = "River Kilometers from Golden Gate") + 
    ylim(c(0, 1.1)) + 
    theme_classic() + 
    theme(text = element_text(size = 16)) + 
    ggsave("figure_output/Survival/final_plots/upper_stand_surv.pdf", dpi = 350, width = 12, height = 7)
 

# Standardized Survival Delta ---------------------------------------------
  s10delt <- read_csv("data/Survival/datafor_GS_surival_code/s10_Delta_estimates.csv")
  
  s10_filtereddelt <- s10delt %>%
    filter(type == "s10") 
  
  
  s10_s10phiBdelt <- s10delt %>% 
    filter(type == "s10phiB")
  
  s10_s10phiCdelt <- s10delt %>% 
    filter(type == "s10phiC")
  
  s10_lambdadelt <- s10delt %>% 
    filter(type == 'lambda')
  
  pd <- position_dodge(width = .8)
  
  lam <- paste("lambda == S", "*p")
  
  ggplot(s10_filtereddelt) + 
    
    geom_vline(xintercept = 72.24, lty = 3, color = "gray40") + 
    
    geom_point(data = s10_s10phiBdelt, aes(x = rkm, y = Estimate,  group = type, color = 'steelblue'), size = 4, show.legend = T) + 
    geom_errorbar(data = s10_s10phiBdelt, aes(x = rkm, ymin= `Lower Bound`, ymax=`Upper Bound`), colour="black",  
                  width = 1.2) +
    geom_line(data = s10_s10phiBdelt, aes(x = rkm, y = Estimate), color = 'black', lty = 2,  size = 1.2, alpha = 0.2) +
    
    geom_point(data = s10_s10phiCdelt, aes(x = rkm, y = Estimate,  group = type, color = 'coral'), size = 4, show.legend = T) + 
    geom_errorbar(data = s10_s10phiCdelt, aes(x = rkm, ymin= `Lower Bound`, ymax=`Upper Bound`), colour="black",  
                  width = 1.2) +
    geom_line(data = s10_s10phiCdelt, aes(x = rkm, y = Estimate), color = 'black', lty = 3, size = 1.2, alpha = 0.2) +
    
    geom_point(data = s10_lambdadelt, aes(x = rkm, y = Estimate),  color = 'black', shape = 1, size = 4) + 
    geom_line(data = s10_lambdadelt, aes(x = rkm, y = Estimate),  color = 'black', size = 1.2, alpha = 0.2) + 
    geom_errorbar(data = s10_lambdadelt, aes(x = rkm, ymin= `Lower Bound`, ymax=`Upper Bound`), colour="black",  
                  width = 1.2) +
    
    geom_point(aes(x = rkm, y = Estimate,  group = type, color = 'black'),size = 4, show.legend = T) + 
    geom_errorbar(aes(x = rkm, ymin= `Lower Bound`, ymax=`Upper Bound`), width = 1.2) +
    geom_line( aes(x = rkm, y = Estimate), color = 'black', size = 1.2, alpha = 0.2) +
    
    annotate("text", label = lam, x = 20, y = 0.55, parse = T, size = 4) +
    annotate("text", label = "Delta", x = 125, y = 0, size = 5, color = "black") +
    annotate("text", label = "Estuary", x = 20, y = 0, size = 5, color = "black") +
    
    scale_color_manual(values = c("black", "coral", "steelblue" ), labels = c("Mainstem SJ", "Old River", "Paradise Cut"), 
                       name = "Route") + 
    scale_x_reverse() +
    labs(title = bquote("Standardzied Reach Specific Survival Estimate (" ~S[10]~ ")"), y = expression(S[10]), 
         x = "River Kilometers from Golden Gate") + 
    ylim(c(0, 1.1)) + 
    theme_classic() + 
    theme(text = element_text(size = 16)) + 
    ggsave("figure_output/Survival/final_plots/delta_stand_surv.pdf", dpi = 350, width = 12, height = 7)

  

# Cumulative Survival Full Model ------------------------------------------
cumul_est <- read_csv("data/cumul_estimates.csv")
  
  ggplot(cumul_est) + 
    stat_smooth(aes(x = rkm, y = cumul), se = F) +
    scale_x_reverse()

  
  spline_int <- as.data.frame(spline(cumul_est$rkm, cumul_est$cumul))
  spline_int2 <- as.data.frame(spline(cumul_est$rkm, cumul_est$cumul2))
  
  #pdf("2017_upper_cumul_final.pdf")
  ggplot(spline_int) + 
    geom_vline(xintercept = 257.64, lty = 3, color = "gray40") + 
    geom_vline(xintercept = 181.72, lty = 3, color = "gray40") + 
    geom_vline(xintercept = 72.24, lty = 3, color = "gray40") +
    geom_line(aes(x = x, y = y)) +
    #geom_line(data = spline_int2, aes(x = x, y =y)) +                # remove comment to plot cumul surv after initial reach in resto are
    annotate("text", label = lam, x = 5, y = 0.1, parse = T, size = 4) +
    annotate("text", label = "Restoration \nArea", x = 300, y = 0, size = 5, lineheight = 0.8, color = "black") +
    annotate("text", label = "Lower \nRiver", x = 220, y = 0, size = 5, lineheight = 0.8, color = "black") +
    annotate("text", label = "Delta", x = 125, y = 0, size = 5, color = "black") +
    annotate("text", label = "Estuary", x = 20, y = 0, size = 5, color = "black") +
    scale_x_reverse() + 
    labs(title = "Cumulative Survival Estimate", y = "Survival", 
         x = "River Kilometers from Golden Gate") + 
    theme_classic() +
    theme(text = element_text(size = 16))+
    ggsave("figure_output/Survival/final_plots/upper_2017_cumulsurv.pdf",  width = 12, height = 7)
  #dev.off()


    