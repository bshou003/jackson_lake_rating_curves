####################### Calling Packages ############################# 
library(minpack.lm)
library(tidyverse)
library(lubridate)
library(zoo)
library(here)
library(broom)
library(ggpubr)
library(scales)
####################### Call Data #################################
source("r/functions.R")
Qall <- read.csv("raw_data/discharge_stage_height.csv")

################# Quick Plot to Visulize Data ######################
ggplot()+
  geom_point(data = Qall, aes(x = discharge.m.s, y = gauge.m, color = event))+
  facet_wrap(~site, scales = "free") +
  labs(y = "Stage Height (m)",
       x = expression(paste("Discharge"(m^3/s))))


######## Model Plots #########
site_10 <-lm_dis_forced_zero(Qall, 10)
site_16 <- nlm_dis_forced_zero(Qall, 16)
site_17 <- lm_dis_forced_zero(Qall, 17)
site_19 <- lm_dis_forced_zero(Qall, 19)
site_20 <- lm_dis_forced_zero(Qall, 20)
site_22 <- lm_dis_forced_zero(Qall, 22)
site_27 <- nlm_dis_forced_zero(Qall, 27)
site_30 <- lm_dis_forced_zero(Qall, 30)
site_38 <- nlm_dis_forced_zero(Qall, 38)
site_40 <- nlm_dis_forced_zero(Qall, 40)

########### Data and Model Plots###################
##### These are the rating curves developed for each site forcing zero as an
##### intercept.
ggarrange(site_10[["plot"]], site_16[["plot"]], site_17[["plot"]], 
          site_19[["plot"]], site_20[["plot"]], site_22[["plot"]],
          site_27[["plot"]], site_30[["plot"]], site_38[["plot"]],
          site_40[["plot"]], ncol = 5, nrow =2)

site_10_dis <- pressure_to_discharge_lm("raw_data/Arizona.Creek.Baro_2025-08-11_19-45-18-131.csv", 63,
                                    "raw_data/Arizona.Creek_2025-08-11_19-34-09-111.csv",
                                    69, site_10[["model_coeff"]], "uncleaned_final_data/site_10_discharge.csv") 
site_16_dis <- pressure_to_discharge_nlm("raw_data/North.Moran.Baro_Append_2025-08-12_12-32-13-014.csv", 64,
                                     "raw_data/Polecat.Creek_Append_2025-08-11_18-34-20-754.csv",
                                     70, site_16[["model_coeff"]], "uncleaned_final_data/site_16_discharge.csv")
site_17_dis <- pressure_to_discharge_lm("raw_data/North.Moran.Baro_Append_2025-08-12_12-32-13-014.csv", 64,
                                   "raw_data/Glade.Creek_Append_2025-08-11_17-02-01-504.csv",
                                   72, site_17[["model_coeff"]], "uncleaned_final_data/site_17_discharge.csv")
site_20_dis <- pressure_to_discharge_lm("raw_data/North.Moran.Baro_Append_2025-08-12_12-32-13-014.csv", 64,
                                   "raw_data/Moose.Creek_Append_2025-08-12_15-09-09-375.csv",
                                   70, site_20[["model_coeff"]], "uncleaned_final_data/site_20_discharge.csv")
site_22_dis <- pressure_to_discharge_lm("raw_data/North.Moran.Baro_Append_2025-08-12_12-32-13-014.csv", 64,
                                   "raw_data/Colter.Canyon_Append_2025-08-12_14-09-54-941.csv",
                                   70, site_22[["model_coeff"]], "uncleaned_final_data/site_22_discharge.csv")
site_27_dis <- pressure_to_discharge_nlm("raw_data/North.Moran.Baro_Append_2025-08-12_12-32-13-014.csv", 64,
                                     "raw_data/Moran.Creek_Append_2025-08-12_11-23-07-530.csv",
                                     70, site_38[["model_coeff"]], "uncleaned_final_data/site_27_discharge.csv") 
site_30_dis <- pressure_to_discharge_lm("raw_data/North.Moran.Baro_Append_2025-08-12_12-32-13-014.csv", 64,
                                    "raw_data/Bear.Paw.Creek_Append_2025-08-12_10-15-25-429.csv",
                                    70, site_30[["model_coeff"]], "uncleaned_final_data/site_30_discharge.csv")
site_38_dis <- pressure_to_discharge_nlm("raw_data/North.Moran.Baro_Append_2025-08-12_12-32-13-014.csv", 64,
                                     "raw_data/Waterfalls.Canyon_Append_2025-08-12_13-26-01-870.csv",
                                     71, site_38[["model_coeff"]], "uncleaned_final_data/site_38_discharge.csv")
site_40_dis <- pressure_to_discharge_nlm("raw_data/North.Moran.Baro_Append_2025-08-12_12-32-13-014.csv", 64,
                                     "raw_data/North.Moran_Append_2025-08-12_12-15-13-793.csv",
                                     70, site_40[["model_coeff"]], "uncleaned_final_data/site_40_discharge.csv")

ggplot()+
  geom_line(data = site_17_discharge, aes(x= Date.and.Time, y =discharge))

########### Cleaning Data ###################
site_10_discharge <- read_csv("uncleaned_final_data/site_10_discharge.csv") %>% 
  subset(select = c(Date.and.Time, Temperature..C., discharge))

site_16_discharge <- read_csv("uncleaned_final_data/site_16_discharge.csv") %>% 
  subset(select = c(Date.and.Time, Temperature..C., discharge))

site_17_discharge <- read_csv("uncleaned_final_data/site_17_discharge.csv") %>% 
  mutate(adjusted_discharge = ifelse(discharge < 0,
                                     NA,  
                                     discharge))%>% 
  rename(raw_discharge = discharge,
         discharge = adjusted_discharge)
write_csv(site_17_discharge, "uncleaned_final_data/site_17_discharge.csv")
site_17_discharge <- read_csv("uncleaned_final_data/site_17_discharge.csv") %>% 
  subset(select = c(Date.and.Time, Temperature..C., discharge))
  

#Site 20 is cleaned removing negative discharge values.
site_20_discharge <- read_csv("uncleaned_final_data/site_20_discharge.csv") %>%
  mutate(adjusted_discharge = ifelse(discharge < 0,
                                     NA,  
                                     discharge))%>% 
  rename(raw_discharge = discharge,
         discharge = adjusted_discharge)
write_csv(site_20_discharge, "uncleaned_final_data/site_20_discharge.csv")
site_20_discharge <- read_csv("uncleaned_final_data/site_20_discharge.csv") %>% 
  subset(select = c(Date.and.Time, Temperature..C., discharge))

#Site 22 is cleaned removing negative discharge values.
site_22_discharge <- read_csv("uncleaned_final_data/site_22_discharge.csv") %>%
  mutate(adjusted_discharge = ifelse(discharge < 0,
                                     NA,  
                                     discharge))%>% 
  rename(raw_discharge = discharge,
         discharge = adjusted_discharge)
write_csv(site_22_discharge, "uncleaned_final_data/site_22_discharge.csv")
site_22_discharge <- read_csv("uncleaned_final_data/site_22_discharge.csv") %>% 
  subset(select = c(Date.and.Time, Temperature..C., discharge))
#Site 27 is cleaned removing inplausibly high discharge values.
site_27_discharge <- read_csv("uncleaned_final_data/site_27_discharge.csv") %>%
  mutate(adjusted_discharge = discharge,
         adjusted_discharge = ifelse(row_number() %in% 18226:19419, NA, adjusted_discharge))%>% 
  rename(raw_discharge = discharge,
         discharge = adjusted_discharge)
write_csv(site_27_discharge, "uncleaned_final_data/site_27_discharge.csv")
site_27_discharge <- read_csv("uncleaned_final_data/site_27_discharge.csv") %>% 
  subset(select = c(Date.and.Time, Temperature..C., discharge))

#Site 30 is cleaned removing negative discharge values.
site_30_discharge <- read_csv("uncleaned_final_data/site_30_discharge.csv") %>%
  mutate(adjusted_discharge = ifelse(discharge < 0,
                                     NA,  
                                     discharge))%>% 
  rename(raw_discharge = discharge,
         discharge = adjusted_discharge)

write_csv(site_30_discharge, "uncleaned_final_data/site_30_discharge.csv")
site_30_discharge <- read_csv("uncleaned_final_data/site_30_discharge.csv") %>% 
  subset(select = c(Date.and.Time, Temperature..C., discharge))

site_38_discharge <- read_csv("uncleaned_final_data/site_38_discharge.csv") %>% 
subset(select = c(Date.and.Time, Temperature..C., discharge))

site_40_discharge <- read_csv("uncleaned_final_data/site_40_discharge.csv") %>% 
subset(select = c(Date.and.Time, Temperature..C., discharge))
#####Writing Final Data #######
write_csv(site_10_discharge, "final_data/site_10_discharge.csv")
write_csv(site_16_discharge, "final_data/site_16_discharge.csv")
write_csv(site_17_discharge, "final_data/site_17_discharge.csv")
write_csv(site_20_discharge, "final_data/site_20_discharge.csv")
write_csv(site_22_discharge, "final_data/site_22_discharge.csv")
write_csv(site_27_discharge, "final_data/site_27_discharge.csv")
write_csv(site_30_discharge, "final_data/site_30_discharge.csv")
write_csv(site_38_discharge, "final_data/site_38_discharge.csv")
write_csv(site_40_discharge, "final_data/site_40_discharge.csv")