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
Qall <- read.csv("discharge_stage_height.csv")

################# Quick Plot to Visulize Data ######################
ggplot()+
  geom_point(data = Qall, aes(x = discharge.m.s, y = gauge.m, color = event))+
  facet_wrap(~site, scales = "free")+
  geom_smooth(method = "lm", col = "blue") 

##################### Functions #############################
###### Non-Linear Model #######
nlm_dis <- function(df, site_num){
  #Create a dataframe from the site of interest
  df <-  df %>% filter(df$site == site_num)
  #Quick plot to check the data
  plot(df$gauge.m, df$discharge.m.s)
  
  lm_init <- lm(log(discharge.m.s) ~ log(gauge.m), data = df)
  coefficients <- coef(lm_init)
  intercept <- coefficients[1]
  slope <- coefficients[2]
  model <- nlsLM(discharge.m.s ~ a * (gauge.m -h0)^b,
                 data = df,
                 start = list(a = intercept, b = slope, h0 = 0),
                 control = nls.lm.control(maxiter = 100))
  
  h_seq <- seq(min(df$gauge.m), max(df$gauge.m), length.out = 100)
  Q_pred <- predict(model, newdata = data.frame(gauge.m = h_seq))
  
  ggplot()+
    geom_point(data = df, aes(x = gauge.m, y = discharge.m.s))+
    labs(x ="Stage (m)", 
         y = "Discharge (m³/s)")+
    geom_line(aes(x = h_seq, y = Q_pred))
              
  coefficients <- coef(model)
  model_coeff <- data_frame(a = coefficients[1],
                            b = coefficients[2],
                            h0 = coefficients[3])
  return(model_coeff)
}

##### Linear Model #####
# Will only use this temporarily until more discharge measurements are made in
# 2026.
lm_dis <- function(df, site_num){
  #Create a dataframe from the site of interest
  df <-  df %>% filter(df$site == site_num)
  #Quick plot to check the data
  plot(df$gauge.m, df$discharge.m.s)
  model <- lm(discharge.m.s ~ gauge.m, data = df)
  h_seq <- seq(min(df$gauge.m), max(df$gauge.m), length.out = 100)
  Q_pred <- predict(model, newdata = data.frame(gauge.m = h_seq))
  
  ggplot()+
    geom_point(data = df, aes(x = gauge.m, y = discharge.m.s, color = "Observed Data"))+
    labs(x ="Stage (m)", 
         y = "Discharge (m³/s)")+
    geom_line(aes(x = h_seq, y = Q_pred))
  
  coefficients <- coef(model)
  model_coeff <- data_frame(intercept = coefficients[1],
                            slope = coefficients[2])
  return(model_coeff)
}
############### Functions for Plots
###### Non-Linear Model #######
nlm_dis_plot <- function(df, site_num){
  #Create a dataframe from the site of interest
  df <-  df %>% filter(df$site == site_num)
  #Quick plot to check the data
  plot(df$gauge.m, df$discharge.m.s)
  
  lm_init <- lm(log(discharge.m.s) ~ log(gauge.m), data = df)
  coefficients <- coef(lm_init)
  intercept <- coefficients[1]
  slope <- coefficients[2]
  model <- nlsLM(discharge.m.s ~ a * (gauge.m -h0)^b,
                 data = df,
                 start = list(a = intercept, b = slope, h0 = 0),
                 control = nls.lm.control(maxiter = 100))
  
  h_seq <- seq(min(df$gauge.m), max(df$gauge.m), length.out = 100)
  Q_pred <- predict(model, newdata = data.frame(gauge.m = h_seq))
  
  plot <- ggplot()+
    geom_point(data = df, aes(x = gauge.m, y = discharge.m.s))+
    labs(x ="Stage (m)", 
         y = "Discharge (m³/s)")+
    geom_line(aes(x = h_seq, y = Q_pred))+
    theme_classic()+ 
    scale_y_continuous(breaks = scales::pretty_breaks(n = 4),
                       labels = scales::number_format(accuracy = 0.001))+ 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4),
                       labels = scales::number_format(accuracy = 0.01))+
    ggtitle(paste("Rating Curve for Site", site_num))
  return(plot)
}

##### Linear Model #####
# Will only use this temporarily until more discharge measurements are made in
# 2026.
lm_dis_plot <- function(df, site_num){
  #Create a dataframe from the site of interest
  df <-  df %>% filter(df$site == site_num)
  #Quick plot to check the data
  plot(df$gauge.m, df$discharge.m.s)
  model <- lm(discharge.m.s ~ gauge.m, data = df)
  h_seq <- seq(min(df$gauge.m), max(df$gauge.m), length.out = 100)
  Q_pred <- predict(model, newdata = data.frame(gauge.m = h_seq))
  
  plot <- ggplot()+
    geom_point(data = df, aes(x = gauge.m, y = discharge.m.s))+
    labs(x ="Stage (m)", 
         y = "Discharge (m³/s)")+
    geom_line(aes(x = h_seq, y = Q_pred))+
    theme_classic()+ 
    scale_y_continuous(breaks = scales::pretty_breaks(n = 4),
                       labels = scales::number_format(accuracy = 0.001))+ 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4),
                       labels = scales::number_format(accuracy = 0.01))+
    ggtitle(paste("Rating Curve for Site", site_num))
  return(plot)
}

pressure_to_discharge_nlm <- function(data_baro, skip_baro, data_head, skip_head, coef_df){
  baro <- read.csv(data_baro, skip = skip_baro) %>% 
    reframe(#Converting mm.hg to psi to water column equvalent (m)
           #https://www.solinst.com/products/dataloggers-and-telemetry/3001-levelogger-series/operating-instructions/user-guide/8-data-compensation/8-2-manual-barometric-compensation.php
           baro_meter = Barometric.Pressure..mmHg. *  0.01933678 * 0.703070,
           Date.and.Time = as.POSIXct(Date.and.Time, format="%m/%d/%Y %H:%M:%S"))
  
  df <- read.csv(data_head, skip = skip_head) %>% 
    mutate(Date.and.Time = as.POSIXct(Date.and.Time, format="%m/%d/%Y %H:%M:%S")) %>% 
    merge(baro) %>% 
    mutate(depth.m = Depth..cm./100,
           depth.m = depth.m - baro_meter,
           discharge = coef_df$a * (depth.m - coef_df$h0)^coef_df$b)
    
  return(df)
}

pressure_to_discharge_lm <- function(data_baro, skip_baro, data_head, skip_head, coef_df){
  baro <- read.csv(data_baro, skip = skip_baro) %>% 
    reframe(#Converting mm.hg to psi to water column equvalent (m)
      #https://www.solinst.com/products/dataloggers-and-telemetry/3001-levelogger-series/operating-instructions/user-guide/8-data-compensation/8-2-manual-barometric-compensation.php
      baro_meter = Barometric.Pressure..mmHg. *  0.01933678 * 0.703070,
      Date.and.Time = as.POSIXct(Date.and.Time, format="%m/%d/%Y %H:%M:%S"))
  
  df <- read.csv(data_head, skip = skip_head) %>% 
    mutate(Date.and.Time = as.POSIXct(Date.and.Time, format="%m/%d/%Y %H:%M:%S")) %>% 
    merge(baro) %>% 
    mutate(depth.m = Depth..cm./100,
           depth.m = depth.m - baro_meter,
           discharge = coef_df$intercept + (depth.m * coef_df$slope))
  
  return(df)
}

if (data_baro == "North.Moran.Baro_Append_2025-08-12_12-32-13-014.csv") {
  baro <- read.csv(data_baro, skip = skip_baro) %>% 
    reframe(#Converting mm.hg to psi to water column equvalent (m)
      #https://www.solinst.com/products/dataloggers-and-telemetry/3001-levelogger-series/operating-instructions/user-guide/8-data-compensation/8-2-manual-barometric-compensation.php
      baro_meter = Barometric.Pressure..mmHg. *  0.01933678 * 0.703070,
      Date.and.Time = as.POSIXct(Date.and.Time, format="%m/%d/%Y %H:%M:%S"))
  
  df <- read.csv(data_head, skip = skip_head) %>% 
    mutate(Date.and.Time = as.POSIXct(Date.and.Time, format="%m/%d/%Y %H:%M:%S")) %>% 
    merge(baro) %>% 
    mutate(depth.m = Depth..cm./100,
           depth.m = depth.m - baro_meter,
           discharge = coef_df$intercept + (depth.m * coef_df$slope))
  
} else {
  baro <- read.csv(data_baro, skip = skip_baro) %>% 
    reframe(#Converting mm.hg to psi to water column equvalent (m)
      #https://www.solinst.com/products/dataloggers-and-telemetry/3001-levelogger-series/operating-instructions/user-guide/8-data-compensation/8-2-manual-barometric-compensation.php
      baro_meter = Barometric.Pressure..mmHg. *  0.01933678 * 0.703070,
      Date.and.Time = as.POSIXct(Date.and.Time, format="%m/%d/%Y %H:%M:%S"))
  
  df <- read.csv(data_head, skip = skip_head) %>% 
    mutate(Date.and.Time = as.POSIXct(Date.and.Time, format="%m/%d/%Y %H:%M:%S")) %>% 
    merge(baro) %>% 
    mutate(depth.m = Depth..cm./100,
           depth.m = depth.m - baro_meter,
           discharge = coef_df$intercept + (depth.m * coef_df$slope))
}

######## Model Plots #########
plot_10 <-lm_dis_plot(Qall, 10)
plot_16 <- nlm_dis_plot(Qall, 16)
plot_17 <- lm_dis_plot(Qall, 17)
plot_19 <- lm_dis_plot(Qall, 19)
plot_20 <- lm_dis_plot(Qall, 20)
plot_22 <- lm_dis_plot(Qall, 22)
plot_27 <- nlm_dis_plot(Qall, 27)
plot_30 <- lm_dis_plot(Qall, 30)
plot_38 <- nlm_dis_plot(Qall, 38)
plot_40 <- nlm_dis_plot(Qall, 40)

######### Model Coefficients ########
coef_10 <-lm_dis(Qall, 10)
coef_16 <- nlm_dis(Qall, 16)
coef_17 <- lm_dis(Qall, 17)
coef_19 <- lm_dis(Qall, 19)
coef_20 <- lm_dis(Qall, 20)
coef_22 <- lm_dis(Qall, 22)
coef_27 <- nlm_dis(Qall, 27)
coef_30 <- lm_dis(Qall, 30)
coef_38 <- nlm_dis(Qall, 38)
coef_40 <- nlm_dis(Qall, 40)

########### Data and Model Plots
ggarrange(plot_10, plot_16, plot_17, plot_19, plot_20, plot_22, plot_27, plot_30,
          plot_38, plot_40, ncol = 5, nrow =2)


site_10 <- pressure_to_discharge_lm("Arizona.Creek.Baro_2025-08-11_19-45-18-131.csv", 63,
                                    "Arizona.Creek_2025-08-11_19-34-09-111.csv",
                                    69, coef_10)
site_16 <- pressure_to_discharge_nlm("North.Moran.Baro_Append_2025-08-12_12-32-13-014.csv", 64,
                                     "Polecat.Creek_Append_2025-08-11_18-34-20-754.csv",
                                     70, coef_16)
site_17<- pressure_to_discharge_lm("North.Moran.Baro_Append_2025-08-12_12-32-13-014.csv", 64,
                                   "Glade.Creek_Append_2025-08-11_17-02-01-504.csv",
                                   72, coef_17)
# site_19<- pressure_to_discharge_lm("North.Moran.Baro_Append_2025-08-12_12-32-13-014.csv", 64,
#                                    "Bear.Paw.Creek_Append_2025-08-12_10-15-25-429.csv",
#                                    70, coef_30)
site_20<- pressure_to_discharge_lm("North.Moran.Baro_Append_2025-08-12_12-32-13-014.csv", 64,
                                   "Moose.Creek_Append_2025-08-12_15-09-09-375.csv",
                                   70, coef_20)
site_22<- pressure_to_discharge_lm("North.Moran.Baro_Append_2025-08-12_12-32-13-014.csv", 64,
                                   "Colter.Canyon_Append_2025-08-12_14-09-54-941.csv",
                                   70, coef_22)
site_27 <- pressure_to_discharge_nlm("North.Moran.Baro_Append_2025-08-12_12-32-13-014.csv", 64,
                                     "Moran.Creek_Append_2025-08-12_11-23-07-530.csv",
                                     70, coef_38)
site_30 <- pressure_to_discharge_lm("North.Moran.Baro_Append_2025-08-12_12-32-13-014.csv", 64,
                                    "Bear.Paw.Creek_Append_2025-08-12_10-15-25-429.csv",
                                    70, coef_30)
site_38 <- pressure_to_discharge_nlm("North.Moran.Baro_Append_2025-08-12_12-32-13-014.csv", 64,
                                     "Waterfalls.Canyon_Append_2025-08-12_13-26-01-870.csv",
                                     71, coef_38)
site_40 <- pressure_to_discharge_nlm("North.Moran.Baro_Append_2025-08-12_12-32-13-014.csv", 64,
                                     "North.Moran_Append_2025-08-12_12-15-13-793.csv",
                                     70, coef_40)

ggplot()+
  geom_line(data = site_16, aes(x = Date.and.Time, y = discharge))
ggplot()+
 geom_line(data = site_17, aes(x = Date.and.Time, y = discharge))
ggplot()+
    geom_line(data = site_20, aes(x = Date.and.Time, y = discharge))
ggplot()+
  geom_line(data = site_22, aes(x = Date.and.Time, y = discharge))
ggplot()+            
  geom_line(data = site_27, aes(x = Date.and.Time, y = discharge))
ggplot()+  
  geom_line(data = site_30, aes(x = Date.and.Time, y = discharge))
ggplot()+
  geom_line(data = site_38, aes(x = Date.and.Time, y = discharge))
ggplot()+
  geom_line(data = site_40, aes(x = Date.and.Time, y = discharge))


