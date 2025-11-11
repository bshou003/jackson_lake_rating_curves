##################### Modeling Functions #############################
####### The following two functions are used to force the intercept
####### to zero. Currently employed given how few discharge measurements
####### that have been made. As of now several locations use a linear function
####### until additional measurements can be collected in 2026, this will be used
###### Non-Linear Model #######
nlm_dis_forced_zero <- function(df, site_num){
  #Create a dataframe from the site of interest
  df <-  df %>% filter(df$site == site_num)
  #Quick plot to check the data
  plot(df$gauge.m, df$discharge.m.s)
  
  lm_init <- lm(log(discharge.m.s) ~ log(gauge.m), data = df)
  coefficients <- coef(lm_init)
  intercept <- coefficients[1]
  slope <- coefficients[2]
  model <- nlsLM(discharge.m.s ~ a * (gauge.m)^b,
                 data = df,
                 start = list(a = intercept, b = slope),
                 control = nls.lm.control(maxiter = 100))
  
  h_seq <- seq(min(df$gauge.m), max(df$gauge.m), length.out = 100)
  Q_pred <- predict(model, newdata = data.frame(gauge.m = h_seq))
  
  ggplot()+
    geom_point(data = df, aes(x = gauge.m, y = discharge.m.s))+
    labs(y = "Stage Height (m)",
         x = expression(paste("Discharge"(m^3/s))))+
    geom_line(aes(x = h_seq, y = Q_pred))
  
  coefficients <- coef(model)
  model_coeff <- data_frame(a = coefficients[1],
                            b = coefficients[2])
  return(model_coeff)
}

##### Linear Model #####
# Will only use this temporarily until more discharge measurements are made in
# 2026.
lm_dis_forced_zero <- function(df, site_num){
  #Create a dataframe from the site of interest
  df <-  df %>% filter(df$site == site_num)
  #Quick plot to check the data
  plot(df$gauge.m, df$discharge.m.s)
  model <- lm(discharge.m.s ~ 0 + gauge.m, data = df)
  h_seq <- seq(min(df$gauge.m), max(df$gauge.m), length.out = 100)
  Q_pred <- predict(model, newdata = data.frame(gauge.m = h_seq))
  
  ggplot()+
    geom_point(data = df, aes(x = gauge.m, y = discharge.m.s, color = "Observed Data"))+
    labs(x ="Stage (m)", 
         y = "Discharge (m³/s)")+
    geom_line(aes(x = h_seq, y = Q_pred))
  
  coefficients <- coef(model)
  model_coeff <- data_frame(intercept = 0,
                            slope = coefficients[1])
  return(model_coeff)
}
####### The following two functions do not force the intercept. 
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
    labs(y = "Stage Height (m)",
         x = expression(paste("Discharge"(m^3/s))))+
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

##################### Plotting Functions #############################
####### The following two functions are used to force the intercept
####### to zero. Currently employed given how few discharge measurements
####### that have been made. As of now several locations use a linear function
####### until additional measurements can be collected in 2026, this will be used
############### Functions for Plots
###### Non-Linear Model #######
nlm_dis_plot_forced_zero <- function(df, site_num){
  #Create a dataframe from the site of interest
  df <-  df %>% filter(df$site == site_num)
  #Quick plot to check the data
  plot(df$gauge.m, df$discharge.m.s)
  
  lm_init <- lm(log(discharge.m.s) ~ log(gauge.m), data = df)
  coefficients <- coef(lm_init)
  intercept <- coefficients[1]
  slope <- coefficients[2]
  model <- nlsLM(discharge.m.s ~ a * (gauge.m)^b,
                 data = df,
                 start = list(a = intercept, b = slope),
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
lm_dis_plot_forced_zero <- function(df, site_num){
  #Create a dataframe from the site of interest
  df <-  df %>% filter(df$site == site_num)
  #Quick plot to check the data
  plot(df$gauge.m, df$discharge.m.s)
  model <- lm(discharge.m.s ~ 0 + gauge.m, data = df)
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

####### The following two functions do not force the intercept.
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
############ Converting the pressure head to Discharge #################
#Converting the non-linear model
pressure_to_discharge_nlm <- function(data_baro, skip_baro, data_head, skip_head, coef_df){
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
             discharge = coef_df$a * (depth.m)^coef_df$b)
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
             discharge = coef_df$a * (depth.m)^coef_df$b)
  }
  return(df)
}

#Converting the linear model
pressure_to_discharge_lm <- function(data_baro, skip_baro, data_head, skip_head, coef_df){
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
      reframe(#Converting Bar to mBar to water column equvalent (m)
        #https://www.solinst.com/products/dataloggers-and-telemetry/3001-levelogger-series/operating-instructions/user-guide/8-data-compensation/8-2-manual-barometric-compensation.php
        baro_meter = Barometric.Pressure..Bar. *  1000 * 0.0101972,
        Date.and.Time = as.POSIXct(Date.and.Time, format="%m/%d/%Y %H:%M:%S"))
    
    df <- read.csv(data_head, skip = skip_head) %>% 
      mutate(Date.and.Time = as.POSIXct(Date.and.Time, format="%m/%d/%Y %H:%M:%S")) %>% 
      merge(baro) %>% 
      mutate(depth.m = Depth..cm./100,
             depth.m = depth.m - baro_meter,
             discharge = coef_df$intercept + (depth.m * coef_df$slope))
  }
  return(df)
}
