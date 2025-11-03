library(minpack.lm)
library(tidyverse)
library(lubridate)
library(zoo)
library(here)
library(broom)

# This steps gets a list of all files in the Data/Velocity.Profiling folder. This will generate a vector with 3 entries, one for each of the cross sections you did. here() references the folder the csv files are stored in.
files <- list.files(here("~/Documents/Data/SEaSON/Data"), recursive=TRUE, pattern = ".csv", full.names=FALSE)

# create empty df with one column Qls that will be filled with discharge values
Qall <- tibble(Qls=numeric())
for (ii in files) {
  
  # read in each file; the ii is the name of the file to be read in during each iteration
  Qdat <- read_csv(here("~/Documents/Data/SEaSON/Data", ii), show_col_types = FALSE)
  
  # calculate difference between subsequent values and divide by two
  lag1_diff <- diff(Qdat$Width.m, lag = 1) / 2
  # calculate difference between three values and divide by two
  lag2_diff <- diff(Qdat$Width.m, lag = 2) / 2
  # combine the two so that the first and last values are taken from lag1_diff and the inner values are taken from lag2_diff
  Width.Inc.m = c(lag1_diff[1], lag2_diff, tail(lag1_diff, n = 1))
  
  Qdat <- Qdat %>%
    mutate(
      # add the column with the incremental width for each segment
      Width.Inc.m = Width.Inc.m,
      # calculate segment area (incremental segment width * segment depth)
      Area.m2 = Width.Inc.m * Depth.m, 
      # calculate streamflow in LITERS per second (segment area * segment velocity *1000 (m3 to l))
      Q.l.s = Area.m2 * Velocity.m.s *1000
    ) %>% 
    # calculate discharge for the cross section (sum of all cross section Qs)
    summarise(Qls = sum(Q.l.s))
  
  # add the column data
  Qall <- add_row(Qall, Qdat)
  
}

### THIS IS ALL OUTSIDE OF THE FOR LOOP

# add a column with the measurement count
Qall <- rowid_to_column(Qall, "ID")

# calculate the mean and median Q and the standard deviation
Qsummary <- Qall %>% 
  summarise(
    Mean = (mean(Qls)),
    Median = median(Qls),
    Std.Dev = sd(Qls)
  )

# plot the three Q values as columns
ggplot(Qall, aes(ID, Qls)) +
  theme_bw(base_size = 15) +
  geom_bar(stat="identity") +
  labs(x="Measurement Number", y="Discharge (l/s)")



salt_dilution <- readxl::read_excel("~/Documents/Data/SEaSON/salt.dilution.discharge.values.xlsx")
stage_heights <- read.csv("~/Documents/Data/SEaSON/stage_heights.csv")

a <- files
Qall <- Qall %>% 
  cbind(a) %>% 
  separate_wider_delim(a, ".", names =  c("site", "event", "trash")) %>% 
  subset(select = -c(trash, ID)) %>% 
  rbind(salt_dilution) %>% 
  merge(stage_heights) %>% 
  mutate(discharge.m.s = Qls * 0.001,
         gauge.m = as.numeric(gauge.cm)/100) %>% 
  subset(select = -c(Qls, gauge.cm))

write_csv(Qall, "~/Documents/Data/SEaSON/discharge_stage_height.csv")