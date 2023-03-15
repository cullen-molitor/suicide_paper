# Verification for Small Test of EU Data
# Tyler Liddell
# March 12, 2023

# Load Libraries
library(tidyverse)
library(here)
library(sf)
library(lubridate)
library(raster)

# Read in Data
eu_polygons <- st_read(here("data/shapefile/NUTS2_RG_60M_2013_mortality.shp"))%>% 
  filter(startsWith(NUTS_ID, "UKI"))

pop_weighted <- read.csv(here("data/cluster_outputs/small_tests/temp_polynomial_1994_1996_EU_ERA5_pop.csv")) %>% 
  filter(year != 1996) %>% 
  rbind(read.csv(here("data/cluster_outputs/small_tests/temp_polynomial_1996_2000_EU_ERA5_pop.csv")),
        read.csv(here("data/cluster_outputs/small_tests/temp_polynomial_2001_2006_EU_ERA5_pop.csv")),
        read.csv(here("data/cluster_outputs/small_tests/temp_polynomial_2007_2010_EU_ERA5_pop.csv"))) %>% 
  mutate(day = 1) %>% 
  mutate(date = as_date(paste(year,month,day,sep = "-"))) %>% 
  mutate(ndays = days_in_month(month)) %>% 
  mutate(ndays = ifelse(leap_year(year) & month == 2, 29, ndays))%>% 
  mutate(avg_temp = order_1 / ndays)

crop_weighted <- read.csv(here("data/cluster_outputs/small_tests/temp_polynomial_1994_1996_EU_ERA5_crop.csv")) %>% 
  filter(year != 1996) %>% 
  rbind(read.csv(here("data/cluster_outputs/small_tests/temp_polynomial_1996_2000_EU_ERA5_crop.csv")),
        read.csv(here("data/cluster_outputs/small_tests/temp_polynomial_2001_2006_EU_ERA5_crop.csv")),
        read.csv(here("data/cluster_outputs/small_tests/temp_polynomial_2007_2010_EU_ERA5_crop.csv"))) %>% 
  mutate(day = 1) %>% 
  mutate(date = as_date(paste(year,month,day,sep = "-"))) %>% 
  mutate(ndays = days_in_month(month)) %>% 
  mutate(ndays = ifelse(leap_year(year) & month == 2, 29, ndays))%>% 
  mutate(avg_temp = order_1 / ndays)

# Plot data by polygon over years
ggplot(pop_weighted) +
  geom_line(aes(x = date, y = avg_temp, color = poly_id)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m")

ggplot(crop_weighted) +
  geom_line(aes(x = date, y = avg_temp, color = poly_id)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m")

#--- Each area almost lines up with the other


# Compare pop and crop weighted
sum_pop <- pop_weighted %>% 
  group_by(date) %>% 
  summarise(avg = mean(avg_temp))

sum_crop <- crop_weighted %>% 
  group_by(date) %>% 
  summarise(avg = mean(avg_temp))

ggplot() + 
  geom_line(data = sum_pop, mapping = aes(x = date, y = avg), size = 2) +
  geom_line(data = sum_crop, mapping = aes(x = date, y = avg), color = "green", linetype = "dashed")


# Nice Graph
ggplot(sum_pop) +
  geom_line(data = sum_pop, mapping = aes(x = date, y = avg), color = "deepskyblue4") +
  theme_classic() +
  labs(x = "Year",
       title = "Average Monthly Temperature In London",
       y = "\u00B0C") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))
  


  

