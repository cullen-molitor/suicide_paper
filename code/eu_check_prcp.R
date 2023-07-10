# Verification of Full Scale EU Prcp Data
# Tyler Liddell
# May 19, 2023

# Load Libraries
library(tidyverse)
library(here)
library(sf)
library(lubridate)
library(raster)

#=======================================   Read in Data =================================

# EU areas
eu_polygons <- st_read(here("data/shapefile/NUTS2_RG_60M_2013_mortality.shp")) %>% 
  rename(poly_id = NUTS_ID)

# Pop weighted prcp
pop_weighted <- rbind(read.csv(here("data/cluster_outputs/prcp/pop_weighted/prcp_polynomial_1994_1996_EU_ERA5_pop.csv")),
                      read.csv(here("data/cluster_outputs/prcp/pop_weighted/prcp_polynomial_1997_1999_EU_ERA5_pop.csv")),
                      read.csv(here("data/cluster_outputs/prcp/pop_weighted/prcp_polynomial_2000_2002_EU_ERA5_pop.csv")),
                      read.csv(here("data/cluster_outputs/prcp/pop_weighted/prcp_polynomial_2003_2005_EU_ERA5_pop.csv")),
                      read.csv(here("data/cluster_outputs/prcp/pop_weighted/prcp_polynomial_2006_2008_EU_ERA5_pop.csv")),
                      read.csv(here("data/cluster_outputs/prcp/pop_weighted/prcp_polynomial_2009_2010_EU_ERA5_pop.csv"))) %>% 
  mutate(day = 1) %>% 
  mutate(date = as_date(paste(year,month,day,sep = "-"))) %>% 
  mutate(ndays = days_in_month(month)) %>% 
  mutate(ndays = ifelse(leap_year(year) & month == 2, 29, ndays))%>% 
  mutate(avg_prcp_pop = order_1 / ndays) %>% 
  dplyr::select(year, month, poly_id, avg_prcp_pop, date)

# Crop weighted prcp
crop_weighted <- rbind(read.csv(here("data/cluster_outputs/prcp/crop_weighted/prcp_polynomial_1994_1996_EU_ERA5_crop.csv")),
                       read.csv(here("data/cluster_outputs/prcp/crop_weighted/prcp_polynomial_1997_1999_EU_ERA5_crop.csv")),
                       read.csv(here("data/cluster_outputs/prcp/crop_weighted/prcp_polynomial_2000_2002_EU_ERA5_crop.csv")),
                       read.csv(here("data/cluster_outputs/prcp/crop_weighted/prcp_polynomial_2003_2005_EU_ERA5_crop.csv")),
                       read.csv(here("data/cluster_outputs/prcp/crop_weighted/prcp_polynomial_2006_2008_EU_ERA5_crop.csv")),
                       read.csv(here("data/cluster_outputs/prcp/crop_weighted/prcp_polynomial_2009_2010_EU_ERA5_crop.csv"))) %>% 
  mutate(day = 1) %>% 
  mutate(date = as_date(paste(year,month,day,sep = "-"))) %>% 
  mutate(ndays = days_in_month(month)) %>% 
  mutate(ndays = ifelse(leap_year(year) & month == 2, 29, ndays)) %>% 
  mutate(avg_prcp_crop = order_1 / ndays) %>% 
  dplyr::select(year, month, poly_id, avg_prcp_crop, date)

old_results <- read.csv(here("data/old_results/eu_nuts2_era5_prcp_sum_1994_2010_polynomial_3_area_crop_weights.csv")) %>% 
  mutate(day = 1) %>% 
  mutate(month = as.numeric(substring(month, 7,8))) %>% 
  mutate(date = as_date(paste(year,month,day,sep = "-"))) %>% 
  mutate(ndays = days_in_month(month)) %>% 
  mutate(ndays = ifelse(leap_year(year) & month == 2, 29, ndays)) %>% 
  mutate(avg_prcp_old = order_1 / ndays) %>% 
  dplyr::select(year, month, poly_id, avg_prcp_old, date)

# Combine
data <- full_join(pop_weighted, crop_weighted, by = c("poly_id", "year", "month", "date")) %>% 
  full_join(eu_polygons, by = "poly_id") %>% 
  full_join(old_results, by = c("poly_id", "year", "month", "date"))


#========================================= Plots =====================================

# Time-series
data_timeseries <- data %>% 
  group_by(date) %>% 
  summarise(daily_avg_prcp_pop = mean(avg_prcp_pop),
            daily_avg_prcp_crop = mean(avg_prcp_crop))

ggplot(data_timeseries) +
  geom_line(mapping = aes(x = date, y = daily_avg_prcp_pop), color = "darkblue") +
  geom_line(mapping = aes(x = date, y = daily_avg_prcp_crop), color = "darkgreen") +
  theme_light() +
  labs(x = "Daily Average Precipitation in EU",
       y = "Date",
       caption = "Blue: Pop Weights, Green: Crop Weights")

# Map
data_map <- data %>% 
  group_by(poly_id, geometry) %>% 
  summarise(poly_avg_prcp_pop = mean(avg_prcp_pop),
            poly_avg_prcp_crop = mean(avg_prcp_crop)) %>% 
  st_as_sf()
  
ggplot(data_map) +
  geom_sf(aes(fill = poly_avg_prcp_pop), color = NA) +
  scale_fill_gradient(low = "white", high = "blue") +
  coord_sf(xlim = c(-10, 30), ylim = c(35, 70)) +
  theme_void() +
  labs(fill = "Daily Average Precipitation")

ggplot(data_map) +
  geom_sf(aes(fill = poly_avg_prcp_crop), color = NA) +
  scale_fill_gradient(low = "white", high = "blue", na.value = "grey") +
  coord_sf(xlim = c(-10, 30), ylim = c(35, 70)) +
  theme_void() +
  labs(fill = "Daily Average Precipitation")

# Scatterplot
ggplot(data) +
  geom_point(aes(avg_prcp_old, avg_prcp_crop)) +
  theme_light() +
  labs(x = "Old Results (Crop Weighted 2003)",
       y = "New Results (Crop Weighted 2015)")

ggplot(data) +
  geom_point(aes(avg_prcp_old, avg_prcp_pop)) +
  theme_light() +
  labs(x = "Old Results (Crop Weighted 2003)",
       y = "New Results (Pop Weighted 2015)")




# Merge all of the data together and save
pop_weighted_raw <- rbind(read.csv(here("data/cluster_outputs/prcp/pop_weighted/prcp_polynomial_1994_1996_EU_ERA5_pop.csv")),
                          read.csv(here("data/cluster_outputs/prcp/pop_weighted/prcp_polynomial_1997_1999_EU_ERA5_pop.csv")),
                          read.csv(here("data/cluster_outputs/prcp/pop_weighted/prcp_polynomial_2000_2002_EU_ERA5_pop.csv")),
                          read.csv(here("data/cluster_outputs/prcp/pop_weighted/prcp_polynomial_2003_2005_EU_ERA5_pop.csv")),
                          read.csv(here("data/cluster_outputs/prcp/pop_weighted/prcp_polynomial_2006_2008_EU_ERA5_pop.csv")),
                          read.csv(here("data/cluster_outputs/prcp/pop_weighted/prcp_polynomial_2009_2010_EU_ERA5_pop.csv")))

write.csv(pop_weighted_raw, file = here("data/cluster_outputs/prcp/combined_outputs/eu_nuts2_prcp_average_1994_2010_polynomial_5_area_pop_weights.csv"))

crop_weighted_raw <- rbind(read.csv(here("data/cluster_outputs/prcp/crop_weighted/prcp_polynomial_1994_1996_EU_ERA5_crop.csv")),
                           read.csv(here("data/cluster_outputs/prcp/crop_weighted/prcp_polynomial_1997_1999_EU_ERA5_crop.csv")),
                           read.csv(here("data/cluster_outputs/prcp/crop_weighted/prcp_polynomial_2000_2002_EU_ERA5_crop.csv")),
                           read.csv(here("data/cluster_outputs/prcp/crop_weighted/prcp_polynomial_2003_2005_EU_ERA5_crop.csv")),
                           read.csv(here("data/cluster_outputs/prcp/crop_weighted/prcp_polynomial_2006_2008_EU_ERA5_crop.csv")),
                           read.csv(here("data/cluster_outputs/prcp/crop_weighted/prcp_polynomial_2009_2010_EU_ERA5_crop.csv")))


write.csv(crop_weighted_raw, file = here("data/cluster_outputs/prcp/combined_outputs/eu_nuts2_prcp_average_1994_2010_polynomial_5_area_crop_weights.csv"))
  
