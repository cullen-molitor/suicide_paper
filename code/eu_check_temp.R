# Verification of Full Scale EU Temp Data
# Tyler Liddell
# April 11, 2023

# Load Libraries
library(tidyverse)
library(here)
library(sf)
library(lubridate)
library(raster)

# Read in Data
eu_polygons <- st_read(here("data/shapefile/NUTS2_RG_60M_2013_mortality.shp")) %>% 
  rename(poly_id = NUTS_ID)


pop_weighted_09_10 <- read.csv(here("data/cluster_outputs/temp/pop_weighted/temp_polynomial_2008_2010_EU_ERA5_pop.csv")) %>% 
  filter(year != 2008) # Removing duplicate data from 2008

pop_weighted <- rbind(read.csv(here("data/cluster_outputs/temp/pop_weighted/temp_polynomial_1994_1997_EU_ERA5_pop.csv")),
                      read.csv(here("data/cluster_outputs/temp/pop_weighted/temp_polynomial_1998_1999_EU_ERA5_pop.csv")),
                      read.csv(here("data/cluster_outputs/temp/pop_weighted/temp_polynomial_2000_2001_EU_ERA5_pop.csv")),
                      read.csv(here("data/cluster_outputs/temp/pop_weighted/temp_polynomial_2002_2005_EU_ERA5_pop.csv")),
                      read.csv(here("data/cluster_outputs/temp/pop_weighted/temp_polynomial_2006_2008_EU_ERA5_pop.csv")),
                      pop_weighted_09_10) %>% 
  mutate(day = 1) %>% 
  mutate(date = as_date(paste(year,month,day,sep = "-"))) %>% 
  mutate(ndays = days_in_month(month)) %>% 
  mutate(ndays = ifelse(leap_year(year) & month == 2, 29, ndays))%>% 
  mutate(avg_temp = order_1 / ndays)

rm(pop_weighted_09_10)

crop_weighted <- rbind(read.csv(here("data/cluster_outputs/temp/crop_weighted/temp_polynomial_1994_1996_EU_ERA5_crop.csv")),
                       read.csv(here("data/cluster_outputs/temp/crop_weighted/temp_polynomial_1997_1999_EU_ERA5_crop.csv")),
                       read.csv(here("data/cluster_outputs/temp/crop_weighted/temp_polynomial_2000_2002_EU_ERA5_crop.csv")),
                       read.csv(here("data/cluster_outputs/temp/crop_weighted/temp_polynomial_2003_2005_EU_ERA5_crop.csv")),
                       read.csv(here("data/cluster_outputs/temp/crop_weighted/temp_polynomial_2006_2008_EU_ERA5_crop.csv")),
                       read.csv(here("data/cluster_outputs/temp/crop_weighted/temp_polynomial_2009_2010_EU_ERA5_crop.csv"))) %>% 
  rename(order_1_crop = order_1,
         order_2_crop = order_2,
         order_3_crop = order_3,
         order_4_crop = order_4,
         order_5_crop = order_5)





old_results <- read.csv(here("data/old_results/eu_nuts2_era5_temp_average_1994_2010_polynomial_5_area_crop_weights.csv")) %>% 
  mutate(month = as.numeric(substr(month, 7,8))) %>% 
  rename(order_1_old = order_1,
         order_2_old = order_2,
         order_3_old = order_3,
         order_4_old = order_4,
         order_5_old = order_5)
  
  
data <- full_join(pop_weighted, old_results, c("year", "month", "poly_id")) %>% 
  full_join(crop_weighted, c("year", "month", "poly_id")) %>% 
  mutate(days_in_month = case_when(month %in% c(4, 6, 9, 11) ~ 30,
                                     month == 2 & year %in% c(1996, 2000, 2004, 2008) ~ 29,
                                     month == 2 & year %in% c(1994, 1995, 
                                                              1997:1999,
                                                              2001:2003,
                                                              2005:2007,
                                                              2009, 2010) ~ 28, 
                                     TRUE ~ 31)) %>% 
    dplyr::select(!(c(order_2, 
                      order_3, 
                      order_4,
                      order_5, 
                      order_2_old,
                      order_3_old,
                      order_4_old,
                      order_5_old,
                      order_2_crop,
                      order_3_crop,
                      order_4_crop,
                      order_5_crop))) %>% 
    mutate(order_1 = order_1 / days_in_month, order_1_old = order_1_old / days_in_month, order_1_crop = order_1_crop / days_in_month) %>% 
    mutate(date = as_date(paste0(year, ".", month, ".", "01"))) %>% 
    left_join(eu_polygons, by = "poly_id")

# Plot new data by polygon over years for random set of polygons
poly_list <- sample(unique(eu_polygons$poly_id), 5)

plot_data_pop1 <- pop_weighted %>% 
  filter(poly_id %in% poly_list)

pop_timeseries <- ggplot(plot_data_pop1) +
  geom_line(aes(x = date, y = avg_temp, color = poly_id))
            # Yearly cycles seem to be correct

ggsave(plot = pop_timeseries, filename = here("figs/eu_check/pop_timeseries.png"), width = 5, height = 5, units = "in")



# Scatter plot of old results vs new results 
pop_scatter <- ggplot(data) +
  geom_point(aes(order_1_old, order_1))

ggsave(plot = pop_scatter, filename = here("figs/eu_check/pop_scatter.png"))

crop_scatter <- ggplot(data) +
  geom_point(aes(order_1_old, order_1_crop), size = .1)

ggsave(plot = crop_scatter, filename = here("figs/eu_check/crop_scatter.png"))


# Map of old results vs new results
data_map1 <- data %>% 
  filter(month == 7, year == 2008) %>% 
  st_as_sf()

pop_map <- ggplot(data_map1) +
  geom_sf(aes(fill = avg_temp), color = NA) +
  scale_fill_viridis_c() +
  coord_sf(xlim = c(-10, 30), ylim = c(35, 70))

ggsave(plot = pop_map, filename = here("figs/eu_check/pop_map.png"), width = 5, height = 5, units = "in")
# Seems to be good


crop_map <- ggplot(data_map1) +
  geom_sf(aes(fill = order_1_crop), color = NA) +
  scale_fill_viridis_c() +
  coord_sf(xlim = c(-10, 30), ylim = c(35, 70))

ggsave(plot = crop_map, filename = here("figs/eu_check/crop_map.png"), width = 5, height = 5, units = "in")


old_map <- ggplot(data_map1) +
  geom_sf(aes(fill = order_1_old), color = NA) +
  scale_fill_viridis_c() +
  coord_sf(xlim = c(-10, 30), ylim = c(35, 70))

ggsave(plot = old_map, filename = here("figs/eu_check/old_map.png"), width = 5, height = 5, units = "in")

