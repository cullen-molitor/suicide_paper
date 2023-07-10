# Tyler Liddell
# Small Test Check for US Degree Days
# May 30, 2023

# Load Libraries
library(tidyverse)
library(here)
library(sf)
library(tigris)


# Read in Data and Merge
ca_counties <- counties("CA") %>% 
  mutate(poly_id = as.numeric(GEOID))

ca_deg_days <- read.csv(here("data/cluster_outputs/deg_days/small_tests/test_stagg_crop.csv"))

data <- full_join(ca_counties, ca_deg_days, by = "poly_id")


# Map the data for a cold month and a warm month
jan_86 <- data %>% 
  filter(year == 1986, month == 1) %>% 
  st_as_sf()



ggplot() +
  geom_sf(data = jan_86, aes(fill = threshold_ninf_to_8)) +
  scale_fill_viridis_c(trans = "log", option = "H", direction = -1) +
  theme_void() +
  labs(fill = "Degree Days Below 8C",
       title = "January 1986") 


jul_86 <- data %>% 
  filter(year == 1986, month == 7) %>% 
  st_as_sf()

ggplot() +
  geom_sf(data = jul_86, aes(fill = threshold_31_to_inf)) +
  scale_fill_viridis_c(trans = "log", option = "H") +
  theme_void() +
  labs(fill = "Degree Days Above 31C",
       title = "July 1986") 

