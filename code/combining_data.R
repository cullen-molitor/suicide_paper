# Merge data
# Tyler Liddell
# April 25, 2023

library(dplyr)
library(here)


pop_weighted_09_10 <- read.csv(here("data/cluster_outputs/pop_weighted/temp_polynomial_2008_2010_EU_ERA5_pop.csv")) %>% 
  filter(year != 2008) # Removing duplicate data from 2008

pop_weighted <- rbind(read.csv(here("data/cluster_outputs/pop_weighted/temp_polynomial_1994_1997_EU_ERA5_pop.csv")),
                      read.csv(here("data/cluster_outputs/pop_weighted/temp_polynomial_1998_1999_EU_ERA5_pop.csv")),
                      read.csv(here("data/cluster_outputs/pop_weighted/temp_polynomial_2000_2001_EU_ERA5_pop.csv")),
                      read.csv(here("data/cluster_outputs/pop_weighted/temp_polynomial_2002_2005_EU_ERA5_pop.csv")),
                      read.csv(here("data/cluster_outputs/pop_weighted/temp_polynomial_2006_2008_EU_ERA5_pop.csv")),
                      pop_weighted_09_10)

crop_weighted <- rbind(read.csv(here("data/cluster_outputs/crop_weighted/temp_polynomial_1994_1996_EU_ERA5_crop.csv")),
                       read.csv(here("data/cluster_outputs/crop_weighted/temp_polynomial_1997_1999_EU_ERA5_crop.csv")),
                       read.csv(here("data/cluster_outputs/crop_weighted/temp_polynomial_2000_2002_EU_ERA5_crop.csv")),
                       read.csv(here("data/cluster_outputs/crop_weighted/temp_polynomial_2003_2005_EU_ERA5_crop.csv")),
                       read.csv(here("data/cluster_outputs/crop_weighted/temp_polynomial_2006_2008_EU_ERA5_crop.csv")),
                       read.csv(here("data/cluster_outputs/crop_weighted/temp_polynomial_2009_2010_EU_ERA5_crop.csv")))


write.csv(pop_weighted, file = here("data/cluster_outputs/combined_outputs/eu_nuts2_temp_average_1994_2010_polynomial_5_area_pop_weights.csv"))
write.csv(crop_weighted, file = here("data/cluster_outputs/combined_outputs/eu_nuts2_temp_average_1994_2010_polynomial_5_area_crop_weights.csv"))
