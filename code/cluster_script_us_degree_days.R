# Cluster Script for USA Temperature Data 
# Tyler Liddell
# May 28, 2023


#============================ Set Up ===========================================

# Settings (Only things to change from run to run)
years <- 1986:1999
weights_var <- "crop"
small_test <- TRUE # If "TRUE", will filter to only London areas
thresholds <- c(8,9,10,29,30,31)

# File paths
data_folder <- "/home/tcarleton/Climate/data"
save_folder <- "/home/tliddell/suicide_paper/data/degree_days"

if(small_test){
  save_folder <- file.path(save_folder, "small_tests")
}

#setwd("/home/tliddell/suicide_paper/code")


# Load Package
library(stagg)
library(tidyverse)
library(sf)
library(haven)
library(here)
library(data.table)
library(parallel)
library(crayon)
library(raster)

message(crayon::green( "Packages successfully attached"))

#======================== Overlay Weights ======================================
# Get EU polygons
#input_polygons <- read_sf(here(data_folder, "shapefiles", "USA", "tl_2016_us_county_mortality.shp"))
input_polygons <- read_sf(here("data/shapefile/USA/tl_2016_us_county_mortality.shp"))

if(small_test){
  input_polygons <- input_polygons %>% 
    filter(startsWith(GEOID, "06"))
}

# Get secondary_weights
if(weights_var == "pop"){
  weights_table <- pop_world_2015_era5
}

if(weights_var == "crop"){
  weights1 <- read.csv(file.path(data_folder, "int/rasterweights/era5_cropland_NW_2015_full.csv"))
  weights2 <- read.csv(file.path(data_folder, "int/rasterweights/era5_cropland_NE_2015_full.csv"))
  weights3 <- read.csv(file.path(data_folder, "int/rasterweights/era5_cropland_SE_2015_full.csv"))
  weights4 <- read.csv(file.path(data_folder, "int/rasterweights/era5_cropland_SW_2015_full.csv"))
  
  weights_table <- rbind(weights1, weights2, weights3, weights4)
}




# Calculate Overlay Weights 
message(crayon::yellow("Calculating overlay weights"))

polygon_weights <- overlay_weights(input_polygons, "GEOID", secondary_weights = cropland_world_2015_era5)

message(crayon::green("Weights successfully calculated"))

#============================== Staggregate ====================================
# Define data extent to cut in half and then merge to reduce memory load
poly_extent <- raster::extent(input_polygons)

data_extent_left <- raster::extent(poly_extent@xmin + 360 - .5, 360.1, poly_extent@ymin - .5, poly_extent@ymax + .5)

data_extent_right <- raster::extent(-.1, poly_extent@xmax, poly_extent@ymin -.5, poly_extent@ymax + .5)


# Define funciton to run stagg on temp
run_stagg_year_temp <- function(year) {  
  
  # climate data file paths
  ncpath  <- file.path(data_folder, 'raw/temp')
  nc_file <- paste0(ncpath, '/', 'era5_temp_', year, '.nc')
  
  # immediately crop to weights extent 
  clim_raster_left <- raster::crop(raster::stack(nc_file), data_extent_left)
  
  clim_raster_right <- raster::crop(raster::stack(nc_file), data_extent_right) 
  
  stack_names <- names(clim_raster_left)
  
  clim_raster <- merge(clim_raster_left, clim_raster_right)
  
  names(clim_raster) <- stack_names
  
  
  # convert from K to C
  clim_raster <- clim_raster - 273.15
  
  ## run stagg for temp
  temp_out <- staggregate_degree_days(clim_raster,
                                      polygon_weights,
                                      time_agg = "month",
                                      thresholds = thresholds)
  
  return(temp_out)
  
}


## set up (cores, cluster)
message(crayon::yellow("Executing stagg in parallel"))

no_cores <- parallel::detectCores() - 1 # Calculate the number of cores. Leave one in case something else needs to be done on the same computer at the same time. 
cl <- parallel::makeCluster(no_cores, type = "FORK") # Initiate cluster. "FORK" means bring everything in your current environment with you. 

## run 
stagg_multiyear_temp <- parLapply(cl, years, run_stagg_year_temp)

## stop cluster
stopCluster(cl)

message(crayon::green("Staggegate completed. Saving output"))

## rbind
stagg_multiyear_temp_all <- data.table::rbindlist(stagg_multiyear_temp)

## save outputs
save_name <- paste0(paste("temp", "degree_days", years[1], years[length(years)], "US", "ERA5",
                          weights_var, sep="_"), ".csv")



## save message
message(crayon::yellow('Saving', save_name, 'to', save_folder))

## save output
data.table::fwrite(stagg_multiyear_temp_all, file = file.path(save_folder, save_name))

## fin
message(crayon::green('fin'))
