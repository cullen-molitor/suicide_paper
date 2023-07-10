# Cluster Script for EU Data 
# Tyler Liddell
# April 19, 2023


#============================ Set Up ===========================================

# Settings (Only things to change from run to run)
years <- 1994:1996
weights_var <- "pop"
small_test <- TRUE # If "TRUE", will filter to only London areas
poly_order <- 3

# File paths
data_folder <- "/home/tcarleton/Climate/data"
save_folder <- "/home/tliddell/suicide_paper/data/prcp"

if(small_test){
  save_folder <- file.path(save_folder, "small_tests")
} else{
  if(weights_var == "pop"){
    save_folder <- file.path(save_folder, "pop_weighted")
  }
  
  if(weights_var == "crop"){
    save_folder <- file.path(save_folder, "crop_weighted")
  }
}


setwd("/home/tliddell/suicide_paper/code")


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
input_polygons <- read_sf(here(data_folder, "shapefiles", "EU", "NUTS2_RG_60M_2013_mortality.shp"))

if(small_test){
  input_polygons <- input_polygons %>% 
    filter(startsWith(NUTS_ID, "UKI"))
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

polygon_weights <- overlay_weights(input_polygons, "NUTS_ID", secondary_weights = weights_table)

message(crayon::green("Weights successfully calculated"))

#============================== Staggregate ====================================
# Define data extent to cut in half and then merge to reduce memory load
poly_extent <- raster::extent(input_polygons)

data_extent_left <- raster::extent(poly_extent@xmin + 360 - .5, 360.1, poly_extent@ymin - .5, poly_extent@ymax + .5)

data_extent_right <- raster::extent(-.1, poly_extent@xmax, poly_extent@ymin -.5, poly_extent@ymax + .5)


# Define function to run stagg on prcp
run_stagg_year_prcp <- function(year) {  
  
  # climate data file paths
  ncpath  <- file.path(data_folder, 'raw/prcp')
  nc_file <- paste0(ncpath, '/', 'era5_prcp_', year, '.nc')
  
  # immediately crop to weights extent 
  clim_raster_left <- raster::crop(raster::stack(nc_file), data_extent_left)
  
  clim_raster_right <- raster::crop(raster::stack(nc_file), data_extent_right) 
  
  stack_names <- names(clim_raster_left)
  
  clim_raster <- merge(clim_raster_left, clim_raster_right)
  
  names(clim_raster) <- stack_names
  
  
  # convert from K to C
  clim_raster <- clim_raster - 273.15
  
  ## run stagg for prcp
  prcp_out <- staggregate_polynomial(clim_raster,
                                     polygon_weights,
                                     daily_agg = 'sum',
                                     time_agg = 'month',
                                     degree = poly_order)
  
  return(prcp_out)
  
}


## set up (cores, cluster)
message(crayon::yellow("Executing stagg in parallel"))

no_cores <- parallel::detectCores() - 1 # Calculate the number of cores. Leave one in case something else needs to be done on the same computer at the same time. 
cl <- parallel::makeCluster(no_cores, type = "FORK") # Initiate cluster. "FORK" means bring everything in your current environment with you. 

## run 
stagg_multiyear_prcp <- parLapply(cl, years, run_stagg_year_prcp)

## stop cluster
stopCluster(cl)

message(crayon::green("Staggegated completed. Saving output"))

## rbind
stagg_multiyear_prcp_all <- data.table::rbindlist(stagg_multiyear_prcp)

## save outputs
save_name <- paste0(paste("prcp", "polynomial", years[1], years[length(years)], "EU", "ERA5",
                          weights_var, sep="_"), ".csv")



## save message
message(crayon::yellow('Saving', save_name, 'to', save_folder))

## save output
data.table::fwrite(stagg_multiyear_prcp_all, file = file.path(save_folder, save_name))

## fin
message(crayon::green('fin'))
