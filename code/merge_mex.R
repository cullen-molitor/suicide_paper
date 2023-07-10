# Merge Mexico Codes
# Tyler Liddell
# April 28, 2023

# Load Libraries
library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(scraEP)
library(data.table)

# Read in Raw Data
file1 <- read_excel(here("data/merge_mex/geo2_mx1960_2015.xlsx")) %>% 
  clean_names() %>% 
  as.data.table()
file2 <- read_excel(here("data/merge_mex/geocode_suicidedata.xlsx")) %>% 
  clean_names() %>% 
  as.data.table()


# It seems the state numbers 1-32 align as they are assigned alphebetically 

# # Testing to see what the issues are in the geocodes
# geocode_merge <- file1 %>% 
#   mutate(adm2code = as.numeric(substr(geolevel2, 7, 9)), adm1code = as.numeric(amd1_code)) %>% 
#   full_join(file2, by = c("adm1code", "adm2code")) %>% 
#   mutate(no_accent = unaccent(adm2_2019)) %>%
#   mutate(matches = ifelse(admin_name == no_accent, 1, 0)) %>% 
#   select(admin_name, no_accent, matches, geolevel2)
# 
# nas_geo <- geocode_merge %>% 
#   filter(is.na(matches))
# 
# percent_right = 100*mean(geocode_merge$matches, na.rm = TRUE)
# 
# # Only about 57% of the codes line up correctly






# # Testing to see if we can merge by unaccented names
# names_merge <- file2 %>% 
#   mutate(geocode = paste0("484", sprintf("%03d", adm1code), sprintf("%03d", adm2code))) %>% 
#   mutate(admin_name = unaccent(adm2_2019)) %>% 
#   filter(adm2code != 0, adm2code != 999) %>% 
#   mutate(dup_code = duplicated(geocode), dup_name = duplicated(admin_name)) %>% 
#   filter(dup_code == FALSE | dup_name == FALSE) %>%  # No duplicated codes with different names but plenty of duplicated names with different codes remain
#   filter(dup_name == FALSE) %>% # For the sake of the experiment, get rid of any duplicated names
#   full_join(file1, by = "admin_name")
# 
# 
# 
# 
# 
# 
# state_code <- 1
# state_name = unaccent(as.character(file2[adm1code == state_code & adm2code == 0, 25]))
# 
# 
# 
# 
# # Using adm1code = 1 as toy data
# file1_1 <- file1 %>% 
#   mutate(state = as.numeric(amd1_code)) %>% 
#   filter(state == state_code) %>% 
#   mutate(city1 = as.numeric(substr(geolevel2, 7, 10))) %>% 
#   mutate(admin_name = ifelse(city1 == 999, admin_name, strsplit(admin_name, ", "))) %>% 
#   unnest(admin_name) %>% 
#   filter(!admin_name %in% c("El", "La", "Los", "Las"))
# 
# file2_1 <- file2 %>% 
#   mutate(state = adm1code) %>% 
#   filter(state == state_code) %>% 
#   mutate(city2 = adm2code) %>% 
#   mutate(admin_name = unaccent(adm2_2019)) %>% 
#   mutate(admin_name = ifelse(city2 == 0, paste0("State of ", admin_name), admin_name)) %>% 
#   mutate(admin_name = ifelse(city2 == 999, paste0(state_name, ", municipality unknown"), admin_name)) %>% 
#   mutate(admin_name = str_remove_all(admin_name, "El ")) %>% 
#   mutate(admin_name = str_remove_all(admin_name, "La ")) %>% 
#   mutate(admin_name = str_remove_all(admin_name, "Los ")) %>% 
#   mutate(admin_name = str_remove_all(admin_name, "Las ")) 
# 
# state_1 <- full_join(file1_1, file2_1, by = c("state", "admin_name")) %>% 
#   select(-one_of(c("state", "city1", "city2")))
# 
# 
# 
# 
# 
# # Save output to show to Tamma 
# file1_1 <- file1 %>% 
#   filter(amd1_code == "001")
# 
# file2_1 <- file2 %>% 
#   filter(adm1code == state_code)
# 
# 
# 
# write.csv(file1_1, paste0(here("data/merge_mex/output/"), "/", state_name, "_file1", ".csv"))
# write.csv(file2_1, paste0(here("data/merge_mex/output/"), "/", state_name, "_file2", ".csv"))
# write.csv(state_1, paste0(here("data/merge_mex/output/"), "/", state_name, "_merged", ".csv"))
# 





# Function to merge state by state according to unaccented names
state_merge <- function(state_code){
  state_name = unaccent(as.character(file2[adm1code == state_code & adm2code == 0, 25]))
  
  file1_tmp <- file1 %>% 
    mutate(state = as.numeric(amd1_code)) %>% 
    filter(state == state_code) %>% 
    mutate(city1 = as.numeric(substr(geolevel2, 7, 9))) %>% 
    mutate(merge_name = ifelse(city1 == 999, admin_name, strsplit(admin_name, ", "))) %>% 
    unnest(merge_name) %>% 
    filter(!merge_name %in% c("El", "La", "Los", "Las")) %>% 
    mutate(merge_name = unaccent(str_to_title(merge_name))) %>% 
    mutate(merge_name = str_remove_all(merge_name, "El ")) %>% 
    mutate(merge_name = str_remove_all(merge_name, "La ")) %>% 
    mutate(merge_name = str_remove_all(merge_name, "Los ")) %>% 
    mutate(merge_name = str_remove_all(merge_name, "Las ")) %>%  
    mutate(merge_name = str_remove_all(merge_name, "De ")) %>% 
    mutate(merge_name = str_remove_all(merge_name, "Del "))
  
  file2_tmp <- file2 %>% 
    mutate(state = adm1code) %>% 
    filter(state == state_code) %>% 
    mutate(city2 = adm2code) %>% 
    mutate(merge_name = unaccent(adm2_2019)) %>% 
    mutate(merge_name = ifelse(city2 == 0, paste0("State of ", merge_name), merge_name)) %>% 
    mutate(merge_name = ifelse(city2 == 999, paste0(state_name, ", municipality unknown"), merge_name)) %>% 
    mutate(merge_name = str_to_title(merge_name)) %>% 
    mutate(merge_name = str_remove_all(merge_name, "El ")) %>% 
    mutate(merge_name = str_remove_all(merge_name, "La ")) %>% 
    mutate(merge_name = str_remove_all(merge_name, "Los ")) %>% 
    mutate(merge_name = str_remove_all(merge_name, "Las ")) %>%  
    mutate(merge_name = str_remove_all(merge_name, "De ")) %>% 
    mutate(merge_name = str_remove_all(merge_name, "Del "))
  
  state <- full_join(file1_tmp, file2_tmp, by = c("state", "merge_name")) %>% 
    select(-one_of(c("state", "city1", "city2"))) %>% 
    mutate(adm1name = state_name)
  
  return(state)
}



# Write a function to fix mismatches like Cuatro Cienegas vs Cuatrocienegas in state_05
fix_mismatch <- function(state_df, merge_name_1, merge_name_2, name_to_keep = 2, remove_old = "both"){
  
  # Decide on new name
  if(name_to_keep == 1){
    name_to_keep = merge_name_1
  }
  if(name_to_keep == 2){
    name_to_keep = merge_name_2
  }
  
  # Get the row with file1 values
  row_1 <- state_df %>% 
    filter(merge_name == merge_name_1) %>% 
    mutate(merge_name = name_to_keep) %>% 
    select(1:6)
  
  row_2 <- state_df %>% 
    filter(merge_name == merge_name_2) %>% 
    mutate(merge_name = name_to_keep) %>% 
    select(6:33)
  
  row_combined <- full_join(row_1, row_2, by = c("merge_name"))
  
  
  if(remove_old == "both"){
    new_state <- state_df %>% 
      filter(!merge_name %in% c(merge_name_1, merge_name_2)) %>% 
      rbind(row_combined) 
  }
  else if(remove_old == 1){
    new_state <- state_df %>% 
      filter(!merge_name %in% c(merge_name_1)) %>% 
      rbind(row_combined) 
  }
  else if(remove_old == 2){
    new_state <- state_df %>% 
      filter(!merge_name %in% c(merge_name_2)) %>% 
      rbind(row_combined)
  }
  else{
    new_state <- state_df %>%
      rbind(row_combined)
  }
  
  
  return(new_state)
}





state_01 <- state_merge(1) # Good

state_02 <- state_merge(2) # Good but missing unknown

state_03 <- state_merge(3) # Good but missing unknown

state_04 <- state_merge(4) # Good

state_05 <- state_merge(5) %>%  # Good now
  fix_mismatch("Cuatrocienegas", 
               "Cuatro Cienegas", 2) %>% 
  fix_mismatch("Coahuila, Municipality Unknown", 
               "Coahuila Zaragoza, Municipality Unknown",
               2)

state_06 <- state_merge(6) # Good

state_07 <- state_merge(7) %>% # Several recently recognized areas in file 2 that do not have file 1 pairs
  fix_mismatch("Monte Cristo Guerrero", "Montecristo Guerrero") 

state_08 <- state_merge(8) %>% # Good now
  fix_mismatch("Temosachi", "Temosachic", 2) %>% 
  fix_mismatch("Batopilas", "Batopilas Manuel Gomez Morin", 1)

state_09 <- state_merge(9) %>% # Assuming ciudad de mexico is same as distrito federal
  fix_mismatch("Distrito Federal, Municipality Unknown", "Ciudad Mexico, Municipality Unknown", 2) # Good now
  

state_10 <- state_merge(10) # Good

state_11 <- state_merge(11) %>%  # Warning: Allende (File 1) is a place in Cuahuila. Given that it is in the state Guanajuato, 
                                 # I'm assuming that it means San Miguel de Allende, Guanajuato.
  fix_mismatch("Allende", "San Miguel Allende", 2) %>% 
  fix_mismatch("Dolores Hidalgo", "Dolores Hidalgo Cuna Independencia Nacional", 1) # Good now

state_12 <- state_merge(12) %>% # Warning: File 1 contains a municipality named Jose Azueta. 
                                # I believe this to be referring to Jose de Azueta which google 
                                # says is the same as Zihuatanejo de Azueta, and not Jose de Azueta 
                                #in Veracruz (this is Guerrero)
  fix_mismatch("Jose Azueta", "Zihuatanejo Azueta")

state_13 <- state_merge(13)

state_14 <- state_merge(14) # All good

state_15 <- state_merge(15) %>% # Good 
  fix_mismatch("Acambay Ru?Z Casta?Eda", "Acambay Ruiz Castaneda", 2)

state_16 <- state_merge(16) %>% # Good
  fix_mismatch("Michaocan, Municipality Unknown", "Michoacan Ocampo, Municipality Unknown", 2)

state_17 <- state_merge(17) %>%  # 2 missing
  fix_mismatch("Jonacatepec", "Jonacatepec Leandro Valle", 1) %>% 
  fix_mismatch("Tlaltizap?N Zapata", "Tlaltizapan Zapata", 2) %>% 
  fix_mismatch("Zacatepec Hidalgo", "Zacatepec", 1) %>% 
  fix_mismatch("Zacualpan", "Zacualpan Amilpas", 1)

state_18 <- state_merge(18) # Good but missing unknown

state_19 <- state_merge(19) # Good

state_20 <- state_merge(20) %>%  # Lots missing from file1
  unique() %>% # Several duplicated rows
  fix_mismatch("Matias Romero", "Matias Romero Avendano", 2) %>%
  mutate(merge_name = ifelse(adm2code != 100 | is.na(adm2code), merge_name, "San Andres")) %>%  # no 2019 name for san andres yaa
  fix_mismatch("San Andres Yaa", "San Andres", 1) %>% 
  mutate(merge_name = ifelse(adm2code != 208 | is.na(adm2code), merge_name, "San Juan Mixtepec_08")) %>% 
  mutate(merge_name = ifelse(adm2code != 209 | is.na(adm2code), merge_name, "San Juan Mixtepec_26")) %>% 
  fix_mismatch("San Juan Mixtepec - Distr. 08", "San Juan Mixtepec_08", 1) %>% 
  fix_mismatch("San Juan Mixtepec - Distr. 26", "San Juan Mixtepec_26", 1) %>% 
  mutate(merge_name = ifelse(adm2code != 318 | is.na(adm2code), merge_name, "San Pedro Mixtepec_22")) %>% 
  mutate(merge_name = ifelse(adm2code != 319 | is.na(adm2code), merge_name, "San Pedro Mixtepec_26")) %>% 
  fix_mismatch("San Pedro Mixtepec - Distr. 22", "San Pedro Mixtepec_22", 1) %>% 
  fix_mismatch("San Pedro Mixtepec - Distr. 26", "San Pedro Mixtepec_26", 1) %>% 
  fix_mismatch("San Pedro Totolapa", "San Pedro Totolapam", 1) %>% 
  fix_mismatch("Villa Tututepec Melchor Ocampo", "Villa Tututepec", 1) %>% 
  fix_mismatch("Santiago Chazumba", "Villa Santiago Chazumba", 1) %>% 
  fix_mismatch("Heroica Villa Tezoatl?N Segura Y Luna", "Heroica Villa Tezoatlan Segura Y Luna, Cuna Independencia Oaxaca", 2) %>% 
  filter(merge_name != "Cuna Independencia Oaxaca") 
  
  

state_21 <- state_merge(21) # Good

state_22 <- state_merge(22) # All good

state_23 <- state_merge(23) # 1 missing from file1

state_24 <- state_merge(24) %>%  # 1 issue Tancanhuitz vs Tancanhuitz Santos
  fix_mismatch("Tancanhuitz Santos", "Tancanhuitz", 2)

state_25 <- state_merge(25) # All good

state_26 <- state_merge(26) # All good

state_27 <- state_merge(27) # All good

state_28 <- state_merge(28) %>% # Good now
  fix_mismatch("Tamoulipas, Municipality Unknown", "Tamaulipas, Municipality Unknown")

state_29 <- state_merge(29) %>% # Good but missing unknown
  fix_mismatch("Altzayanca", "Atltzayanca", 1) %>% 
  fix_mismatch("Zitlaltepec Trinidad Sanchez Santos", "Ziltlaltepec Trinidad Sanchez Santos", 1) %>% 
  fix_mismatch("Yauhquemecan", "Yauhquemehcan", 1)

state_30 <- state_merge(30) %>% # Good now
  fix_mismatch("Cazones", "Cazones Herrera", 1) %>% 
  fix_mismatch("Huiloapan", "Huiloapan Cuauhtemoc", 2) %>% 
  fix_mismatch("Medell?N", "Medellin Bravo", 2) %>% 
  fix_mismatch("Temapache", "Alamo Temapache", 2) %>% 
  fix_mismatch("Tuxpam", "Tuxpan", 2) %>% 
  fix_mismatch("Veracruz, Municipality Unknown", "Veracruz Ignacio Llave, Municipality Unknown", 2)
  

state_31 <- state_merge(31) # Good but missing unkown

state_32 <- state_merge(32) # All good


# States with missing municipalities
# Missing from File 1: 7, 17, 20, 23
# Missing from File 2: 

state_na <- file2 %>% 
  filter(!adm1code %in% 1:32)

# for(i in 1:32){a <- paste0(a, "state_", sprintf("%02d", i), ", ")}

all_states <- rbind(state_01, state_02, state_03, state_04, state_05, 
                    state_06, state_07, state_08, state_09, state_10, 
                    state_11, state_12, state_13, state_14, state_15, 
                    state_16, state_17, state_18, state_19, state_20, 
                    state_21, state_22, state_23, state_24, state_25, 
                    state_26, state_27, state_28, state_29, state_30, 
                    state_31, state_32) %>% 
  bind_rows(state_na)




# Check Results
all(file1$admin_name %in% all_states$admin_name) # True
all(file2$adm2_2000 %in% all_states$adm2_2000) # This holds true for 2000:2019

setdiff(all_states$admin_name, file1$admin_name) # Only new value is NA
setequal(all_states$adm2_2000, file2$adm2_2000) # True


is_missing <- all_states %>% 
  filter(!is.na(admin_name) & is.na(adm1code)) # There are no rows in file 1 that lack a file 2 counterpart


# These checks show that every row in file 1 has been assigned to a row in file 2, and that the unassigned rows 
# from file 2 are still included in the end data set, just don't have a file 1 counterpart





# Save
write.csv(all_states, paste0(here("data", "merge_mex", "output"), "/mexico_geocodes_merged.csv"))


