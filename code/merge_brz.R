# Merge Brazil Codes
# Tyler Liddell
# April 28, 2023

# Load Libraries
library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(scraEP)
library(data.table)

# Had to redo script because of split states, kept old code for record, scroll down for new code

# # Read in raw data
# file1 <- read_excel(here("data/merge_brz/IPUMS_geo2_br1980_2010.xlsx")) %>% 
#   clean_names() %>% 
#   as.data.table()
# file2 <- read_excel(here("data/merge_brz/IBGE_br_municipios_2021.xlsx")) %>% 
#   clean_names() %>% 
#   as.data.table()
# 
# 
# # Rework state_merge function from merge_mex.R to fit Brazil
# state_merge <- function(state_code){
#   
#   file1_tmp <- file1 %>% 
#     mutate(state = as.numeric(adm1_code)) %>% 
#     filter(state == state_code) %>% 
#     mutate(merge_name = strsplit(adm2_name, ", ")) %>% 
#     unnest(merge_name) %>% 
#     mutate(merge_name = unaccent(str_to_title(merge_name))) %>% 
#     mutate(merge_name = str_remove_all(merge_name, "Do ")) %>% 
#     mutate(merge_name = str_remove_all(merge_name, "Da ")) %>% 
#     mutate(merge_name = str_remove_all(merge_name, "De "))
#   
#   file2_tmp <- file2 %>% 
#     mutate(state = adm1_code) %>% 
#     filter(state == state_code) %>% 
#     mutate(merge_name = unaccent(str_to_title(nm_mun))) %>% 
#     mutate(merge_name = str_remove_all(merge_name, "El ")) %>% 
#     mutate(merge_name = str_remove_all(merge_name, "Do ")) %>% 
#     mutate(merge_name = str_remove_all(merge_name, "Da ")) %>% 
#     mutate(merge_name = str_remove_all(merge_name, "De "))
#   
#   state <- full_join(file1_tmp, file2_tmp, by = c("state", "adm1_code", "merge_name")) %>% 
#     select(-"state")
#   
#   return(state)
# }
# 
# 
# # Rework the function fix_mismatches from merge_mex.R for Brazil
# fix_mismatch <- function(state_df, merge_name_1, merge_name_2, name_to_keep = 1, remove_old = "both"){
#   
#   # Decide on new name
#   if(name_to_keep == 1){
#     name_to_keep = merge_name_1
#   }
#   if(name_to_keep == 2){
#     name_to_keep = merge_name_2
#   }
#   
#   # Get the row with file1 values
#   row_1 <- state_df %>% 
#     filter(merge_name == merge_name_1) %>% 
#     mutate(merge_name = name_to_keep) %>% 
#     select(1:4)
#   
#   row_2 <- state_df %>% 
#     filter(merge_name == merge_name_2) %>% 
#     mutate(merge_name = name_to_keep) %>% 
#     select(4:10)
#   
#   row_combined <- full_join(row_1, row_2, by = c("merge_name"))
#   
#   
#   if(remove_old == "both"){
#     new_state <- state_df %>% 
#       filter(!merge_name %in% c(merge_name_1, merge_name_2)) %>% 
#       rbind(row_combined) 
#   }
#   else if(remove_old == 1){
#     new_state <- state_df %>% 
#       filter(!merge_name %in% c(merge_name_1)) %>% 
#       rbind(row_combined) 
#   }
#   else if(remove_old == 2){
#     new_state <- state_df %>% 
#       filter(!merge_name %in% c(merge_name_2)) %>% 
#       rbind(row_combined)
#   }
#   else{
#     new_state <- state_df %>%
#       rbind(row_combined)
#   }
#   
#   
#   return(new_state)
# }
# 
# # Check which state numbers exist
# codes_1 <- sort(unique(file1$adm1_code))
# codes_2 <- sort(unique(file2$adm1_code))
# setdiff(codes_2, codes_1) # 17 and 50
# setdiff(codes_1, codes_2) # None
# # 17 and 50 are not in file 1
# 
# # Start merging state by state
# state_11 <- state_merge(11) %>% # Lots missing from this one
#   fix_mismatch("Alto Aleg", "Alto Alegre Dos Parecis", 2) # There may be an issue with some file 1 names being abbreviated or cut off
# 
# # From state_11 and state_21 I've deduced that columns are limited to 250 bytes and so several get cut off
# 
# state_12 <- state_merge(12) # Good
# 
# state_13 <- state_merge(13) # Good
# 
# state_14 <- state_merge(14) # Good
# 
# state_15 <- state_merge(15) %>% # 1 missing
#   fix_mismatch("Santa Isabel Para", "Santa Izabel Para", 1) %>% 
#   fix_mismatch("Eldorado Dos Carajas", "Eldorado Carajas", 2)
# 
# state_16 <- state_merge(16) # Good
# 
# state_21 <- state_merge(21) %>% # Tons missing and Ca cutoff in file 1
#   fix_mismatch("Brejo De", "Brejo Areia", 2) # Brejo de was the cutoff item, I'm not sure this is accurate but this is the only brejo de... in file 2
#   
# state_22 <- state_merge(22) # Tons missing and Is cutoff in file 1
# 
# state_23 <- state_merge(23) %>% 
#   fix_mismatch("Itapage", "Itapaje", 2)
# 
# state_24 <- state_merge(24) %>% # Good now 
#   fix_mismatch("Olho-D agua Borges", "Olho D agua Borges", "Olho D'Agua Borges") %>% 
#   fix_mismatch("Augusto Severo", "Campo Grande", 1) %>% # Campo Grande used to be Augusto Severo
#   fix_mismatch("Presidente Juscelino", "Serra Caiada", 1) # Serra Caiada was formerly Presidente Juscelino
# 
# state_25 <- state_merge(25) %>% # Good now
#   fix_mismatch("Serido", "Sao Vicente Serido", 2) %>% 
#   fix_mismatch("Santarem", "Joca Claudino", 1) #Joca Claudino was once Santarem
# 
# state_26 <- state_merge(26) %>% # Good now
#   fix_mismatch("Iguaraci", "Iguaracy", 1)
# 
# state_27 <- state_merge(27) # Good
# 
# state_28 <- state_merge(28) # Good
# 
# state_29 <- state_merge(29) %>%  # Good
#   fix_mismatch("Santa Teresinha", "Santa Terezinha", 2)
# 
# state_31 <- state_merge(31) %>% # Good now
#   fix_mismatch("Brasopolis", "Brazopolis", 1) %>% 
#   fix_mismatch("Dona Eusebia", "Dona Euzebia", 1) %>% 
#   fix_mismatch("Passa-Vinte", "Passa Vinte", 2) %>% 
#   fix_mismatch("Sao Thome Das Letras", "Sao Tome Das Letras", 1)
#   
# state_32 <- state_merge(32) # Good
# 
# state_33 <- state_merge(33) # Good 
# 
# state_35 <- state_merge(35) %>% # Good now
#   fix_mismatch("Embu", "Embu Das Artes", 2) %>% 
#   fix_mismatch("Moji Mirim", "Mogi Mirim", 2) %>% 
#   fix_mismatch("Biritiba-Mirim", "Biritiba Mirim", 2) %>% 
#   fix_mismatch("Florinia", "Florinea", 1) %>% 
#   fix_mismatch("Sao Luis Paraitinga", "Sao Luiz Paraitinga", 2) 
# 
# state_41 <- state_merge(41) # Good
# 
# state_42 <- state_merge(42) %>% # Two missing
#   fix_mismatch("Grao Para", "Grao-Para", 1)
# 
# state_43 <- state_merge(43) %>% # Several missing
#   fix_mismatch("Passo Fundo,", "Passo Fundo", 2)
# 
# state_51 <- state_merge(51) %>% # Headacheeeeee
#   fix_mismatch("Nova Canaa Nort", "Nova Canaa Norte", 2) %>% 
#   fix_mismatch("Poxoreo", "Poxoreu")
# 
# state_52 <- state_merge(52) # Tons missing from file 2
# 
# state_53 <- state_merge(53) # Just Brasilia
# 
# 
# 
# # Bind all together
# all_states_string <- ""
# for(i in codes_1){
#   all_states_string <- paste0(all_states_string, ", state_", sprintf("%02d", i))
# }
# 
# all_states <- rbind(state_11, state_12, state_13, state_14, state_15, state_16, 
#                     state_21, state_22, state_23, state_24, state_25, state_26, 
#                     state_27, state_28, state_29, state_31, state_32, state_33, 
#                     state_35, state_41, state_42, state_43, state_51, state_52, 
#                     state_53) 
# 
# 
# # File1 municipalities not paired with file2 municipalities
# file1_unmatched <- all_states %>% 
#   filter(is.na(nm_mun))
# 
# # states with no file1 counterpart
# state_17 <- file2 %>% 
#   filter(adm1_code == 17) %>% 
#   mutate(merge_name = unaccent(str_to_title(nm_mun))) %>% 
#   mutate(merge_name = str_remove_all(merge_name, "El ")) %>% 
#   mutate(merge_name = str_remove_all(merge_name, "Do ")) %>% 
#   mutate(merge_name = str_remove_all(merge_name, "Da ")) %>% 
#   mutate(merge_name = str_remove_all(merge_name, "De "))
#   
# 
# state_50 <- file2 %>% 
#   filter(adm1_code == 50) %>% 
#   mutate(merge_name = unaccent(str_to_title(nm_mun))) %>% 
#   mutate(merge_name = str_remove_all(merge_name, "El ")) %>% 
#   mutate(merge_name = str_remove_all(merge_name, "Do ")) %>% 
#   mutate(merge_name = str_remove_all(merge_name, "Da ")) %>% 
#   mutate(merge_name = str_remove_all(merge_name, "De "))
# 
# # File2 municipalities not paired with file1 municipalities
# file2_unmatched <- all_states %>% 
#   filter(is.na(adm2_name)) %>% 
#   bind_rows(state_17) %>% 
#   bind_rows(state_50)
# 
# 
# all_unmatched <- rbind(file1_unmatched, file2_unmatched) # Several from 17 align with file1's 50
# 
# 
# # Create false adm1_code to merge un by using the state_merge function
# file1_99 <- file1_unmatched %>% 
#   mutate(adm1_code_file1 = adm1_code) %>% 
#   mutate(adm1_code = 99) %>% 
#   select(1:4, 11)
# 
# file2_99 <- file2_unmatched %>% 
#   mutate(adm1_code_file2 = adm1_code) %>% 
#   mutate(adm1_code = 99) %>% 
#   select(3:11)
# 
# # Tweak fix_mismatch to work with the file2_99
# fix_mismatch_99 <- function(state_df, merge_name_1, merge_name_2, name_to_keep = 1, remove_old = "both"){
#   
#   # Decide on new name
#   if(name_to_keep == 1){
#     name_to_keep = merge_name_1
#   }
#   if(name_to_keep == 2){
#     name_to_keep = merge_name_2
#   }
#   
#   # Get the row with file1 values
#   row_1 <- state_df %>% 
#     filter(merge_name == merge_name_1) %>% 
#     mutate(merge_name = name_to_keep) %>% 
#     select(1:5)
#   
#   row_2 <- state_df %>% 
#     filter(merge_name == merge_name_2) %>% 
#     mutate(merge_name = name_to_keep) %>% 
#     select(4, 6:12)
#   
#   row_combined <- full_join(row_1, row_2, by = c("merge_name"))
#   
#   
#   if(remove_old == "both"){
#     new_state <- state_df %>% 
#       filter(!merge_name %in% c(merge_name_1, merge_name_2)) %>% 
#       rbind(row_combined) 
#   }
#   else if(remove_old == 1){
#     new_state <- state_df %>% 
#       filter(!merge_name %in% c(merge_name_1)) %>% 
#       rbind(row_combined) 
#   }
#   else if(remove_old == 2){
#     new_state <- state_df %>% 
#       filter(!merge_name %in% c(merge_name_2)) %>% 
#       rbind(row_combined)
#   }
#   else{
#     new_state <- state_df %>%
#       rbind(row_combined)
#   }
#   
#   
#   return(new_state)
# }
# 
# merge_99 <- full_join(file1_99, file2_99, by = c("adm1_code", "merge_name")) %>% # All non-cutoff file1 municipalities matched
#   fix_mismatch_99("Fortaleza Tabocao", "Tabocao", 2) %>% 
#   fix_mismatch_99("Tocantin", "Tocantinopolis", 2) #%>% # Got cut off, only am ok doing this because Tocantina was already matched
#   
# # No mismatched states unless file2 state = 17 and 50, and when file2 = 17, file1 = 52, when file2 = 50, file1 = 51
# 
# # File2 17, TO (Tocantins) is a state that split from Goias in 1989, as a result we can safely merge 17 and 52
# 
# # File2 50, MS (Mato Grosso do Sul) is a state that split from Mato Grosso in 1979, as a result we can safely merge 50 and 51



# Redo the entire script to create columns file1_adm1_code and file2_adm1_code 
# to reflect the states that split above. Update functions accordingly


# Reread data with modifications
file1 <- read_excel(here("data/merge_brz/IPUMS_geo2_br1980_2010.xlsx")) %>% 
  clean_names() %>% 
  rename(file1_adm1_code = adm1_code) %>%  
  as.data.table() # Always merging by file1's adm1_code

file2 <- read_excel(here("data/merge_brz/IBGE_br_municipios_2021.xlsx")) %>% 
  clean_names() %>% 
  rename(file2_adm1_code = adm1_code) %>% 
  mutate(file1_adm1_code = case_when(file2_adm1_code == 17 ~ 52,
                                     file2_adm1_code == 50 ~ 51,
                                     .default = file2_adm1_code)) %>% 
  as.data.table()




  
# Redefine functions
state_merge <- function(state_code){
  
  file1_tmp <- file1 %>% 
    mutate(state = as.numeric(file1_adm1_code)) %>% 
    filter(state == state_code) %>% 
    mutate(merge_name = strsplit(adm2_name, ", ")) %>% 
    unnest(merge_name) %>% 
    mutate(merge_name = unaccent(str_to_title(merge_name))) %>% 
    mutate(merge_name = str_remove_all(merge_name, "Do ")) %>% 
    mutate(merge_name = str_remove_all(merge_name, "Da ")) %>% 
    mutate(merge_name = str_remove_all(merge_name, "De "))
  
  file2_tmp <- file2 %>% 
    mutate(state = file1_adm1_code) %>% 
    filter(state == state_code) %>% 
    mutate(merge_name = unaccent(str_to_title(nm_mun))) %>% 
    mutate(merge_name = str_remove_all(merge_name, "El ")) %>% 
    mutate(merge_name = str_remove_all(merge_name, "Do ")) %>% 
    mutate(merge_name = str_remove_all(merge_name, "Da ")) %>% 
    mutate(merge_name = str_remove_all(merge_name, "De "))
  
  state <- full_join(file1_tmp, file2_tmp, by = c("state", "file1_adm1_code", "merge_name")) %>% 
    select(-"state")
  
  return(state)
}

fix_mismatch <- function(state_df, merge_name_1, merge_name_2, name_to_keep = 1){
  
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
    select(1:4)
  
  row_2 <- state_df %>% 
    filter(merge_name == merge_name_2) %>% 
    mutate(merge_name = name_to_keep) %>% 
    select(4:11)
  
  row_combined <- full_join(row_1, row_2, by = c("merge_name"))
  

  new_state <- state_df %>% 
    filter(!merge_name %in% c(merge_name_1, merge_name_2)) %>% 
    rbind(row_combined) 

  
  return(new_state)
}

# Get updated list of file1_adm1_code we need to run through
codes <- sort(unique(file1$file1_adm1_code))

# Merge state by state
state_11 <- state_merge(11) %>% # Lots missing from this one
  fix_mismatch("Alto Aleg", "Alto Alegre Dos Parecis", 2) # There may be an issue with some file 1 names being abbreviated or cut off

# From state_11 and state_21 I've deduced that columns are limited to 250 bytes and so several get cut off

state_12 <- state_merge(12) # Good

state_13 <- state_merge(13) # Good

state_14 <- state_merge(14) # Good

state_15 <- state_merge(15) %>% # 1 missing
  fix_mismatch("Santa Isabel Para", "Santa Izabel Para", 1) %>% 
  fix_mismatch("Eldorado Dos Carajas", "Eldorado Carajas", 2)

state_16 <- state_merge(16) # Good

state_21 <- state_merge(21) %>% # Tons missing and Ca cutoff in file 1
  fix_mismatch("Brejo De", "Brejo Areia", 2) # Brejo de was the cutoff item, I'm not sure this is accurate but this is the only brejo de... in file 2

state_22 <- state_merge(22) %>%  # Tons missing
  fix_mismatch("Is", "Isaias Coelho", 2) # Only Is... in table

state_23 <- state_merge(23) %>% 
  fix_mismatch("Itapage", "Itapaje", 2)

state_24 <- state_merge(24) %>% # Good now 
  fix_mismatch("Olho-D agua Borges", "Olho D agua Borges", "Olho D'Agua Borges") %>% 
  fix_mismatch("Augusto Severo", "Campo Grande", 1) %>% # Campo Grande used to be Augusto Severo
  fix_mismatch("Presidente Juscelino", "Serra Caiada", 1) # Serra Caiada was formerly Presidente Juscelino

state_25 <- state_merge(25) %>% # Good now
  fix_mismatch("Serido", "Sao Vicente Serido", 2) %>% 
  fix_mismatch("Santarem", "Joca Claudino", 1) #Joca Claudino was once Santarem

state_26 <- state_merge(26) %>% # Good now
  fix_mismatch("Iguaraci", "Iguaracy", 1)

state_27 <- state_merge(27) # Good

state_28 <- state_merge(28) # Good

state_29 <- state_merge(29) %>%  # Good
  fix_mismatch("Santa Teresinha", "Santa Terezinha", 2)

state_31 <- state_merge(31) %>% # Good now
  fix_mismatch("Brasopolis", "Brazopolis", 1) %>% 
  fix_mismatch("Dona Eusebia", "Dona Euzebia", 1) %>% 
  fix_mismatch("Passa-Vinte", "Passa Vinte", 2) %>% 
  fix_mismatch("Sao Thome Das Letras", "Sao Tome Das Letras", 1)

state_32 <- state_merge(32) # Good

state_33 <- state_merge(33) # Good 

state_35 <- state_merge(35) %>% # Good now
  fix_mismatch("Embu", "Embu Das Artes", 2) %>% 
  fix_mismatch("Moji Mirim", "Mogi Mirim", 2) %>% 
  fix_mismatch("Biritiba-Mirim", "Biritiba Mirim", 2) %>% 
  fix_mismatch("Florinia", "Florinea", 1) %>% 
  fix_mismatch("Sao Luis Paraitinga", "Sao Luiz Paraitinga", 2) 

state_41 <- state_merge(41) # Good

state_42 <- state_merge(42) %>% # Two missing
  fix_mismatch("Grao Para", "Grao-Para", 1)

state_43 <- state_merge(43) %>% # Several missing
  fix_mismatch("Passo Fundo,", "Passo Fundo", 2)

state_51 <- state_merge(51) %>%
  fix_mismatch("Nova Canaa Nort", "Nova Canaa Norte", 2) %>% 
  fix_mismatch("Poxoreo", "Poxoreu") %>% 
  fix_mismatch("Arag", "Araguaiana", 2) # There are other Arag... but this is the only unmatched one

state_52 <- state_merge(52) %>% 
  fix_mismatch("Fortaleza Tabocao", "Tabocao", 2) %>% 
  fix_mismatch("Tocantin", "Tocantinopolis", 2) # Aguiarnopolis is unmatched from file1

state_53 <- state_merge(53) # Just Brasilia



# Bind all together
all_states_string <- ""
for(i in codes){
  all_states_string <- paste0(all_states_string, ", state_", sprintf("%02d", i))
}

all_states <- rbind(state_11, state_12, state_13, state_14, state_15, state_16, 
                    state_21, state_22, state_23, state_24, state_25, state_26, 
                    state_27, state_28, state_29, state_31, state_32, state_33, 
                    state_35, state_41, state_42, state_43, state_51, state_52, 
                    state_53) 

# Isolate the unmatched to see if any matches have been overlooked
unmatched <- all_states %>% 
  filter(is.na(adm2_code) | is.na(file2_adm1_code))

# The only unmatched one from file1 is Ca, 102 unmatched from file2


# Save output
write.csv(all_states, paste0(here("data", "merge_brz", "output"), "/brazil_geocodes_merged.csv"))


