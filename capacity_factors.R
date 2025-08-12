# Libraries --------------------------------------------------------------------

library(countrycode) # For compability between naming standards
library(tidyverse) # Used for data manipulation

# Lists ------------------------------------------------------------------------

# Lists of the 28 countries that are included.

countries <- list("Austria", "Belgium", "Bulgaria", "Croatia", "Czechia", 
                  "Denmark", "Estonia", "Finland", "France", "Germany", 
                  "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", 
                  "Luxembourg", "Netherlands", "Norway", "Poland", "Portugal", 
                  "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", 
                  "Switzerland", "United Kingdom")

country_codes <- list('AT', 'BE', 'BG', 'HR', 'CZ', 'DK', 'EE', 'FI', 'FR', 'DE',
                      'EL', 'HU', 'IE', 'IT', 'LV', 'LT', 'LU', 'NL', 'NO', 'PL',
                      'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'CH', 'UK')

countries_geo <- list("Austria", "Belgium", "Bulgaria", "Czechia", "Germany", 
                      "Denmark", "Estonia", "Greece", "Spain", "Finland", "France", 
                      "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", 
                      "Luxembourg", "Latvia", "Netherlands", "Norway", "Poland", 
                      "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia", 
                      "Switzerland", "United Kingdom", "Kosovo", "Republic of Serbia",
                      'North Macedonia', 'Albania', 'Bosnia and Herzegovina', 
                      'Moldova', 'Montenegro')

# Data from renewables ninja ---------------------------------------------------

# file_names <- list.files('data/ninja') # Extracts filenames
# 
# ninja_list = list() # Makes a list called ninja to be used inside the loop.
# 
# for (i in 1:56) { # for the numbers 1 to 56 (I have 56 files) do the following :
#   tmp <- read.csv(paste0('data/ninja/', file_names[i]), skip = 2) %>%  # Read the csv files and put them in a temporary file
#     mutate(onshore = ifelse('onshore' %in% names(.), onshore, NA), # Check to see if the datasets include variables called onshore and offshore, if not make them and populate them with NA values. This is because some countries do not have offshore wind and the datasets are not compatible without doing this.
#            offshore = ifelse('offshore' %in% names(.), offshore, NA), # See above.
#            geo = str_extract(file_names[i], '[A-Z]+'), # Make a geo variable to identify country
#            type = str_extract(file_names[i], '(wind)|(pv)')) # Make a type variable to identify if PV or wind.
#   smr <- tmp %>% group_by(geo, type) %>%  # This is used to keep the geo and type variables when summarising.
#     summarise(national = mean(national, na.rm = T),
#               onshore = mean(onshore, na.rm = T),
#               offshore = mean(offshore, na.rm = T)) # Calculate mean for the variables.
#     
#   ninja_list[[i]] <- smr # Make a list
# }
# 
# ninja <- do.call(bind_rows, ninja_list) %>% # Binds the data from ninja_list to one dataframe.
#   pivot_longer(cols = c('national', 'onshore', 'offshore'), 
#                names_to = 'type_2', values_to = 'cf') %>% 
#   mutate(siec = case_when(type == 'pv' & type_2 == 'national' ~ 'RA420',
#                           type == 'wind' & type_2 == 'onshore' ~ 'RA310',
#                           type == 'wind' & type_2 == 'offshore' ~ 'RA320',
#                           .default = NA))



# Renewables Ninja 2016 --------------------------------------------------------

# This is only for up to 2016 data

wind <- read.csv('data/ninja_wind/ninja_wind_europe_v1.1_current_on-offshore.csv')

pv <- read.csv('data/ninja_pv/ninja_pv_europe_v1.1_merra2.csv')

# Calculating mean production for onshore and offshore wind

mean_wind <- lapply(wind, mean) %>% # Calculates the mean for each country and technology
  data.frame() %>% # Makes it a dataframe
  select(!time) %>%  # Removes the time column
  t() %>% # Transposes the table
  data.frame() %>% # Makes the result of the transposing a data frame
  rownames_to_column() %>% # Makes the id column a normal column
  rename(mean_wind = '.') %>% # Renames the column labelled . to mean
  mutate(geo = str_extract(rowname, '^.{2}'), # Makes a variable for country id
         type = ifelse(str_detect(rowname, '_ON'), 'onshore', 'offshore')) %>%  # Makes a variable for onshore/offshore technology
  select(!rowname) %>% # Removes the rowname column
  pivot_wider(names_from = type, values_from = mean_wind) # Makes a wide format table

# Calculating mean capacity factor for PVs

mean_pv <- lapply(pv, mean) %>% # Calculates the mean for each country and technology
  data.frame() %>% # Makes it a dataframe
  select(!time) %>%  # Removes the time column
  t() %>% # Transposes the table
  data.frame() %>% # Makes the result of the transposing a data frame
  rownames_to_column() %>% # Makes the id column a normal column
  rename(pv = '.', # Renames the column labelled . to pv
         geo = rowname) # Renames the column labelled rowname to geo

# Dataset with capacity factors

capacity_factor <- left_join(mean_wind, mean_pv, by = 'geo') %>% 
  mutate(geo = case_when(geo == 'GB' ~ 'UK',
                         geo == 'GR' ~ 'EL',
                         .default = as.character(geo)))

# Save the finished data as a Rdata object.

if(!file.exists('data/cf.Rdata')){
  save(capacity_factor, file = 'data/cf.Rdata')
} else {
  print('A file named cf.Rdata already exists in directory')
}
