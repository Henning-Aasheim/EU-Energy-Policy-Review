# Libraries --------------------------------------------------------------------

library(countrycode) # For compability between naming standards
#library(eurostat) # If more data is needed
library(rnaturalearth) # Used to download map
library(rnaturalearthdata) # Used to download higher resolution map
library(rio) # Used for importing excel sheets
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

file_names <- list.files('data/ninja') # Extracts filenames

ninja_list = list() # Makes a list called ninja to be used inside the loop.

for (i in 1:56) { # for the numbers 1 to 56 (I have 56 files) do the following :
  tmp <- read.csv(paste0('data/ninja/', file_names[i]), skip = 2) %>%  # Read the csv files and put them in a temporary file
    mutate(onshore = ifelse('onshore' %in% names(.), onshore, NA), # Check to see if the datasets include variables called onshore and offshore, if not make them and populate them with NA values. This is because some countries do not have offshore wind and the datasets are not compatible without doing this.
           offshore = ifelse('offshore' %in% names(.), offshore, NA), # See above.
           geo = str_extract(file_names[i], '[A-Z]+'), # Make a geo variable to identify country
           type = str_extract(file_names[i], '(wind)|(pv)')) # Make a type variable to identify if PV or wind.
  smr <- tmp %>% group_by(geo, type) %>%  # This is used to keep the geo and type variables when summarising.
    summarise(national = mean(national, na.rm = T),
              onshore = mean(onshore, na.rm = T),
              offshore = mean(offshore, na.rm = T)) # Calculate mean for the variables.
    
  ninja_list[[i]] <- smr # Make a list
}

ninja <- do.call(bind_rows, ninja_list) %>% # Binds the data from ninja_list to one dataframe.
  pivot_longer(cols = c('national', 'onshore', 'offshore'), 
               names_to = 'type_2', values_to = 'cf') %>% 
  mutate(siec = case_when(type == 'pv' & type_2 == 'national' ~ 'RA420',
                          type == 'wind' & type_2 == 'onshore' ~ 'RA310',
                          type == 'wind' & type_2 == 'offshore' ~ 'RA320',
                          .default = NA))



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

# EU Energy Policy Review ------------------------------------------------------

# I use the package rio to load all the excel sheets of the main file.
# import_list() takes the following arguments: file name, the sheets (by number),
# and a command to bind the rows together.

eu_nrg <- import_list('data/EU Energy Policy Review.xlsx', which = 6:33, rbind = T)

# Adding capacity factor

eu_nrg_prod <- eu_nrg %>% 
  filter(siec %in% c('RA310', 'RA320', 'RA420') & (unit == 'GWh' | unit == 'TWh')) %>% 
  left_join(capacity_factor, by = 'geo') %>% 
  mutate(cap_gw = NA,
         cap_gw = case_when(siec == 'RA310' & unit == 'TWh' ~ (cap*1000)/(onshore*8760),
                            siec == 'RA310' & unit == 'GWh' ~ cap/(onshore*8760),
                            siec == 'RA320' & unit == 'TWh' ~ (cap*1000)/(offshore*8760),
                            siec == 'RA320' & unit == 'GWh' ~ cap/(offshore*8760),
                            siec == 'RA420' & unit == 'TWh' ~ (cap*1000)/(pv*8760),
                            siec == 'RA420' & unit == 'GWh' ~ cap/(pv*8760),
                            .default = cap_gw))

eu_nrg_giga <- eu_nrg %>% 
  left_join(capacity_factor, by = 'geo') %>% 
  mutate(cap_gwh = NA,
         cap_gwh = case_when(unit == 'ktoe' ~ cap*11.63, # 11.63 is used as a conversion factor between ktoe and GWh.
                             unit == 'Mtoe' ~ (cap*1000)*11.63, # Mtoe*1000 = ktoe, then 11.63 is used as a conversion factor between ktoe and GWh.
                             unit == 'PJ'   ~ cap*227.77778, # 227.77778 is used as a conversion factor between PJ and GWh.
                             unit == 'TWh'  ~ cap*1000, # TWh*1000 = GWh,
                             .default = NA),
         cap_gw = NA,
         cap_gw = case_when(unit == 'GW' ~ cap,
                            unit == 'MW' ~ cap/1000,
                            siec == 'RA310' & unit == 'TWh' ~ (cap*1000)/(onshore*8760),
                            siec == 'RA310' & unit == 'GWh' ~ cap/(onshore*8760),
                            siec == 'RA320' & unit == 'TWh' ~ (cap*1000)/(offshore*8760),
                            siec == 'RA320' & unit == 'GWh' ~ cap/(offshore*8760),
                            siec == 'RA420' & unit == 'TWh' ~ (cap*1000)/(pv*8760),
                            siec == 'RA420' & unit == 'GWh' ~ cap/(pv*8760),
                            .default = cap_gw),
         cap_gwh_min = NA,
         cap_gwh_min = case_when(unit == 'ktoe' ~ cap_min*11.63, # 11.63 is used as a conversion factor between ktoe and GWh.
                                 unit == 'Mtoe' ~ (cap_min*1000)*11.63, # Mtoe*1000 = ktoe, then 11.63 is used as a conversion factor between ktoe and GWh.
                                 unit == 'PJ'   ~ cap_min*227.77778, # 227.77778 is used as a conversion factor between PJ and GWh.
                                 unit == 'TWh'  ~ cap_min*1000, # TWh*1000 = GWh,
                                 .default = NA),
         cap_gwh_max = NA,
         cap_gwh_max = case_when(unit == 'ktoe' ~ cap_max*11.63, # 11.63 is used as a conversion factor between ktoe and GWh.
                                 unit == 'Mtoe' ~ (cap_max*1000)*11.63, # Mtoe*1000 = ktoe, then 11.63 is used as a conversion factor between ktoe and GWh.
                                 unit == 'PJ'   ~ cap_max*227.77778, # 227.77778 is used as a conversion factor between PJ and GWh.
                                 unit == 'TWh'  ~ cap_max*1000, # TWh*1000 = GWh,
                                 .default = NA),
         cap_gw_min = NA,
         cap_gw_min = case_when(unit == 'GW' ~ cap_min,
                                unit == 'MW' ~ cap_min/1000,
                                siec == 'RA310' & unit == 'TWh' ~ (cap_gwh_min*1000)/(onshore*8760),
                                siec == 'RA310' & unit == 'GWh' ~ cap_gwh_min/(onshore*8760),
                                siec == 'RA320' & unit == 'TWh' ~ (cap_gwh_min*1000)/(offshore*8760),
                                siec == 'RA320' & unit == 'GWh' ~ cap_gwh_min/(offshore*8760),
                                siec == 'RA420' & unit == 'TWh' ~ (cap_gwh_min*1000)/(pv*8760),
                                siec == 'RA420' & unit == 'GWh' ~ cap_gwh_min/(pv*8760),
                                .default = cap_gw_min),
         cap_gw_max = NA,
         cap_gw_max = case_when(unit == 'GW' ~ cap_max,
                                unit == 'MW' ~ cap_max/1000,
                                siec == 'RA310' & unit == 'TWh' ~ (cap_gwh_max*1000)/(onshore*8760),
                                siec == 'RA310' & unit == 'GWh' ~ cap_gwh_max/(onshore*8760),
                                siec == 'RA320' & unit == 'TWh' ~ (cap_gwh_max*1000)/(offshore*8760),
                                siec == 'RA320' & unit == 'GWh' ~ cap_gwh_max/(offshore*8760),
                                siec == 'RA420' & unit == 'TWh' ~ (cap_gwh_max*1000)/(pv*8760),
                                siec == 'RA420' & unit == 'GWh' ~ cap_gwh_max/(pv*8760),
                                .default = cap_gw_max)) %>% 
  select(!c(unit, pv, offshore, onshore, `_file`, cap_min, cap_max, cap))
  


# Eurostat ---------------------------------------------------------------------

# I load two main datasets: one showing capacity (euro_cap) and one showing
# energy production (euro_prod). The actual downloading of the files are found
# in energy_production_capacity.R

## Loading SIEC-code data ------------------------------------------------------

load('data/siec.Rdata')

# Switzerland ------------------------------------------------------------------

# Switzerland is not included in the Eurostat data, so this section loads Swiss 
# data. 

switzerland <- read.csv('data/switzerland.csv', sep = ',') %>% 
  mutate(cap = ifelse(unit == 'MW', cap/1000, cap),
         unit = ifelse(unit == 'MW', 'GW', unit))

## Loads capacity data ---------------------------------------------------------

load('data/nrg.Rdata')

# First processing:

euro_cap <- nrg_retrive %>% 
  filter(geo %in% country_codes & # Filters countries
           plant_tec == 'CAP_NET_ELC' & # Filters capacity
           siec %in% c('RA300', 'RA310', 'RA320', 'RA400', 'RA410', 'RA420', 'N9000')) %>% # Filters type of capacity
  mutate(country = countrycode(geo, 'eurostat', 'country.name.en'), # Makes a country variable with country names
         year = as.numeric(str_remove(TIME_PERIOD, '-01-01')), # Makes a year variable
         gw_capacity = values/1000, # Converts MW to GW
         source = 'EUROSTAT', # Makes a source variable (EUROSTAT)
         scenario = 'obs', # Makes a scenario variable (observed values)
         unit = 'GW') %>%  # Makes a unit variable
  left_join(siec, by = 'siec') %>% # join with SIEC data to get a readable type variable 
  rename(cap = gw_capacity) %>% # Renames gw_capacity
  select(country, geo, siec, type, unit, source, scenario, year, cap) %>%  # Selects the needed variables
  rbind(switzerland[which(switzerland$unit == 'GW'),]) # Adds data from Switzerland

## Loads production data -------------------------------------------------------

load('data/nrg_prod_nf.Rdata')



euro_prod <- nrg_prod_nf_retrieve %>% # Filters countries
  filter(geo %in% country_codes & # Filters countries 
           nrg_bal == 'GEP' & # Filters bal-codes (All codes are found in nrg_bal.Rdata)
           plants == 'TOTAL' & # Filters power plant type
           operator == 'TOTAL', # Filters operator type
         siec %in% c('RA300', 'RA310', 'RA320', 'RA400', 'RA410', 'RA420')) %>% # Filters type of capacity)
  mutate(country = countrycode(geo, 'eurostat', 'country.name.en'), # Makes a country variable with country names
         year = as.numeric(str_remove(TIME_PERIOD, '-01-01')), # Makes a year variable
         source = 'EUROSTAT', # Makes a source variable (EUROSTAT)
         scenario = 'obs', # Makes a scenario variable (observed values)
         unit = 'GWh') %>%  # Makes a unit variable
  left_join(siec, by = 'siec') %>% # join with SIEC data to get a readable type variable 
  rename(cap = values) %>% # Renames values to cap
  select(country, geo, siec, type, unit, source, scenario, year, cap) %>%  # Selects the needed variables
  rbind(switzerland[which(switzerland$unit == 'GWh'),]) # Adds data from Switzerland

## Remove temporary datasets ---------------------------------------------------

rm(nrg_prod_nf_retrieve, nrg_retrive, siec, switzerland)

# Geodata ----------------------------------------------------------------------

# Geodata from rnaturalearth and rnaturalearthdata

geodata <- ne_countries(scale = 50, returnclass = 'sf') %>% 
  filter(admin %in% countries_geo) %>% 
  mutate(geo = iso_a2_eh,
         geo = case_when(geo == 'GB' ~ 'UK',
                         geo == 'GR' ~ 'EL',
                         .default = as.character(geo))) %>% 
  rename(status = type)

