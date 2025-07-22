# Libraries --------------------------------------------------------------------

library(countrycode)
library(eurostat)
library(rio)
library(tidyverse)

# Lists ------------------------------------------------------------------------

# Lists of the 28 countries that are included.

countries <- list("Austria", "Belgium", "Bulgaria", "Czechia", "Germany", 
                  "Denmark", "Estonia", "Greece", "Spain", "Finland", "France", 
                  "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", 
                  "Luxembourg", "Latvia", "Netherlands", "Norway", "Poland", 
                  "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia", 
                  "Switzerland", "United Kingdom")

country_codes <- list('BE', 'BG', 'CZ', 'DK', 'DE', 'EE', 'IE', 'EL', 'ES', 'FR', 
                      'HR', 'IT', 'LV', 'LT', 'LU', 'HU', 'NL', 'AT', 'PL', 'PT', 
                      'RO', 'SI', 'SK', 'FI', 'SE', 'NO', 'CH', 'UK')

# EU Energy Policy Review ------------------------------------------------------

# I use the package rio to load all the excel sheets of the main file.
# import_list() takes the following arguments: file name, the sheets (by number),
# and a command to bind the rows together.

eu_nrg <- import_list('EU Energy Policy Review.xlsx', which = 6:33, rbind = T)

# Eurostat ---------------------------------------------------------------------

# I load two main datasets: one showing capacity (euro_cap) and one showing
# energy production (euro_prod). The actual downloading of the files are found
# in energy_production_capacity.R