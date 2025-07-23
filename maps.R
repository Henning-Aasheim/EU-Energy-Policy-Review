# Libraries --------------------------------------------------------------------

library(tmap)
library(sf)
library(tidyverse)

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

# Capacity targets 2030 --------------------------------------------------------

trg_2030 <- eu_nrg %>% 
  filter(target == 1 & year == 2030 & unit == 'GW' & siec %in% c('RA310', 'RA320', 'RA420'))

target_2030 <- inner_join(geodata, trg_2030, by = 'geo') 

tm_shape(geodata, projection = 'EPSG:3035',
         xlim = c(2400000, 6000000),
         ylim = c(1320000, 5500000)) +
  tm_fill('grey') +
  tm_borders(col = 'darkgrey') +
  tm_shape(target_2030) +
  tm_polygons('Capacity targets for 2030 (GW)', title = 'Targets', fill = 'cap',
              breaks = c(0, 5, 10, 20, 50, 100, 200)) +
  tm_facets('type', drop.NA.facets = T) +
  tm_layout(legend.position = tm_pos_out('right', 'center')) +
  tm_title('Capacity targets for 2030 (GW)')

# Capacity targets 2040 --------------------------------------------------------

trg_2040 <- eu_nrg %>% 
  filter(target == 1 & year == 2040 & unit == 'GW' & siec %in% c('RA310', 'RA320', 'RA420'))

target_2040 <- inner_join(geodata, trg_2040, by = 'geo') 

tm_shape(geodata, projection = 'EPSG:3035',
         xlim = c(2400000, 6000000),
         ylim = c(1320000, 5500000)) +
  tm_fill('grey') +
  tm_borders(col = 'darkgrey') +
  tm_shape(target_2040) +
  tm_polygons('Capacity targets for 2040 (GW)', title = 'Targets', fill = 'cap', 
              breaks = c(0, 5, 10, 20, 50, 100, 200)) +
  tm_facets('type', drop.NA.facets = T) +
  tm_layout(legend.position = tm_pos_out('right', 'center')) +
  tm_title('Capacity targets for 2040 (GW)')

# Map of the regions -----------------------------------------------------------

regions <- data.frame(
  country = c("Austria", "Belgium", "Bulgaria", "Croatia", "Czechia", 
                 "Denmark", "Estonia", "Finland", "France", "Germany", 
                 "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", 
                 "Luxembourg", "Netherlands", "Norway", "Poland", "Portugal", 
                 "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", 
                 "Switzerland", "United Kingdom"),
  geo = c('AT', 'BE', 'BG', 'HR', 'CZ', 'DK', 'EE', 'FI', 'FR', 'DE', 'EL', 'HU', 
          'IE', 'IT', 'LV', 'LT', 'LU', 'NL', 'NO', 'PL', 'PT', 'RO', 'SK', 'SI', 
          'ES', 'SE', 'CH', 'UK'),
# Regions are divided roughly based on the regions from van Greevenbroek et al.
  regions = c('Central', # Austria
              'West', # Belgium
              'East', # Bulgaria
              'Adriatic', # Croatia
              'Central', # Czechia
              'Nordics', # Denmark
              'Baltics and Poland', # Estonia 
              'Nordics', # Finland
              'West', # France
              'Central', # Germany
              'Adriatic', # Greece
              'East', # Hungary
              'British Isles', # Ireland
              'Adriatic', # Italy
              'Baltics and Poland', # Latvia 
              'Baltics and Poland', # Lithuania
              'West', # Luxembourg
              'West', # Netherlands
              'Nordics', # Norway
              'Baltics and Poland', # Poland 
              'Iberia', # Portugal
              'East', # Romania
              'East', # Slovakia
              'Central', # Slovenia
              'Iberia', # Spain
              'Nordics', # Sweden
              'Central', # Switzerland
              'British Isles' # UK
              ) 
)

regions_map <- inner_join(geodata, regions, by = 'geo')

tm_shape(geodata, projection = 'EPSG:3035',
         xlim = c(2400000, 6000000),
         ylim = c(1320000, 5500000)) +
  tm_fill('grey') +
  tm_borders(col = 'darkgrey') +
  tm_shape(regions_map) +
  tm_polygons(title = 'Targets', fill = 'regions')

# Approach map -----------------------------------------------------------------

approach <- data.frame(
  country = c("Austria", "Belgium", "Bulgaria", "Croatia", "Czechia", 
              "Denmark", "Estonia", "Finland", "France", "Germany", 
              "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", 
              "Luxembourg", "Netherlands", "Norway", "Poland", "Portugal", 
              "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", 
              "Switzerland", "United Kingdom"),
  geo = c('AT', 'BE', 'BG', 'HR', 'CZ', 'DK', 'EE', 'FI', 'FR', 'DE', 'EL', 'HU', 
          'IE', 'IT', 'LV', 'LT', 'LU', 'NL', 'NO', 'PL', 'PT', 'RO', 'SK', 'SI', 
          'ES', 'SE', 'CH', 'UK'),
# Approach: 0 = Bottom up, 1 = Top-Down.
  approach = c(0, # Austria
               1, # Belgium
               1, # Bulgaria
               0, # Croatia
               0, # Czechia
               1, # Denmark
               0, # Estonia
               0, # Finland
               0, # France
               1, # Germany
               0, # Greece
               0, # Hungary
               0, # Ireland
               0, # Italy
               0, # Latvia
               0, # Lithuania
               1, # Luxembourg
               0, # Netherlands
               0, # Norway
               1, # Poland
               0, # Portugal
               0, # Romania
               0, # Slovakia
               1, # Slovenia
               0, # Spain
               0, # Sweden
               0, # Switzerland
               1  # UK
               )
)

approach_map <- inner_join(geodata, approach, by = 'geo') %>% 
  mutate(approach = as.factor(approach))

tm_shape(geodata, projection = 'EPSG:3035',
         xlim = c(2400000, 6000000),
         ylim = c(1320000, 5500000)) +
  tm_fill('grey') +
  tm_borders(col = 'darkgrey') +
  tm_shape(approach_map) +
  tm_polygons(title = 'Targets', fill = 'approach')




