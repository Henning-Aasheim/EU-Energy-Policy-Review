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

# Eurostat country codes, note that this is different from the iso 2 codes. 

country_codes <- list('AT', 'BE', 'BG', 'HR', 'CZ', 'DK', 'EE', 'FI', 'FR', 'DE',
                      'EL', 'HU', 'IE', 'IT', 'LV', 'LT', 'LU', 'NL', 'NO', 'PL',
                      'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'CH', 'UK')

# Countries to be included in the map data, mainly to avoid creating the Sea of 
# Balkan. Note that Serbia is called Republic of Serbia. The reason is that the
# Package rnaturalearth uses this name.

countries_geo <- list("Austria", "Belgium", "Bulgaria", "Czechia", "Germany", 
                      "Denmark", "Estonia", "Greece", "Spain", "Finland", "France", 
                      "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", 
                      "Luxembourg", "Latvia", "Netherlands", "Norway", "Poland", 
                      "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia", 
                      "Switzerland", "United Kingdom", "Kosovo", "Republic of Serbia",
                      'North Macedonia', 'Albania', 'Bosnia and Herzegovina', 
                      'Moldova', 'Montenegro')

# Capacity targets 2030 --------------------------------------------------------

# Filter from the main dataset (eu_nrg) all target values that are in GW for 2030
# and only for (wind onshore [RA310], wind offshore [RA320], and solar 
# photovoltaic [RA420]).

trg_2030 <- eu_nrg_giga %>% 
  filter(target == 1 & year == 2030  & siec %in% c('RA310', 'RA320', 'RA420'))

# Joins the target data with the map data.

target_2030 <- inner_join(geodata, trg_2030, by = 'geo') 

# Creates a tmap plot using the EPSG:3035 projection. 

tm_shape(geodata, crs = 'EPSG:3035', # Dataset and projection for the base map.
         xlim = c(2400000, 6000000), # Latitude limits (Note that these are not the normal ones).
         ylim = c(1320000, 5500000)) + # Longitude limits.
  tm_fill('grey') + # Fill for the base map.
  tm_borders(col = 'darkgrey') + # borders for the base map.
  tm_shape(target_2030) + # Dataset for the target map.
  tm_polygons(fill = 'cap_gw', # Data for the target map (using the variable cap).
              fill.legend = tm_legend(frame = FALSE, # Removes the border around the legend (But has to be used in tandem with fill.scale)
                                      title ='Target capacity', # Adds title to the legend.
                                      orientation = 'landscape', # Puts the legend in landscape mode.
                                      position = tm_pos_out('center', 'bottom', pos.h = 'center'), # Centres the legend at the bottom.
                                      width = 60, # Sets the Width of the legend.
                                      title.align = 'center', # Helps to centre the legend title. How? Idk...
                                      text.size = .5), # Titles the legend
              fill.scale = tm_scale_continuous(limits = c(0, 215))) + # Breaks set the intervals shown in the legend
  tm_facets('type', drop.NA.facets = T, type = 'stack') + # Makes seperat facet plots for wind onshore, wind offshore and solar photovoltaic
  tm_layout(panel.label.bg.color = 'white', # Sets panel lable background color to white.
            frame = F, # Removes border around map.
            panel.label.frame = F, # Removes the border around the panel label
            title.position = tm_pos_out('center', 'top', pos.h = 'center')) + # Removes border around the panel label.
  tm_title('Capacity targets for 2030 (GW)') # Title

# Capacity targets 2040 --------------------------------------------------------

# Filter from the main dataset (eu_nrg) all target values that are in GW for 2040
# and only for (wind onshore [RA310], wind offshore [RA320], and solar 
# photovoltaic [RA420]).

trg_2040 <- eu_nrg_giga %>% 
  filter(target == 1 & year == 2040  & siec %in% c('RA310', 'RA320', 'RA420'))

# Joins the target data with the map data.

target_2040 <- inner_join(geodata, trg_2040, by = 'geo') 

# Creates a tmap plot using the EPSG:3035 projection. 

tm_shape(geodata, crs = 'EPSG:3035', # Dataset and projection for the base map.
         xlim = c(2400000, 6000000), # Latitude limits (Note that these are not the normal ones).
         ylim = c(1320000, 5500000)) + # Longitude limits.
  tm_fill('grey') + # Fill for the base map.
  tm_borders(col = 'darkgrey') + # borders for the base map.
  tm_shape(target_2040) + # Dataset for the target map.
  tm_polygons(fill = 'cap_gw', # Data for the target map (using the variable cap).
              fill.legend = tm_legend(frame = FALSE, # Removes the border around the legend (But has to be used in tandem with fill.scale)
                                      title ='Target capacity', # Adds title to the legend.
                                      orientation = 'landscape', # Puts the legend in landscape mode.
                                      position = tm_pos_out('center', 'bottom', pos.h = 'center'), # Centres the legend at the bottom.
                                      width = 60, # Sets the Width of the legend.
                                      title.align = 'center', # Helps to centre the legend title. How? Idk...
                                      text.size = .5), # Titles the legend
              fill.scale = tm_scale_continuous(limits = c(0, 400))) + # Breaks set the intervals shown in the legend
  tm_facets('type', drop.NA.facets = T, type = 'stack') + # Makes seperat facet plots for wind onshore, wind offshore and solar photovoltaic
  tm_layout(panel.label.bg.color = 'white', # Sets panel lable background color to white.
            frame = F, # Removes border around map.
            panel.label.frame = F, # Removes the border around the panel label
            title.position = tm_pos_out('center', 'top', pos.h = 'center')) + # Removes border around the panel label.
  tm_title('Capacity targets for 2040 (GW)') # Title

# Targets 2030-2050 ------------------------------------------------------------

trg <- eu_nrg_giga %>% 
  filter(target == 1 & 
         year %in% c(2030, 2040, 2050) & 
         siec %in% c('RA310', 'RA320', 'RA420')) %>% 
  mutate(type2 = paste0(type, ' ', year))

# Joins the target data with the map data.

target <- inner_join(geodata, trg, by = 'geo') 

# Creates a tmap plot using the EPSG:3035 projection. 

map <- tm_shape(geodata, crs = 'EPSG:3035', # Dataset and projection for the base map.
         xlim = c(2400000, 6000000), # Latitude limits (Note that these are not the normal ones).
         ylim = c(1320000, 5500000)) + # Longitude limits.
  tm_fill('grey') + # Fill for the base map.
  tm_borders(col = 'darkgrey') + # borders for the base map.
tm_shape(target) + # Dataset for the target map.
  tm_polygons(fill = 'cap_gw', # Data for the target map (using the variable cap).
              fill.legend = tm_legend(frame = FALSE, # Removes the border around the legend (But has to be used in tandem with fill.scale)
                                      title ='Target capacity (logarithmic scale)', # Adds title to the legend.
                                      orientation = 'landscape', # Puts the legend in landscape mode.
                                      position = tm_pos_out('center', 'bottom', pos.h = 'center'), # Centres the legend at the bottom.
                                      width = 60, # Sets the Width of the legend.
                                      title.align = 'center', # Helps to centre the legend title. How? Idk...
                                      text.size = .5), # Titles the legend
              fill.scale = tm_scale_continuous(limits = c(0, 400), trans = 'log1p', ticks = c(0, 1.2, 3.5, 8, 20, 35, 90, 150, 400))) + # Breaks set the intervals shown in the legend
  tm_facets('type', drop.NA.facets = T, columns = 'year') + # Makes seperat facet plots for wind onshore, wind offshore and solar photovoltaic
  tm_layout(panel.label.bg.color = 'white', # Sets panel lable background color to white.
            frame = F, # Removes border around map.
            panel.label.frame = F, # Removes the border around the panel label
            title.position = tm_pos_out('center', 'top', pos.h = 'center')) + # Removes border around the panel label.
  tm_title('Capacity targets for 2030, 2040, and 2050 (GW)') + # Title
  tm_options()

tmap_save(map, dpi = 300, height = 2000, width = 2000, outer.margins = NA)
  
# Map of the regions -----------------------------------------------------------

# Map of regions which is generally based on van Greevenbroek et al., but is
# subject to change. 

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

# Joins the region data with the map data.

regions_map <- inner_join(geodata, regions, by = 'geo')

# Creates a tmap plot using the EPSG:3035 projection.

tm_shape(geodata, projection = 'EPSG:3035', # Dataset and projection for the base map
         xlim = c(2400000, 6000000), # Latitude limits (Note that these are not the normal ones)
         ylim = c(1320000, 5500000)) + # Longitude limits
  tm_fill('grey') + # Fill for the base map
  tm_borders(col = 'darkgrey') + # borders for the base map
  tm_shape(regions_map) + # Dataset for the regions
  tm_polygons(title = 'Targets', fill = 'regions') # Data for the regions (using regions as the main variable)

# Approach map -----------------------------------------------------------------

# There are two main approaches to targets, one is a bottom-up one, and the other
# is top down. This is a map based on our own impressions.

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

# Joins the approach data with the map data.

approach_map <- inner_join(geodata, approach, by = 'geo') %>% 
  mutate(approach = as.factor(approach))

# Creates a tmap plot using the EPSG:3035 projection.

tm_shape(geodata, projection = 'EPSG:3035', # Dataset and projection for the base map
         xlim = c(2400000, 6000000), # Latitude limits (Note that these are not the normal ones)
         ylim = c(1320000, 5500000)) + # Longitude limits
  tm_fill('grey') + # Fill for the base map
  tm_borders(col = 'darkgrey') + # borders for the base map
  tm_shape(approach_map) + # Dataset for the approaches
  tm_polygons(title = 'Targets', fill = 'approach') # Data for the approaches (using approach as the main variable)




