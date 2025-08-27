# Libraries --------------------------------------------------------------------

library(tmap)
library(sf)
library(tidyverse)

# Data -------------------------------------------------------------------------

# Target data

load('data/energy_target_capacity.Rdata')

# Geodata

load('data/geodata.Rdata')

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

trg_2030 <- energy_target_capacity %>% 
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
                                      text.size = .5), # Changes legend size (smaller becomes larger somehow)
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

trg_2040 <- energy_target_capacity %>% 
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

trg <- energy_target_capacity %>% 
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

map

#tmap_save(map, dpi = 300, height = 2000, width = 2000, outer.margins = NA)





