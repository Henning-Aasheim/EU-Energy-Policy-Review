# Map of the regions -----------------------------------------------------------

library(tmap)
library(tidyverse)

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

# Colours for the regions

colours = c('Adriatic' = '#f89c99',
            'Baltics and Poland' = '#baa046',
            'British Isles' = '#a5dae6',
            'Central' = '#ac7aa2',
            'East' = '#c56576',
            'Iberia' = '#edcd5a',
            'Nordics' = '#7eabdf',
            'West' = '#41a021')

# MAP --------------------------------------------------------------------------

# Creates a tmap plot using the EPSG:3035 projection.

r_map = tm_shape(geodata, projection = 'EPSG:3035', # Dataset and projection for the base map
         xlim = c(2400000, 6000000), # Latitude limits (Note that these are not the normal ones)
         ylim = c(1320000, 5500000)) + # Longitude limits
  tm_fill('grey') + # Fill for the base map
  tm_borders(col = 'darkgrey') + # borders for the base map
  tm_shape(regions_map) + # Dataset for the regions
  tm_polygons(fill = 'regions', # The fill uses regions as a variable
              fill.legend = tm_legend(position = tm_pos_in('left', 'top'), # Fixes the legend to the top left (the tm_legend must come inside tm_polygon)
                                      title = 'Regions', # Gives the legend a title
                                      title.size = 1.1, # Sets the title to be size 1.1
                                      title.fontface = 'bold', # Sets the title to be bold
                                      text.size = .9, # Sets the size of text to be 0.9
                                      na.show = F, # Excludes NA/missing category in the legend
                                      frame = F, # Removes the frame around the legend
                                      bg.color = 'transparent'), # Sets the legend background to be transparent
              fill.scale = tm_scale_categorical(values = colours)) + # Sets new colour for the regions (can only be done using the fill.scale = inside tm_polygon)
  tm_layout(frame = F) # Removes the frame around the map

tmap_save(r_map, dpi = 300, height = 2000, width = 2000, outer.margins = NA)
