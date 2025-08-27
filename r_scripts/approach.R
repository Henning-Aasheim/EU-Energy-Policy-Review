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
