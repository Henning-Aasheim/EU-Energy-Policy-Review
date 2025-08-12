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
