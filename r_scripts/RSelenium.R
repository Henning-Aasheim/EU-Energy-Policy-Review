# Libraries --------------------------------------------------------------------

library(RSelenium)
library(rvest)
library(tidyverse)

# Info -------------------------------------------------------------------------

countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Czech Republic", 
                  "Denmark", "Estonia", "Finland", "France", "Germany", 
                  "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", 
                  "Luxembourg", "Netherlands", "Norway", "Poland", "Portugal", 
                  "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", 
                  "Switzerland", "Great Britain")

# URL for Renewables.ninja

url <- 'https://renewables.ninja'

# The download directory does not work:(

down_dir <- 'C:/Users/henni/OneDrive/UiO/EU Energy Policy/EU-Energy-Policy-Review/data/ninja'

# See above

ff_opts <- makeFirefoxProfile(list(
  browser.download.dir = down_dir
  )
)


# RSelenium --------------------------------------------------------------------

# Initialising server and navigation to the right page

rD <- RSelenium::rsDriver(browser = 'firefox', version = 'latest', # Starts RSelenium server (firefox).
                          phantomver = NULL, chromever = NULL, # Phantomver and chromever has to be set to NULL to make things work.
                          extraCapabilities = ff_opts) # Sets download directory (doesn't work).
remDr <- rD[['client']] # ?
remDr$navigate(url) # Navigates to the renewables.ninja page.
Sys.sleep(4) # Waits for the page to load.

# Closes the opening message

welcome <- remDr$findElement(using = 'css selector', 'button.close')
welcome$clickElement()
Sys.sleep(1)

# Navigates to the country menu

button <- remDr$findElement(using = 'xpath', '//button[text()="Country"]') # Finds the select countries button.
button$clickElement() # clicks the select countries button. 
Sys.sleep(2) # Waits for this to happen.

# Navigate to the select country drop-down menu

selector <- remDr$findElement(using = 'css selector', 'div.selection') # Finds the selector div.
selector$clickElement() # clicks the selector div. 
Sys.sleep(2) # Waits for this to happen.

# Selects the input

input <- remDr$findElement(using = 'css selector', 'input#id_country') # Finds the input field.



# Loop through the relevant countries.
for (country in countries){
  
  # Finds the result box for every country
  
  input$clickElement() # Clicks the input field.
  Sys.sleep(1) # Waits for this to happen.
  input$clearElement() #  Clears input data.
  Sys.sleep(1) # Waits for this to happen.
  input$sendKeysToElement(list(country)) # Enters input data. (However, pressing enter here does not work so wee need an additional step, this is the fault of the page, not me, promise)
  Sys.sleep(1) # Waits for the input to be recognised.
  span <- remDr$findElement(using = 'css selector', 'span.control-select__option--highlighted') # Finds the highlighted country based on the input.
  span$clickElement() # Presses enter.
  Sys.sleep(2)
  
  pv_down <- remDr$findElement(using = 'link text', 'PV (MERRA-2)')
  pv_down$clickElement()
  
  # Wait for download to complete
  Sys.sleep(5)
  
  wind_down <- remDr$findElement(using = 'link text', 'Wind (Current fleet, onshore/offshore separate, MERRA-2)')
  wind_down$clickElement()
  
  Sys.sleep(5)
}


# Close the RSelenium driver

remDr$close()
rD[["server"]]$stop()
