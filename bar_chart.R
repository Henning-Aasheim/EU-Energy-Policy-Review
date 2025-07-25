# Libraries --------------------------------------------------------------------

library(data.table)
library(forcats)
library(tidyverse)

# Creates a data.table structure from the data set which is imported 

dta <- as.data.table(eu_nrg)

#Values of interest which can be looked at for (here done as wind and solar)

values_of_interest <- list("RA300", "RA400")

# A for loop which iterates over the different countries where black is 2023 and grey is the target of 2030

for (x in values_of_interest){
  plot <- ggplot(dta[(year == 2023 | (year == 2030& target == 1)) & unit == "GW" & siec == x], #This subdivides the data
                 aes(x = country, y = cap, fill = as.factor(year))) +
    scale_fill_manual(values = c("2023" = "Black", "2030" = "grey")) +
    geom_bar(stat = "identity", position = "identity", alpha = 0.5) +
    ggtitle("Capacity of GW by country in",x) +
    coord_flip()
  print(plot)
}

# Including Eurostat data ------------------------------------------------------

euro_cap_2023 <- euro_cap %>% 
  filter(year == 2023 & siec %in% c('RA310', 'RA320', 'RA420')) %>% 
  select(!c(source, scenario)) %>% 
  mutate(target = 0)

eu_nrg %>% 
  filter(siec %in% c('RA310', 'RA320', 'RA420') & unit == 'GW' & 
           ((year == 2030 & target == 1) | (year == 2040 & target == 1))) %>% 
  select(country, geo, siec, type, unit, year, cap, target) %>% 
  rbind(euro_cap_2023) %>% 
  complete(year, nesting(country, geo, siec, type)) %>% 
  ggplot(aes(x = fct_reorder(country, cap), y = cap, fill = factor(year))) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_wrap(~factor(type), scale = 'free_x') +
  coord_flip()
