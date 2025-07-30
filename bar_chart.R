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
  select(!c(source, scenario, unit))

eu_nrg_giga %>% 
  filter(siec %in% c('RA310', 'RA320', 'RA420') &
           ((year == 2030 & target == 1) | (year == 2040 & target == 1))) %>% 
  select(country, geo, siec, type, year, cap_gw) %>% 
  rename(cap = cap_gw) %>% 
  rbind(euro_cap_2023) %>% 
#  complete(year, nesting(country, geo, siec, type)) %>% 
  pivot_wider(names_from = year, values_from = cap) %>% 
  mutate(cap_2023 = `2023`,
         cap_2023 = ifelse(is.na(cap_2023), 0, cap_2023), # Makes the calculations possible, the assumption, however, is wrong.
         cap_2030 = `2030` - `cap_2023`,
         cap_2030 = ifelse(is.na(cap_2030), 0, cap_2030), # Makes the calculations possible, the assumption, however, is wrong.
         cap_2040 = `2040` - (`cap_2030` + `cap_2023`)) %>% 
  select(!c(`2023`, `2030`, `2040`)) %>% # Removes the whole cap numbers.
  pivot_longer(cols = starts_with('cap'), names_to = 'year', values_to = 'cap') %>% # Reverts list to long format.
  mutate(year = as.numeric(str_remove(year, '[^0-9]+'))) %>%  # Removes cap_ and makes the years numeric.
  ggplot(aes(x = fct_reorder(country, cap), y = cap)) +
  geom_bar(aes(fill = factor(year, levels = c('2040', '2030', '2023'))), stat = 'identity', position = 'stack') +
  facet_wrap(~factor(type), scale = 'free_x') +
  coord_flip() +
  scale_fill_manual(values = c('#227C9D', '#17C3B2', '#FFCB77')) +
  labs(x = 'Countries',
       y = 'Capacity',
       fill = 'Year') +
  theme_minimal()
