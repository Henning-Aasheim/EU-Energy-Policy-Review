# Libraries --------------------------------------------------------------------

library(forcats)
library(tidyverse)

# Load necessary data ----------------------------------------------------------

# Target data 

load('data/energy_target_capacity.Rdata')

# Eurostat data

load('data/eurostat_capacity.Rdata')

# Including Eurostat data ------------------------------------------------------

euro_cap_2023 <- eurostat_capacity %>% 
  filter(year == 2023 & siec %in% c('RA310', 'RA320', 'RA420')) %>% 
  select(!c(source, scenario, unit))

bar_chart <- energy_target_capacity %>% 
  filter(siec %in% c('RA310', 'RA320', 'RA420') &
           ((year == 2030 & target == 1) | (year == 2040 & target == 1) | (year == 2023 & source == 'UKGOV'))) %>% 
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
  geom_bar(aes(fill = factor(year, levels = c('2040', '2030', '2023'))), 
           stat = 'identity', position = 'stack') +
  facet_wrap(~factor(type), scale = 'free_x') +
  coord_flip() +
  scale_fill_manual(values = c('#FFCB77', '#17C3B2', '#227C9D'), limits = c('2023', '2030', '2040')) +
  labs(x = 'Countries',
       y = 'Capacity',
       fill = 'Year:') +
  theme_minimal() +
  theme(axis.title.x = element_text(margin = margin(t = 5, unit = 'mm'), 
                                    face = 'bold', size = 15),
        axis.title.y = element_text(margin = margin(r = 5, unit = 'mm'), 
                                    face = 'bold', size = 15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 13, face = 'bold'),
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 10),
        legend.position = 'bottom',
        panel.grid.major.y = element_blank())

bar_chart

#ggsave('img/bar_chart.png', width = 30, height = 20, units = 'cm', dpi = 300, bg = 'white')
