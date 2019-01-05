library('data.table') # data manipulation
library('tibble') # data wrangling

library('dplyr')
library('tidyr')
library('ggplot2')

geography <- as.tibble(fread('kiva_mpi_region_locations.csv')) %>%
  filter(country == 'Philippines') %>%
  mutate(country = as.factor(country),
         LocationName = as.factor(LocationName),
         region = as.factor(region),
         geo = as.factor(geo)) 
  
partner <- as.tibble(fread('loan_themes_by_region.csv')) %>%
  filter(country == 'Philippines') %>%
  mutate(mpi_region = as.factor(mpi_region),
         LocationName = as.factor(LocationName),
         geo = as.factor(geo))

ggplot(partner, aes(number, amount, size = amount)) +
  geom_point(alpha = 1/5) 
  #geom_text(aes(y = log(amount) + .3, label = mpi_region))


partner.region <- partner %>%
  group_by(mpi_region) %>%
  summarise(number = mean(number),
            amount = mean(amount)) %>%
  separate(mpi_region, c('region', 'country'), sep = ',') 
#  left_join(geography, by = 'region')


ggplot(partner.region, aes(log(number), log(amount))) +
  geom_point() +
  geom_text(aes(y = log(amount) + .45, label = region))

library(leaflet)
pal <- colorNumeric(palette = c('darkgreen', 'yellow'), domain = partner$number)
leaflet(data = partner) %>% 
  addTiles() %>%
  addCircleMarkers(~lon, ~lat, group = ~mpi_region, opacity = 0.2,
                   fillColor = ~pal(partner$number), stroke = FALSE,
                   #clusterOptions = markerClusterOptions(),
                   radius = ~log(number)) %>%
  addLegend('bottomright', pal = pal, values = partner$number, 
            title = 'Kiva Loans', opacity = 1)


## Loading Data
kiva <- as.tibble(fread('kiva_loans.csv')) %>%
  mutate(activity = as.factor(activity),
         sector = as.factor(sector),
         country_code = as.factor(country_code),
         country = as.factor(country),
         region = as.factor(region),
         currency = as.factor(currency),
         disbursed_time = as.Date(disbursed_time),
         funded_time = as.Date(funded_time),
         partner_id = as.factor(partner_id),
         borrower_genders = as.factor(borrower_genders),
         repayment_interval = as.factor(repayment_interval))
# Philippines
kiva.ph <- kiva %>%
  filter(country == 'Philippines') %>%
  mutate(year = substring(funded_time, 1, 4),
         month = substring(funded_time, 6, 7))