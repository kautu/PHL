library('data.table') # data manipulation
library('tibble') # data wrangling

library('dplyr')
library('tidyr')
library('ggplot2')

library(stringr)

## 
partner <- as.tibble(fread('loan_themes_by_region.csv', encoding = 'UTF-8')) %>%
  filter(country == 'Philippines') %>%
  mutate(mpi_region = as.factor(mpi_region),
         LocationName = as.factor(LocationName),
         geo = as.factor(geo)) 
##
theme.id <- subset(as.tibble(fread('loan_theme_ids.csv')), 
                   `Partner ID` %in% unique(partner$`Partner ID`))
## 
kiva.loan <- subset(as.tibble(fread('kiva_loans.csv', encoding = 'UTF-8')), country == 'Philippines') 
kiva.loan$date <- as.Date(kiva.loan$date)
##
kiva.join <- left_join(kiva.loan, theme.id, by = 'id') %>% 
  separate(region, c('region1','region2'), sep = ', ', remove = FALSE)

#n_distinct(kiva.join$region) 
region.summary <- kiva.join %>% 
  group_by(region2, region1) %>% 
  summarise(n = n()) %>% 
  unite(region, c('region1','region2'), sep = ', ', remove = FALSE) %>% 
  arrange(-n) ## Ordering in the concentrated regions in scale

#grep("Tukuran", region.summary$region,  ignore.case = TRUE)
region.summary[grep("molave", region.summary$region, ignore.case = TRUE),]$region2 <- 'Zamboanga Del Sur'
region.summary[grep("molave", region.summary$region, ignore.case = TRUE),]$region1 <- 'Molave'

region.summary[grep("Tukuran", region.summary$region, ignore.case = TRUE),]$region2 <- 'Zamboanga Del Sur'
region.summary[grep("Tukuran", region.summary$region, ignore.case = TRUE),]$region1 <- 'Tukuran'
#region.summary[which(str_detect(region.summary$region, "tukuran") == TRUE),2:3] <- c('Zamboanga Del Sur', 'Tukuran')
region.summary[grep("Pagadian", region.summary$region, ignore.case = TRUE),]$region2 <- 'Zamboanga Del Sur'
region.summary[grep("Pagadian", region.summary$region, ignore.case = TRUE),]$region1 <- 'Pagadian City'
region.summary[grep("Josefina", region.summary$region, ignore.case = TRUE),]$region2 <- 'Zamboanga Del Sur'
region.summary[grep("Josefina", region.summary$region, ignore.case = TRUE),]$region1 <- 'Josefina'
region.summary[grep("Tambulig", region.summary$region, ignore.case = TRUE),]$region2 <- 'Zamboanga Del Sur'
region.summary[grep("Tambulig", region.summary$region, ignore.case = TRUE),]$region1 <- 'Tambulig'
region.summary[grep("Mahayag", region.summary$region, ignore.case = TRUE),]$region2 <- 'Zamboanga Del Sur'
region.summary[grep("Mahayag", region.summary$region, ignore.case = TRUE),]$region1 <- 'Mahayag'
region.summary[grep("Dumingag", region.summary$region, ignore.case = TRUE),]$region2 <- 'Zamboanga Del Sur'
region.summary[grep("Dumingag", region.summary$region, ignore.case = TRUE),]$region1 <- 'Dumingag'


region.summary[grep("Iloilo", region.summary$region, ignore.case = TRUE),]$region2 <- 'Iloilo'


region.summary[grep("sogod", region.summary$region, ignore.case = TRUE),]$region2 <- 'Leyte'
region.summary[grep("sogod", region.summary$region, ignore.case = TRUE),]$region1 <- 'Sogod'
region.summary[grep("Hilongos", region.summary$region, ignore.case = TRUE),]$region2 <- 'Leyte'
region.summary[grep("Hilongos", region.summary$region, ignore.case = TRUE),]$region1 <- 'Hilongos'
region.summary[grep("Mayorga", region.summary$region, ignore.case = TRUE),]$region2 <- 'Leyte'
region.summary[grep("Mayorga", region.summary$region, ignore.case = TRUE),]$region1 <- 'Mayorga'

region.summary[grep("roxas palawan", region.summary$region, ignore.case = TRUE),]$region2 <- 'Palawan'
region.summary[grep("roxas palawan", region.summary$region, ignore.case = TRUE),]$region1 <- 'Roxas'

region.summary[grep("Kabankalan", region.summary$region, ignore.case = TRUE),]$region2 <- 'Negros Occidental'
region.summary[grep("Kabankalan", region.summary$region, ignore.case = TRUE),]$region1 <- 'Kabankalan'
region.summary[grep("Hinigaran", region.summary$region, ignore.case = TRUE),]$region2 <- 'Negros Occidental'

region.summary[grep("Ayungon", region.summary$region, ignore.case = TRUE),]$region2 <- 'Negros Occidental'
region.summary[grep("Ayungon", region.summary$region, ignore.case = TRUE),]$region1 <- 'Ayungon'

region.summary[grep("Aloguinsan", region.summary$region, ignore.case = TRUE),]$region2 <- 'Cebu'
region.summary[grep("Aloguinsan", region.summary$region, ignore.case = TRUE),]$region1 <- 'Aloguinsan'

region.summary[grep("Kalibo", region.summary$region, ignore.case = TRUE),]$region2 <- 'Aklan'
region.summary[grep("Kalibo", region.summary$region, ignore.case = TRUE),]$region1 <- 'Kalibo'

region.summary[grep("Binalbagan", region.summary$region, ignore.case = TRUE),]$region1 <- 'Binalbagan'
region.summary[grep("Baybay City", region.summary$region, ignore.case = TRUE),]$region1 <- 'Baybay'

region.summary[grep("Baliangao", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Baliangao", region.summary$region, ignore.case = TRUE),]$region1 <- 'Baliangao'
region.summary[grep("Calamba, Mis", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Calamba Mis", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Calamba Mis", region.summary$region, ignore.case = TRUE),]$region1 <- 'Calamba'
region.summary[grep("tudela mis", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("tudela mis", region.summary$region, ignore.case = TRUE),]$region1 <- 'Tudela'
region.summary[grep("tudela, mis", region.summary$region, ignore.case = TRUE),]$region1 <- 'Tudela'
region.summary[grep("tudela ,", region.summary$region, ignore.case = TRUE),]$region1 <- 'Tudela'
region.summary[grep("tudela ,", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("tudela,mis", region.summary$region, ignore.case = TRUE),]$region1 <- 'Tudela'
region.summary[grep("tudela,mis", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("nailon", region.summary$region, ignore.case = TRUE),]$region1 <- 'Tudela'
region.summary[grep("nailon", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("clarin mis", region.summary$region, ignore.case = TRUE),]$region1 <- 'Clarin'
region.summary[grep("clarin mis", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("clarin, mis", region.summary$region, ignore.case = TRUE),]$region1 <- 'Clarin'
region.summary[grep("clarin, mis", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("clarin,mis", region.summary$region, ignore.case = TRUE),]$region1 <- 'Clarin'
region.summary[grep("clarin,mis", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Dela Paz, Clarin", region.summary$region, ignore.case = TRUE),]$region1 <- 'Clarin'
region.summary[grep("Dela Paz, Clarin", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Gata Daku", region.summary$region, ignore.case = TRUE),]$region1 <- 'Clarin'
region.summary[grep("Gata Daku", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Lapasan", region.summary$region, ignore.case = TRUE),]$region1 <- 'Clarin'
region.summary[grep("Lapasan", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Mialen", region.summary$region, ignore.case = TRUE),]$region1 <- 'Clarin'
region.summary[grep("Mialen", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Sebasi", region.summary$region, ignore.case = TRUE),]$region1 <- 'Clarin'
region.summary[grep("Sebasi", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Segatic", region.summary$region, ignore.case = TRUE),]$region1 <- 'Clarin'
region.summary[grep("Segatic", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("clarin", region.summary$region, ignore.case = TRUE),]$region1 <- 'Clarin'
region.summary[grep("clarin", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'

region.summary[grep("Plaridel Mis", region.summary$region, ignore.case = TRUE),]$region1 <- 'Plaridel'
region.summary[grep("Plaridel Mis", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Plaridel, Mis", region.summary$region, ignore.case = TRUE),]$region1 <- 'Plaridel'
region.summary[grep("Plaridel, Mis", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Cartagena Proper", region.summary$region, ignore.case = TRUE),]$region1 <- 'Plaridel'
region.summary[grep("Cartagena Proper", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Panalsalan, Plaridel", region.summary$region, ignore.case = TRUE),]$region1 <- 'Plaridel'
region.summary[grep("Panalsalan, Plaridel", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Lao Proper", region.summary$region, ignore.case = TRUE),]$region1 <- 'Plaridel'
region.summary[grep("Lao Proper", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'

region.summary[grep("Plaridel-Polo Pitogo", region.summary$region, ignore.case = TRUE),]$region1 <- 'Plaridel - Bato'
region.summary[grep("Plaridel-Polo Pitogo", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Balanlinao", region.summary$region, ignore.case = TRUE),]$region1 <- 'Plaridel - Balanlinao'
region.summary[grep("Southern Pob.,", region.summary$region, ignore.case = TRUE),]$region1 <- 'Plaridel'
region.summary[grep("Southern Pob.,", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Mangidkid", region.summary$region, ignore.case = TRUE),]$region1 <- 'Plaridel-Mangidkid'
region.summary[grep("Mangidkid", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Mamanga Daku", region.summary$region, ignore.case = TRUE),]$region1 <- 'Plaridel-Mamanga Daku'
region.summary[grep("Mamanga Gamay", region.summary$region, ignore.case = TRUE),]$region1 <- 'Plaridel-Mamanga Gamay'
region.summary[grep("Mamanga", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Looc Proper", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Plaridel-Kauswagan", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Plaridel-Katipunan", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Eastern Looc", region.summary$region, ignore.case = TRUE),]$region1 <- 'Plaridel-Eastern Looc'
region.summary[grep("Eastern Looc", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Deboloc", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Plaridel-Danao", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Plaridel-Agunod", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'

region.summary[grep("Sinacaban", region.summary$region, ignore.case = TRUE),]$region1 <- 'Sinacaban'
region.summary[grep("Sinacaban", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'

region.summary[grep("Ozamis", region.summary$region, ignore.case = TRUE),]$region1 <- 'Ozamis City'
region.summary[grep("Ozamis", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'

region.summary[grep("Bonifacio", region.summary$region, ignore.case = TRUE),]$region1 <- 'Bonifacio'
region.summary[grep("Bonifacio", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'

region.summary[grep("Oroquieta City", region.summary$region, ignore.case = TRUE),]$region1 <- 'Oroquieta City'
region.summary[grep("Oroquieta City", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'

region.summary[grep("Aloran", region.summary$region, ignore.case = TRUE),]$region1 <- 'Aloran'
region.summary[grep("Aloran", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'

region.summary[grep("Lopez Jaena", region.summary$region, ignore.case = TRUE),]$region1 <- 'Lopez Jaena'
region.summary[grep("Lopez Jaena", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Puntod", region.summary$region, ignore.case = TRUE),]$region1 <- 'Lopez Jaena'
region.summary[grep("Puntod", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Eastern Pob", region.summary$region, ignore.case = TRUE),]$region1 <- 'Lopez Jaena'
region.summary[grep("Eastern Pob", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'


region.summary[grep("Jimenez", region.summary$region, ignore.case = TRUE),]$region1 <- 'Jimenez'
region.summary[grep("Jimenez", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Seti", region.summary$region, ignore.case = TRUE),]$region1 <- 'Jimenez'
region.summary[grep("Seti", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'

region.summary[grep("panaon", region.summary$region, ignore.case = TRUE),]$region1 <- 'Panaon'
region.summary[grep("panaon", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Pana-on,Mis", region.summary$region, ignore.case = TRUE),]$region1 <- 'Panaon'
region.summary[grep("Pana-on,Mis", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'
region.summary[grep("Pana-on Mis", region.summary$region, ignore.case = TRUE),]$region1 <- 'Panaon'
region.summary[grep("Pana-on Mis", region.summary$region, ignore.case = TRUE),]$region2 <- 'Misamis Occidental'


region.summary[grep("Clarin, Bohol", region.summary$region, ignore.case = TRUE),]$region2 <- 'Bohol'


region.summary[grep("sindangan", region.summary$region, ignore.case = TRUE),]$region1 <- 'Sindangan'
region.summary[grep("sindangan", region.summary$region, ignore.case = TRUE),]$region2 <- 'Zamboanga del Norte'


#n_distinct(region.combined$combined)
region.combined <- region.summary %>% 
  unite(combined, c('region1','region2'), sep = ', ', remove = FALSE)

## 

region.biweekly <- kiva.join %>% 
  group_by(region, date) %>% 
  summarise(n = n()) %>% 
  mutate(biweek = julian(date, as.Date('2014-01-01'))%/%14) %>% 
  left_join(region.combined[,1:2], by = 'region') %>% 
  group_by(combined, biweek) %>% 
  summarise(n = sum(n))


biweek.count <- region.biweekly %>% 
  group_by(combined) %>% 
  summarise(biweek = n_distinct(biweek))

ggplot(subset(biweek.count, biweek > 8), aes(biweek)) +
  geom_histogram(binwidth = 1, fill = "white", colour = "red") 

ggplot(na.omit(subset(region.biweekly, 
                      combined %in% subset(biweek.count, biweek > 26)$combined)), 
               aes(biweek, n, color = combined)) +
  geom_line(alpha = 2/5) +
  theme(legend.position = 'none')

## Modified Regions 
weekly.region <- spread(na.omit(subset(region.biweekly, 
                                       combined %in% subset(biweek.count, biweek > 26)$combined)),
                        key = combined, value = n, fill = 0)

# Modified Regions (%)
(dim(weekly.region)[2]-1) /n_distinct(region.biweekly$combined)
# Counting (%)
sum(subset(region.biweekly, 
           combined %in% subset(biweek.count, biweek > 26)$combined)$n) /
  sum(region.biweekly$n)


region.monthly <- kiva.join %>% 
  group_by(region, date) %>% 
  summarise(n = n()) %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  group_by(region, year, month) %>% 
  summarise(n = sum(n)) %>% 
  mutate(day = as.Date(paste(year, '-', month, '- 30'), '%Y - %m - %d')) %>% 
  left_join(region.combined[,1:2], by = 'region') %>% 
  group_by(combined, day) %>% 
  summarise(n = sum(n))

month.compose <- region.month %>% 
  group_by(region) %>% 
  summarise(month = n())

series.compose <- region.series %>% 
  group_by(region) %>% 
  summarise(week = n_distinct(week))

ggplot(na.omit(region.monthly), aes(day, n, color = combined)) +
  geom_line(alpha = 2/5) +
  theme(legend.position = 'none')



