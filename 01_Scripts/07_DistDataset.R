# https://gist.githubusercontent.com/mtriff/185e15be85b44547ed110e412a1771bf/raw/1bb4d287f79ca07f63d4c56110099c26e7c6ee7d/countries_distances.csv

library(dplyr)

CountrDist <- read.csv(url("https://gist.githubusercontent.com/mtriff/185e15be85b44547ed110e412a1771bf/raw/1bb4d287f79ca07f63d4c56110099c26e7c6ee7d/countries_distances.csv"))
CountrDist

write.csv(CountrDist, "CountrDist.csv") #save as csv file

LatvDist <- CountrDist %>%
  filter(pays1 == "Latvia")
  LatvDist$X <- NULL
LatvDist

write.csv(LatvDist, "LatvDist.csv") #save as csv file

#to allign the names of countries

setdiff(LatvDist$pays2, DataFDI_LV_DTT$country)
setdiff(DataFDI_LV_DTT$country, LatvDist$pays2)


LatvDistAllignNames <- LatvDist %>%
  mutate(pays2=replace(pays2, pays2=="Virgin Islands, US", "United States Virgin Islands")) %>%
  mutate(pays2=replace(pays2, pays2=="USA", "United States of America")) %>%
  mutate(pays2=replace(pays2, pays2=="Barbuda", "Antigua and Barbuda")) %>%
  mutate(pays2=replace(pays2, pays2=="UK", "United Kingdom of Great Britain and Northern Ireland")) %>%
  mutate(pays2=replace(pays2, pays2=="Virgin Islands, British", "British Virgin Islands")) %>%
  mutate(pays2=replace(pays2, pays2=="Czech Republic", "Czechia")) %>%
  mutate(pays2=replace(pays2, pays2=="Iran", "Iran (Islamic Republic of)")) %>%
  mutate(pays2=replace(pays2, pays2=="Cape Verde", "Cabo Verde")) %>%
  mutate(pays2=replace(pays2, pays2=="South Korea", "Republic of Korea")) %>%
  mutate(pays2=replace(pays2, pays2=="Kosovo", "Kosovo, Republic of")) %>%
  mutate(pays2=replace(pays2, pays2=="Ivory Coast", "Cote d'Ivoire")) %>%
  mutate(pays2=replace(pays2, pays2=="Russia", "Russian Federation")) %>%
  mutate(pays2=replace(pays2, pays2=="Laos", "Lao People's Democratic Republic")) %>%
  mutate(pays2=replace(pays2, pays2=="Moldova", "Republic of Moldova")) %>%
  mutate(pays2=replace(pays2, pays2=="Saint Kitts", "Saint Kitts and Nevis")) %>%
  mutate(pays2=replace(pays2, pays2=="Grenadines", "Saint Vincent and the Grenadines")) %>%
  mutate(pays2=replace(pays2, pays2=="Syria", "Syrian Arab Republic")) %>%
  mutate(pays2=replace(pays2, pays2=="Tanzania", "United Republic of Tanzania")) %>%
  mutate(pays2=replace(pays2, pays2=="Venezuela", "Venezuela (Bolivarian Republic of)")) %>%
  mutate(pays2=replace(pays2, pays2=="Vietnam", "Viet Nam")) %>%
  mutate(pays2=replace(pays2, pays2=="Macedonia", "The former Yugoslav Republic of Macedonia"))
LatvDistAllignNames

setdiff(DataFDI_LV_DTT$country, LatvDistAllignNames$pays2)

write.csv(LatvDistAllignNames, "LatvDistAllignNames.csv") #save as csv file

setdiff(DataFDI_LV_DTT$country, LatvDistAllignNames$pays2)

# konstatējam, ja iztrūkst informācijas par 
# 2 teritorijām, kas mums ir nepieciešamas:
# 1) "Gibraltar"
# 2) "China, Hong Kong Special Administrative Region"       

# atrodam distanci no Latvijas līdz šīm teritorijām, izmantojot GoogleMaps funkciju
# rezultāti:
# https://www.google.com/maps/@39.8374144,-1.9613374,6.91z
# Latvija - Gibraltāra - 3000 km

# https://www.google.com/maps/@46.8065123,64.2952478,4.1z
# Latvija - Hongkonga - 7000 km

#pārbaudām datu tipu tabulā, lai varētu tai pievienot vērtības par 2 teritorijām
sapply(LatvDistAllignNames, typeof)

# izveidojam tabulu ar divu līdz šim iztrūkstošo teritoriju attālumu no Latvijas
pays1 <- c("Latvia", "Latvia")
pays2 <- c("Gibraltar", "China, Hong Kong Special Administrative Region")
dist <- c(3000.00, 7000.00)
GbHk2values <- data.frame(pays1, pays2, dist)

# pievinojam tabulu ar 2 teritorijām tabulai ar attālumiem no pārējām valstīm.
LatvDistAllignNames <- rbind(LatvDistAllignNames, GbHk2values)
LatvDistAllignNames$pays1 <- NULL
LatvDistAllignNames

write.csv(LatvDistAllignNames, "LatvDistAllignNames.csv") #save as csv file

# nākamajā solī pie vērtības tiks pievinotas pie galvenās datukopas