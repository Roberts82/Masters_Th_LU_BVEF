# Distance between countries (borders) as well as maps
# https://cran.r-project.org/web/packages/rnaturalearth/README.html
install.packages("devtools")
install.packages("usethis")
library(usethis)
library(devtools)

devtools::install_github("ropenscilabs/rnaturalearthdata")
install.packages("rnaturalearthhires",
                 repos = "http://packages.ropensci.org",
                 type = "source")

library(rnaturalearth)
library(sp)

#world countries
sp::plot(ne_countries())

#uk
sp::plot(ne_countries(country = 'united kingdom'))

#lv
sp::plot(ne_countries(country = 'latvia'))

#states, admin level1 boundaries
sp::plot(ne_states(country = 'spain'))
sp::plot(ne_states(country = 'latvia'))
sp::plot(ne_states(country = 'united kingdom'))
sp::plot(ne_states(country = 'barbados'))

# distances
install.packages("rnaturalearth")
library(rnaturalearth)

af =  ne_countries(continent = 'africa', returnclass="sf")

countries <- ne_countries(scale = 110, type = "countries", continent = NULL,
             country = NULL, geounit = NULL, sovereignty = NULL,
             returnclass = c("sp", "sf"))

spdf_world <- ne_countries()

af =  ne_countries(continent = 'africa', returnclass="sf")
af$admin[3]
d = st_distance(af[3,],af)
st_distance(af, af)
