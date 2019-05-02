#### last updated: Kevin 4/30/19 @ 1pm

#### set up ####
rm(list = ls())
setwd('c:/users/juggl_000/Desktop/R Scripts') # set directory 

library(magrittr)
library(texreg)

library(tidycensus)
library(ggplot2)
library(ggmap)
library(stringr)
library(tidyverse)

#map libraries - need these to load shapefile
library(leaflet)
library(tigris)
library(acs)
library(gridExtra)


#### shapefile test - OLD ####
lookup_code(state="MA",county="Suffolk")
countylist <- c('17','25') #cambridge + boston (fips codes for suffolk + middlesex)
shapefile <- tracts(state='25', county=countylist) #cambridge and boston
plot(shapefile)

#### testing pulls - OLD ####

# a list of all the ACS 2015 variables is stored here. https://api.census.gov/data/2015/acs/acs5/subject/variables.html
vars <- data.frame(load_variables(2016, 'acs5')) # downloads a list of all variables from the 2016 ACS
head(vars) # there are a LOT of options

allvars2010 <- data.frame(load_variables(2010, 'sf1')) 
allvars2000 <- data.frame(load_variables(2000, 'sf1')) 
allvars1990 <- data.frame(load_variables(1990, 'sf1')) 

race_vars <- vars[str_detect(vars2000$concept, 'RACE'), ] # what is the following code designed to do?
white_race_vars <- race_vars[str_detect(race_vars$concept, 'HISPANIC'), ]

race_vars2 <- vars2[str_detect(vars2000$concept, 'RACE'), ] # what is the following code designed to do?
his_race_vars2 <- race_vars2[str_detect(race_vars2$concept, 'HISPANIC'), ]
head(white_race_vars)

# P007001 --> P007015 -- Total race tally? alternative way to look at data
# P015001 -- household by race (homeowners)

vars2010 <- NA

for (i in 1:17) {
  if (i <10){
    vars2010[i] <- paste( 'P00500', i, sep = "")}
  else{vars2010[i] <- paste( 'P0050', i, sep = "")}
}

vars2010 <- c(vars2010, "P004002", "P004003")

cendat2010 <- get_decennial(geography = "tract", variables = vars2010, year = 2010, geometry = FALSE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Massachusetts')

vars2000 <- c("P004001","P004002","P004003","P004004","P004005","P004006","P004007","P004008","P004009","P004010","P004011")

cendat2000 <- get_decennial(geography = "tract", variables = vars2000, year = 2000, geometry = FALSE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Massachusetts')

vars1990 <- c("P0100001","P0100002","P0100003","P0100004","P0100005","P0090001","P0080001")

county1990 <- c("01","03","05","07","09","11","13","15","17","19","21","23","25","27")

cendat1990a <- get_decennial(geography = "tract", variables = vars1990, year = 1990, geometry = FALSE, output = 'wide', shift_geo = FALSE, summary_var = 'P0010001', state= 'Massachusetts', county = '05')

cendat1990b <- get_decennial(geography = "tract", variables = vars1990, year = 1990, geometry = FALSE, output = 'wide', shift_geo = FALSE, summary_var = 'P0010001', state= 'Massachusetts', county = '03')

cendat1990<- rbind(cendat1990a,cendat1990b)

cendat1990 <- get_decennial(geography = "tract", variables = vars1990, year = 1990, geometry = FALSE, output = 'wide', shift_geo = FALSE, summary_var = 'P0010001', state= 'Massachusetts', county = '01')

for (i in 2:length(county1990)) {
  temp <- get_decennial(geography = "tract", variables = vars1990, year = 1990, geometry = FALSE, output = 'wide', shift_geo = FALSE, summary_var = 'P0010001', state= 'Massachusetts', county = county1990[i])
  
  cendat1990<- rbind(cendat1990, temp)
}

cendat1990[cendat1990$GEOID %in% cendat1990$GEOID[duplicated(cendat1990$GEOID)],]

cendat1990$test <- rowSums(cendat1990[,8:9])

census_dat %<>% dplyr::rename('countyfips' = 'GEOID', 'county' = 'NAME', 'foreign_total' = 'B05006_001E', 'recent_total' = 'B05007_002E',
                              'population' = 'summary_est')

#### mapping non-white percentage ####
countylist <- c('Suffolk')

vars1990 <- c('P0070001')
cendat1990 <- get_decennial(geography = "tract", variables = vars1990, year = 1990, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P0010001', state= 'Massachusetts', county = countylist)

vars2000 <- c('P003003', 'P003004', 'P003001')
cendat2000 <- get_decennial(geography = "tract", variables = vars2000, year = 2000, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Massachusetts', county = countylist)

vars2010 <- c('P010003','P001001') #white alone, population total
cendat2010 <- get_decennial(geography = "tract", variables = vars2010, year = 2010, geometry = TRUE, output = 'wide', shift_geo = FALSE, state= 'Massachusetts', county = countylist)

# Changing to Percentages 
cendat2010$pct_white_2010 <- round(cendat2010$P010003 / cendat2010$P001001, 3) * 100 
cendat2000$pct_white_2000 <- round(cendat2000$P003003 / cendat2000$summary_value, 3) * 100 
cendat1990$pct_white_1990 <- round(cendat1990$P0070001 / cendat1990$summary_value, 3) * 100 

# Leaflet prep
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
                                 transform: translate(-50%,20%);
                                 position: fixed !important;
                                 left: 50%;
                                 text-align: center;
                                 padding-left: 10px; 
                                 padding-right: 10px; 
                                 background: rgba(255,255,255,0.75);
                                 font-weight: bold;
                                 font-size: 28px;
                                 }
                                 "))

title1990 <- tags$div(
  tag.map.title, HTML("Racial Diversity: 1990 Census")) 
title2000 <- tags$div(
  tag.map.title, HTML("Racial Diversity: 2000 Census")) 
title2010 <- tags$div(
  tag.map.title, HTML("Racial Diversity: 2010 Census")) 

# MAP! 2010
pal2010 = colorNumeric(palette = "viridis", domain = cendat2010$pct_white_2010)
map2010 <- cendat2010 %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.5,
              color = ~ pal2010(pct_white_2010)) %>%
  addLegend("bottomright", 
            pal = pal2010, 
            values = ~ pct_white_2010,
            title = "Census Tract Pct White",
            opacity = 1) %>%
  addControl(title2010, position = "topleft", className = "map-title")

# MAP! 2000
pal2000 = colorNumeric(palette = "viridis", domain = cendat2000$pct_white_2000)
map2000 <- cendat2000 %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.5,
              color = ~ pal2000(pct_white_2000)) %>%
  addLegend("bottomright", 
            pal = pal2000, 
            values = ~ pct_white_2000,
            title = "Census Tract Pct White",
            opacity = 1) %>%
  addControl(title2000, position = "topleft", className = "map-title")

# MAP! 1990
pal1990 = colorNumeric(palette = "viridis", domain = cendat1990$pct_white_1990)
map1990 <- cendat1990 %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.5,
              color = ~ pal1990(pct_white_1990)) %>%
  addLegend("bottomright", 
            pal = pal1990, 
            values = ~ pct_white_1990,
            title = "Census Tract Pct White",
            opacity = 1) %>%
  addControl(title1990, position = "topleft", className = "map-title")

map1990
map2000
map2010

library(htmltools)
# view all leaflets together -- not sure why the titles don't all appear
leaflet_grid <- 
  tagList(
    tags$table(width = "100%",
               tags$tr(
                 tags$td(map1990),
                 tags$td(map2000)),
               tags$tr(
                 tags$td(map2010))))
browsable(leaflet_grid)


#P080A001 -- income?


#### join and compare percentage over time? ####

sub_1990 <- as.data.frame(test_cendat1990[, c('GEOID', 'pct_white1990')])
sub_1990$geometry <- NULL
sub_2010 <- as.data.frame(test_cendat2010[, c('GEOID', 'pct_white')])
compare_pct <- right_join(sub_2010,sub_1990, by= "GEOID")
compare_pct %<>% subset(!is.na(pct_white)) 


compare_pct$change <- compare_pct$pct_white - compare_pct$pct_white1990

ggplot(compare_pct, aes(fill = change, color = change)) +
  geom_sf()
coord_sf(crs = 26914)

# can't figure out why this is only creating one point
ggplot(compare_pct, aes(x = "pct_white", y = "change")) + geom_point()
