#### last update ####
# Rachel 5/7 12:30 PM

#### set up ####
rm(list = ls())
setwd('c:/users/juggl_000/Desktop/R Scripts') # set directory 

library(magrittr)
library(texreg)
library(dplyr)
library(tidycensus)
library(ggplot2)
library(ggmap)
library(stringr)
library(tidyverse)
library(leaflet)
library(htmltools)
library(tigris)
library(acs)
library(gridExtra)
library(sf)


#### prep for all maps ####
vars1990 <- c('P0070001') #all white
vars2000 <- c('P003003') #all white
vars2010 <- c('P003002') #all white
pal_all = colorNumeric(palette = "viridis", domain = c(0, 100))
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

#### BOSTON - Pulls and Cleaning ####
countylist_MA <- c('Suffolk', 'Norfolk', 'Middlesex')

cendat_MA_2010 <- get_decennial(geography = "tract", variables = vars2010, year = 2010, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Massachusetts', county = countylist_MA)

cendat_MA_2000 <- get_decennial(geography = "tract", variables = vars2000, year = 2000, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Massachusetts', county = countylist_MA)

cendat_MA_1990 <- get_decennial(geography = "tract", variables = vars1990, year = 1990, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P0010001', state= 'Massachusetts', county = countylist_MA)

# Changing to %
cendat_MA_2010$pct_white <- round(cendat_MA_2010$P003002 / cendat_MA_2010$summary_value, 3) * 100 
cendat_MA_2000$pct_white <- round(cendat_MA_2000$P003003 / cendat_MA_2000$summary_value, 3) * 100 
cendat_MA_1990$pct_white <- round(cendat_MA_1990$P0070001 / cendat_MA_1990$summary_value, 3) * 100 

# Remove NAs from datasets
cendat_MA_2010 %<>% subset(!is.na(pct_white))
cendat_MA_2000 %<>% subset(!is.na(pct_white))
cendat_MA_1990 %<>% subset(!is.na(pct_white))

#### BOSTON - Maps ####
# MAP! 2010
map_MA_2010 <- cendat_MA_2010 %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.5,
              color = ~ pal_all(pct_white),
              label = ~paste0(pct_white, "%, (", P003002, ")")) %>%
  addLegend("bottomright", 
            pal = pal_all, 
            values = ~ pct_white,
            title = "Census Tract Pct White",
            opacity = 1) %>%
  addControl(title2010, position = "topleft", className = "map-title")

# MAP! 2000
map_MA_2000 <- cendat_MA_2000 %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.5,
              color = ~ pal_all(pct_white),
              label = ~paste0(pct_white, "%, (", P003003, ")"))%>%
  addLegend("bottomright", 
            pal = pal_all, 
            values = ~ pct_white,
            title = "Census Tract Pct White",
            opacity = 1) %>%
  addControl(title2000, position = "topleft", className = "map-title")

# MAP! 1990
map_MA_1990 <- cendat_MA_1990 %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.5,
              color = ~ pal_all(pct_white),
              label = ~paste0(pct_white, "%, (", P0070001, ")")) %>%
  addLegend("bottomright", 
            pal = pal_all, 
            values = ~ pct_white,
            title = "Census Tract Pct White",
            opacity = 1) %>%
  addControl(title1990, position = "topleft", className = "map-title")

# view maps
map_MA_1990
map_MA_2000
map_MA_2010

# view all leaflets together -- not sure why the titles don't all appear. ugh.
leaflet_grid <- 
  tagList(
    tags$table(width = "100%",
               tags$tr(
                 tags$td(map_MA_1990),
                 tags$td(map_MA_2000)),
               tags$tr(
                 tags$td(map_MA_2010))))
browsable(leaflet_grid)


#### DC - Pulls and Cleaning ####
# pull data (no county necessary)
cendat_DC_2010 <- get_decennial(geography = "tract", variables = vars2010, year = 2010, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'District of Columbia')

cendat_DC_2000 <- get_decennial(geography = "tract", variables = vars2000, year = 2000, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'District of Columbia')

cendat_DC_1990 <- get_decennial(geography = "tract", variables = vars1990, year = 1990, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P0010001', state= 'District of Columbia')

# Change to %s
cendat_DC_2010$pct_white <- round(cendat_DC_2010$P003002 / cendat_DC_2010$summary_value, 3) * 100 
cendat_DC_2000$pct_white <- round(cendat_DC_2000$P003003 / cendat_DC_2000$summary_value, 3) * 100 
cendat_DC_1990$pct_white <- round(cendat_DC_1990$P0070001 / cendat_DC_1990$summary_value, 3) * 100 

cendat_DC_2010 %<>% subset(!is.na(pct_white))
cendat_DC_2000 %<>% subset(!is.na(pct_white))
cendat_DC_1990 %<>% subset(!is.na(pct_white))

#### DC - Maps ####
# Map 2010
map_DC_2010 <- cendat_DC_2010 %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.5,
              color = ~ pal_all(pct_white),
              label = ~paste0(pct_white, "%, (", P003002, ")")) %>%
  addLegend("bottomright", 
            pal = pal_all, 
            values = ~ pct_white,
            title = "Census Tract Pct White",
            opacity = 1) %>%
  addControl(title2010, position = "topleft", className = "map-title")

# Map 2000
map_DC_2000 <- cendat_DC_2000 %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.5,
              color = ~ pal_all(pct_white),
              label = ~paste0(pct_white, "%, (", P003003, ")")) %>%
  addLegend("bottomright", 
            pal = pal_all, 
            values = ~ pct_white,
            title = "Census Tract Pct White",
            opacity = 1) %>%
  addControl(title2000, position = "topleft", className = "map-title")

# Map 1990
map_DC_1990 <- cendat_DC_1990 %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.5,
              color = ~ pal_all(pct_white),
              label = ~paste0(pct_white, "%, (", P0070001, ")")) %>%
  addLegend("bottomright", 
            pal = pal_all, 
            values = ~ pct_white,
            title = "Census Tract Pct White",
            opacity = 1) %>%
  addControl(title1990, position = "topleft", className = "map-title")

map_DC_2010
map_DC_2000
map_DC_1990

#### OAKLAND - Pulls and cleaning ####
countylist_CA <- c('Alameda', 'Contra Costa', 'San Francisco', 'San Mateo', 'Marin', 'Santa Clara')

cendat_CA_2010 <- get_decennial(geography = "tract", variables = vars2010, year = 2010, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'California', county = countylist_CA)

cendat_CA_2000 <- get_decennial(geography = "tract", variables = vars2000, year = 2000, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'California', county = countylist_CA)

cendat_CA_1990 <- get_decennial(geography = "tract", variables = vars1990, year = 1990, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P0010001', state= 'California', county = countylist_CA)

# Changing to %
cendat_CA_2010$pct_white <- round(cendat_CA_2010$P003002 / cendat_CA_2010$summary_value, 3) * 100 
cendat_CA_2000$pct_white <- round(cendat_CA_2000$P003003 / cendat_CA_2000$summary_value, 3) * 100 
cendat_CA_1990$pct_white <- round(cendat_CA_1990$P0070001 / cendat_CA_1990$summary_value, 3) * 100 

cendat_CA_2010 %<>% subset(!is.na(pct_white))
cendat_CA_2000 %<>% subset(!is.na(pct_white))
cendat_CA_1990 %<>% subset(!is.na(pct_white))

#### OAKLAND - Maps ####
map_CA_2010 <- cendat_CA_2010 %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.5,
              color = ~ pal_all(pct_white),
              label = ~paste0(pct_white, "%, (", P003002, ")")) %>%
  addLegend("bottomright", 
            pal = pal_all, 
            values = ~ pct_white,
            title = "Census Tract Pct White",
            opacity = 1) %>%
  addControl(title2010, position = "topleft", className = "map-title")


#### PORTLAND - Pulls and cleaning ####
countylist_OR <- c('Columbia', 'Multnomah', 'Washington', 'Clackamas', 'Hood River')

cendat_OR_2010 <- get_decennial(geography = "tract", variables = vars2010, year = 2010, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Oregon', county = countylist_OR)

cendat_OR_2000 <- get_decennial(geography = "tract", variables = vars2000, year = 2000, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Oregon', county = countylist_OR)

cendat_OR_1990 <- get_decennial(geography = "tract", variables = vars1990, year = 1990, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P0010001', state= 'Oregon', county = countylist_OR)

# Changing to %
cendat_OR_2010$pct_white <- round(cendat_OR_2010$P003002 / cendat_OR_2010$summary_value, 3) * 100 
cendat_OR_2000$pct_white <- round(cendat_OR_2000$P003003 / cendat_OR_2000$summary_value, 3) * 100 
cendat_OR_1990$pct_white <- round(cendat_OR_1990$P0070001 / cendat_OR_1990$summary_value, 3) * 100 

cendat_OR_2010 %<>% subset(!is.na(pct_white))
cendat_OR_2000 %<>% subset(!is.na(pct_white))
cendat_OR_1990 %<>% subset(!is.na(pct_white))

#### PORTLAND - Maps ####
map_OR_2010 <- cendat_OR_2010 %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.5,
              color = ~ pal_all(pct_white),
              label = ~paste0(pct_white, "%, (", P003002, ")")) %>%
  addLegend("bottomright", 
            pal = pal_all, 
            values = ~ pct_white,
            title = "Census Tract Pct White",
            opacity = 1) %>%
  addControl(title2010, position = "topleft", className = "map-title")

#### KNOXVILLE - Pulls and cleaning ####
countylist_TN <- c('Knox', 'Anderson', 'Sevier', 'Blount')

cendat_TN_2010 <- get_decennial(geography = "tract", variables = vars2010, year = 2010, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Tennessee', county = countylist_TN)

cendat_TN_2000 <- get_decennial(geography = "tract", variables = vars2000, year = 2000, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Tennessee', county = countylist_TN)

cendat_TN_1990 <- get_decennial(geography = "tract", variables = vars1990, year = 1990, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P0010001', state= 'Tennessee', county = countylist_TN)

# Changing to %
cendat_TN_2010$pct_white <- round(cendat_TN_2010$P003002 / cendat_TN_2010$summary_value, 3) * 100 
cendat_TN_2000$pct_white <- round(cendat_TN_2000$P003003 / cendat_TN_2000$summary_value, 3) * 100 
cendat_TN_1990$pct_white <- round(cendat_TN_1990$P0070001 / cendat_TN_1990$summary_value, 3) * 100 

cendat_TN_2010 %<>% subset(!is.na(pct_white))
cendat_TN_2000 %<>% subset(!is.na(pct_white))
cendat_TN_1990 %<>% subset(!is.na(pct_white))

#### KNOXVILLE - Maps ####
map_TN_2010 <- cendat_TN_2010 %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.5,
              color = ~ pal_all(pct_white),
              label = ~paste0(pct_white, "%, (", P003002, ")")) %>%
  addLegend("bottomright", 
            pal = pal_all, 
            values = ~ pct_white,
            title = "Census Tract Pct White",
            opacity = 1) %>%
  addControl(title2010, position = "topleft", className = "map-title")

#### SEATTLE - Pulls and Cleaning ####
countylist_WA <- c('King', 'Snohomish', 'Kitsap', 'Island')

cendat_WA_2010 <- get_decennial(geography = "tract", variables = vars2010, year = 2010, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Washington', county = countylist_WA)

cendat_WA_2000 <- get_decennial(geography = "tract", variables = vars2000, year = 2000, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Washington', county = countylist_WA)

cendat_WA_1990 <- get_decennial(geography = "tract", variables = vars1990, year = 1990, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P0010001', state= 'Washington', county = countylist_WA)

# Changing to %
cendat_WA_2010$pct_white <- round(cendat_WA_2010$P003002 / cendat_WA_2010$summary_value, 3) * 100 
cendat_WA_2000$pct_white <- round(cendat_WA_2000$P003003 / cendat_WA_2000$summary_value, 3) * 100 
cendat_WA_1990$pct_white <- round(cendat_WA_1990$P0070001 / cendat_WA_1990$summary_value, 3) * 100 

cendat_WA_2010 %<>% subset(!is.na(pct_white))
cendat_WA_2000 %<>% subset(!is.na(pct_white))
cendat_WA_1990 %<>% subset(!is.na(pct_white))

#### SEATTLE - Maps ####
map_WA_2010 <- cendat_WA_2010 %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.5,
              color = ~ pal_all(pct_white),
              label = ~paste0(pct_white, "%, (", P003002, ")")) %>%
  addLegend("bottomright", 
            pal = pal_all, 
            values = ~ pct_white,
            title = "Census Tract Pct White",
            opacity = 1) %>%
  addControl(title2010, position = "topleft", className = "map-title")

#### DETROIT - Pulls and Cleaning ####
countylist_MI <- c('Oakland', 'Macomb', 'Wayne')

cendat_MI_2010 <- get_decennial(geography = "tract", variables = vars2010, year = 2010, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Michigan', county = countylist_MI)

cendat_MI_2000 <- get_decennial(geography = "tract", variables = vars2000, year = 2000, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Michigan', county = countylist_MI)

cendat_MI_1990 <- get_decennial(geography = "tract", variables = vars1990, year = 1990, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P0010001', state= 'Michigan', county = countylist_MI)

# Changing to %
cendat_MI_2010$pct_white <- round(cendat_MI_2010$P003002 / cendat_MI_2010$summary_value, 3) * 100 
cendat_MI_2000$pct_white <- round(cendat_MI_2000$P003003 / cendat_MI_2000$summary_value, 3) * 100 
cendat_MI_1990$pct_white <- round(cendat_MI_1990$P0070001 / cendat_MI_1990$summary_value, 3) * 100 

cendat_MI_2010 %<>% subset(!is.na(pct_white))
cendat_MI_2000 %<>% subset(!is.na(pct_white))
cendat_MI_1990 %<>% subset(!is.na(pct_white))

#### DETROIT - Maps ####
map_MI_2010 <- cendat_MI_2010 %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.5,
              color = ~ pal_all(pct_white),
              label = ~paste0(pct_white, "%, (", P003002, ")")) %>%
  addLegend("bottomright", 
            pal = pal_all, 
            values = ~ pct_white,
            title = "Census Tract Pct White",
            opacity = 1) %>%
  addControl(title2010, position = "topleft", className = "map-title")

#### join and compare percentage over time? ####

sub_1990 <- as.data.frame(cendat1990[, c('GEOID', 'pct_white')])
sub_1990$geometry <- NULL
sub_2010 <- as.data.frame(cendat2010[, c('GEOID', 'pct_white')])
compare_pct <- right_join(sub_2010,sub_1990, by= "GEOID")
compare_pct %<>% subset(!is.na(pct_white)) 


compare_pct$change <- compare_pct$pct_white - compare_pct$pct_white

ggplot(compare_pct, aes(fill = pct_white, color = pct_white)) +
  geom_sf()
coord_sf(crs = 26914)

# can't figure out why this is only creating one point
ggplot(compare_pct, aes(x = GEOID, y = change)) + geom_bar()

#### save data ####
#cendats <- list('cendat_MA_1990', 'cendat_MA_2000', 'cendat_MA_2010', 'cendat_DC_1990', 'cendat_DC_2000', 'cendat_DC_2010', 'cendat_CA_1990', 'cendat_CA_2000', 'cendat_CA_2010', 'cendat_OR_1990', 'cendat_OR_2000', 'cendat_OR_2010', 'cendat_WA_1990', 'cendat_WA_2000', 'cendat_WA_2010', 'cendat_TN_1990', 'cendat_TN_2000', 'cendat_TN_2010', 'cendat_MI_1990', 'cendat_MI_2000', 'cendat_MI_2010')

save(cendat_MA_1990, cendat_MA_2000, cendat_MA_2010, cendat_DC_1990, cendat_DC_2000, cendat_DC_2010, cendat_CA_1990, cendat_CA_2000, cendat_CA_2010, cendat_OR_1990, cendat_OR_2000, cendat_OR_2010, cendat_WA_1990, cendat_WA_2000, cendat_WA_2010, cendat_TN_1990, cendat_TN_2000, cendat_TN_2010, cendat_MI_1990, cendat_MI_2000, cendat_MI_2010, file = 'C:/Users/Erin/Documents/GitHub/finalproject/SavedData.Rdata')


###----------rachel experimenting with permutation------------
#read csv files for year csv files
setwd("~/Desktop")
year2000 <- read.csv("/Users/rachelkwon/Desktop/year2000.csv")
year1990 <- read.csv("/Users/rachelkwon/Desktop/year1990.csv")
year2010 <- read.csv("/Users/rachelkwon/Desktop/year2010.csv")
dim(year1990)
dim(year2000)
dim(year2010)
dim(cendat_MA_2010)
dim(cendat_MA_1990)
dim(cendat_MA_2000)

#merge year dataframes to cendat data 
###------convert everything to dataframes for permutation tests-----------
year2000 <- as.data.frame(year2000)
year1990 <- as.data.frame(year1990)
year2010 <- as.data.frame(year2010)
cendat_MA_1990 <- as.data.frame(cendat_MA_1990)
cendat_MA_2000 <- as.data.frame(cendat_MA_2000)
cendat_MA_2010 <- as.data.frame(cendat_MA_2010)
testing_cendat_MA_2000 <- cbind(cendat_MA_2000,year2000)
testing_cendat_MA_1990 <- cbind(cendat_MA_1990,year1990)
testing_cendat_MA_2010 <- cbind(cendat_MA_2010,year2010)
#names(testing_cendat_MA_1990) %<>% str_replace_all('P0070001', 'whiteonly')
#names(testing_cendat_MA_2000) %<>% str_replace_all('P003003', 'whiteonly')

#subsetting dataframes for permutation testing-----------------------------------
sub_testing_cendat_MA_2000 <- as.data.frame(testing_cendat_MA_2000[,c('NAME','GEOID','year','pct_white')])
sub_testing_cendat_MA_2000 <- subset(sub_testing_cendat_MA_2000,select=c(NAME,GEOID,year,pct_white))
sub_testing_cendat_MA_1990 <- as.data.frame(testing_cendat_MA_1990[,c('NAME','GEOID','year','pct_white')])
sub_testing_cendat_MA_1990 <- subset(sub_testing_cendat_MA_1990,select=c(NAME,GEOID,year,pct_white))

sub_testing_cendat_MA_2010 <- as.data.frame(testing_cendat_MA_2010[,c('NAME','GEOID','year','pct_white')])
sub_testing_cendat_MA_2010 <- subset(sub_testing_cendat_MA_2010,select=c(NAME,GEOID,year,pct_white))
summary(sub_testing_cendat_MA_1990)
summary(sub_testing_cendat_MA_2000)
summary(sub_testing_cendat_MA_2010) #5NA's

#sub_testing_cendat_MA_2000$geometry <- NULL
#sub_testing_cendat_MA_1990$geometry <- NULL


###-----prepping for 3 permutations-----------------------------------------------------------
#merged 1990 and 2000 by year
testing_MA_1990_2000 <- rbind(sub_testing_cendat_MA_1990,sub_testing_cendat_MA_2000)
testing_MA_1990_2000$year %<>% factor(levels = c(1990, 2000), labels = c('1990', '2000'))
table(testing_MA_1990_2000$year)

#merged 2000 and 2010 by year
testing_MA_2000_2010 <- rbind(sub_testing_cendat_MA_2000,sub_testing_cendat_MA_2010)
testing_MA_2000_2010$year %<>% factor(levels = c(2000, 2010), labels = c('2000', '2010'))

#merged 1990 and 2010 by year
testing_MA_1990_2010 <- rbind(sub_testing_cendat_MA_1990,sub_testing_cendat_MA_2010)
testing_MA_1990_2010$year %<>% factor(levels = c(1990, 2010), labels = c('1990', '2010'))

#actual permutation starts now
####-for loop for 1990-2000 ------------------------------------------------------------------------
set.seed(1)
#testing
#resample year
testing_MA_1990_2000$year_permuted <- sample(testing_MA_1990_2000$year)
testing_MA_1990_2000[c('year', 'year_permuted')]
#ttest
t.test(pct_white ~ year_permuted, testing_MA_1990_2000)
R <- 10000
mean_diffs <- rep(NA, R)
#for loop for permuting
for(i in 1:R){
  testing_MA_1990_2000$year_permuted <- sample(testing_MA_1990_2000$year_permuted)
  means <- tapply(testing_MA_1990_2000$pct_white, testing_MA_1990_2000$year_permuted, mean,na.rm=TRUE)
  mean_diffs[i] <- means['2000'] - means['1990']
  if(i %% 1000 == 0) print(i)
}
means <- tapply(testing_MA_1990_2000$pct_white, testing_MA_1990_2000$year, mean,na.rm=TRUE)
observed_mean <- means['2000'] - means['1990']
mean_diffs <- data.frame(mean_diffs = mean_diffs)
#distibution of mean diffs 
plot1 <- ggplot(mean_diffs, aes(x = mean_diffs)) + geom_histogram() + geom_vline(xintercept = observed_mean, col = 'red', lty = 2) + ggtitle("Distribution of permuted mean differences: \nBoston 1990 & 2000 census")
plot1
densities <- density(mean_diffs$mean_diffs)
plot_dat <- data.frame(x = densities$x, y = densities$y)
#density plot
ggplot(plot_dat, aes(x = x, y = y)) + geom_line() + geom_area(data = subset(plot_dat, x >= observed_mean), fill = 'salmon2', alpha = .5) + 
  geom_vline(xintercept = observed_mean, col = 'red', lty = 2) 

#symmetric plot
ggplot(plot_dat, aes(x = x, y = y)) + geom_line() + geom_area(data = subset(plot_dat, x >= observed_mean), fill = 'salmon2', alpha = .5) +
  geom_area(data = subset(plot_dat, x <= -observed_mean), fill = 'salmon2', alpha = .5) +
  geom_vline(xintercept = observed_mean, col = 'red', lty = 2)

###---permutation for 2000-2010-------------------------------------------------------
set.seed(1)
testing_MA_2000_2010$year_permuted <- sample(testing_MA_2000_2010$year)
testing_MA_2000_2010[c('year', 'year_permuted')]
#ttest
t.test(pct_white ~ year_permuted, testing_MA_2000_2010)
R <- 10000
mean_diffs <- rep(NA, R)
#for loop for permuting
for(i in 1:R){
  testing_MA_2000_2010$year_permuted <- sample(testing_MA_2000_2010$year_permuted)
  means <- tapply(testing_MA_2000_2010$pct_white, testing_MA_2000_2010$year_permuted, mean,na.rm=TRUE)
  mean_diffs[i] <- means['2010'] - means['2000']
  if(i %% 1000 == 0) print(i)
}
means <- tapply(testing_MA_2000_2010$pct_white, testing_MA_2000_2010$year, mean,na.rm=TRUE)
observed_mean <- means['2010'] - means['2000']
mean_diffs <- data.frame(mean_diffs = mean_diffs)
#this graph is suspect - look at it later
plot2 <- ggplot(mean_diffs, aes(x = mean_diffs)) + geom_histogram() + geom_vline(xintercept = observed_mean, col = 'red', lty = 2) + ggtitle("Distribution of permuted mean differences: \nBoston 2000 & 2010 census")
plot2
densities <- density(mean_diffs$mean_diffs)
plot_dat <- data.frame(x = densities$x, y = densities$y)
#density plot
ggplot(plot_dat, aes(x = x, y = y)) + geom_line() + geom_area(data = subset(plot_dat, x >= observed_mean), fill = 'salmon2', alpha = .5) + 
  geom_vline(xintercept = observed_mean, col = 'red', lty = 2) 

#symmetric plot
ggplot(plot_dat, aes(x = x, y = y)) + geom_line() + geom_area(data = subset(plot_dat, x >= observed_mean), fill = 'salmon2', alpha = .5) +
  geom_area(data = subset(plot_dat, x <= -observed_mean), fill = 'salmon2', alpha = .5) +
  geom_vline(xintercept = observed_mean, col = 'red', lty = 2)



###---permutation for 1990-2010-------------------------------------------------------
set.seed(1)
testing_MA_1990_2010$year_permuted <- sample(testing_MA_1990_2010$year)
testing_MA_1990_2010[c('year', 'year_permuted')]
#ttest
t.test(pct_white ~ year_permuted, testing_MA_1990_2010)
R <- 10000
mean_diffs <- rep(NA, R)
#for loop for permuting
for(i in 1:R){
  testing_MA_1990_2010$year_permuted <- sample(testing_MA_1990_2010$year_permuted)
  means <- tapply(testing_MA_1990_2010$pct_white, testing_MA_1990_2010$year_permuted, mean,na.rm=TRUE)
  mean_diffs[i] <- means['2010'] - means['1990']
  if(i %% 1000 == 0) print(i)
}
means <- tapply(testing_MA_1990_2010$pct_white, testing_MA_1990_2010$year, mean,na.rm=TRUE)
observed_mean <- means['2010'] - means['1990']
mean_diffs <- data.frame(mean_diffs = mean_diffs)
#plot
plot3 <- ggplot(mean_diffs, aes(x = mean_diffs)) + geom_histogram() + geom_vline(xintercept = observed_mean, col = 'red', lty = 2) + ggtitle("Distribution of permuted mean differences: \nBoston 1990 & 2010 census")
plot3

densities <- density(mean_diffs$mean_diffs)
plot_dat <- data.frame(x = densities$x, y = densities$y)
#density plot
ggplot(plot_dat, aes(x = x, y = y)) + geom_line() + geom_area(data = subset(plot_dat, x >= observed_mean), fill = 'salmon2', alpha = .5) + 
  geom_vline(xintercept = observed_mean, col = 'red', lty = 2) 

#symmetric plot
ggplot(plot_dat, aes(x = x, y = y)) + geom_line() + geom_area(data = subset(plot_dat, x >= observed_mean), fill = 'salmon2', alpha = .5) +
  geom_area(data = subset(plot_dat, x <= -observed_mean), fill = 'salmon2', alpha = .5) +
  geom_vline(xintercept = observed_mean, col = 'red', lty = 2)


#### OLD/ARCHIVE testing pulls - can these be deleted? ####
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

vars2010 <- NA

for (i in 1:17) {
  if (i <10){
    vars2010[i] <- paste( 'P00500', i, sep = "")}
  else{vars2010[i] <- paste( 'P0050', i, sep = "")}
}

vars2010 <- c(vars2010, "P004002", "P004003")

cendat2010 <- get_decennial(geography = "tract", variables = vars2010, year = 2010, geometry = FALSE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Massachusetts')

vars2000 <- c("P004001","P004002","P004003","P004004","P003003","P004006","P004007","P004008","P004009","P004010","P004011")

cendat2000 <- get_decennial(geography = "tract", variables = vars2000, year = 2000, geometry = FALSE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Massachusetts')

vars1990 <- c("P0070001","P0100002","P0100003","P0100004","P0100005","P0090001","P0080001")

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

