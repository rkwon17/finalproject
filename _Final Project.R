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


#### shapefile test ####
lookup_code(state="MA",county="Suffolk")
countylist <- c('17','25') #cambridge + boston (fips codes for suffolk + middlesex)
shapefile <- tracts(state='25', county=countylist) #cambridge and boston
plot(shapefile)

#### testing pulls ####

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



#P080A001 -- income?


# Changing to Percentages 
test_cendat2010$pct_white_2010 <- round(test_cendat2010$P010003 / test_cendat2010$P001001, 3) * 100 
test_cendat2000$pct_white_2000 <- round(test_cendat2000$P003003 / test_cendat2000$summary_value, 3) * 100 
test_cendat1990$pct_white_1990 <- round(test_cendat1990$P0070001 / test_cendat1990$summary_value, 3) * 100 

# MAP! 2010
pal2010 = colorNumeric(palette = "viridis", domain = test_cendat2010$pct_white_2010)
test_cendat2010 %>%
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
            opacity = 1)

# MAP! 2000
pal2000 = colorNumeric(palette = "viridis", domain = test_cendat2000$pct_white_2000)
test_cendat2000 %>%
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
            opacity = 1)

# MAP! 1990
pal1990 = colorNumeric(palette = "viridis", domain = test_cendat1990$pct_white_1990)
test_cendat1990 %>%
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
            opacity = 1)





library(gridExtra)
grid.arrange(plot_1990, plot_2000, plot_2010)

# join and compare percentage over time? 

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
