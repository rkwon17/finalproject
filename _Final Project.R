#### last updated: Rachel 4/23/19

rm(list = ls())
setwd('c:/users/juggl_000/Desktop/R Scripts') # set directory 

library(magrittr)
library(texreg)

library(tidycensus)
library(ggplot2)
library(stringr)
library(tidyverse)

#map libraries - need these to load shapefile
library(leaflet)
library(tigris)
library(acs)


#shapefile test
lookup_code(state="MA",county="Suffolk")
countylist <- c('17','25') #cambridge + boston (fips codes for suffolk + middlesex)
shapefile <- tracts(state='25', county=countylist) #cambridge and boston
plot(shapefile)

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


# Erin testing 2000 white-only data 
test_vars2000 <- c('P003003', 'P003004', 'P003001')
test_cendat2000 <- get_decennial(geography = "tract", variables = test_vars2000, year = 2000, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Massachusetts', county = '01')


m90 <- get_decennial(geography = "state", variables = "H043A001", year = 1990)
head(m90)

#P080A001 -- income?

library(censusapi)

#### New censusapi ####
#census_key <- 

apis <- listCensusApis()
View(apis)

# p011002  zip+code+tabulation+area:
#"P011002"-- 18 and over

cen_api_vars <- listCensusMetadata(name = "dec/sf1", vintage = 2010, 
                                   type = "variables")

geo<- listCensusMetadata(name = "dec/sf1", vintage = 2010,
                         type = "geography")


cen_dat2010 <- getCensus(name = 'dec/sf1',
                         vintage = 2010,
                         region ="tract:*",
                         regionin = "state:25",
                         vars = vars2010)


cen_dat2000 <- getCensus(name = 'sf1',
                         vintage = 2000,
                         region ="tract:*",
                         regionin = "state:25",
                         vars = vars2000)

#"state:36+county:027+tract:010000" 

cen_dat1990 <- getCensus(name = 'sf1',
                         vintage = 1990,
                         region ="county:*",
                         regionin = "state:25",
                         vars = 'P0010001')

