rm(list = ls())
setwd('c:/users/juggl_000/Desktop/R Scripts') # set directory 

library(magrittr)
library(texreg)

library(tidycensus)
library(ggplot2)
library(stringr)

# a list of all the ACS 2015 variables is stored here. https://api.census.gov/data/2015/acs/acs5/subject/variables.html
vars <- data.frame(load_variables(2016, 'acs5')) # downloads a list of all variables from the 2016 ACS
head(vars) # there are a LOT of options

vars2 <- data.frame(load_variables(2010, 'sf1')) 
head(vars2)


race_vars <- vars[str_detect(vars$concept, 'RACE'), ] # what is the following code designed to do?
white_race_vars <- race_vars[str_detect(race_vars$concept, 'HISPANIC'), ]

race_vars2 <- vars2[str_detect(vars2$concept, 'RACE'), ] # what is the following code designed to do?
his_race_vars2 <- race_vars2[str_detect(race_vars2$concept, 'HISPANIC'), ]
head(white_race_vars)

# P005001 --> P005017 -- Race & ethnicity
# P007001 --> P007015 -- Total race tally? alternative way to look at data
# P015001 -- household by race (homeowners)


var_names <- NA

for (i in 1:17) {
  if (i <10){
    var_names[i] <- paste( 'P00500', i, sep = "")}
  else{var_names[i] <- paste( 'P0050', i, sep = "")}
}


dicensus_dat <- get_decennial(geography = "zcta", variables = var_names, geometry = FALSE, output = 'wide', shift_geo = FALSE, summary_var = 'P005001', state= 'Massachusetts')

dicensus_dat2 <- get_decennial(geography = "zcta", variables = 'P005001', geometry = FALSE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Massachusetts')

census_dat <- get_acs(geography = "State", variables = c('B03002_001',
                                                         'B03002_002',
                                                         'B03002_003',
                                                         'B03002_004',
                                                         'B03002_005',
                                                         'B03002_006',
                                                         'B03002_007',
                                                         'B03002_008',
                                                         'B03002_009',
                                                         'B03002_010',
                                                         'B03002_011',
                                                         'B03002_012',
                                                         'B03002_013',
                                                         'B03002_014',
                                                         'B03002_015',
                                                         'B03002_016',
                                                         'B03002_017',
                                                         'B03002_018',
                                                         'B03002_019',
                                                         'B03002_020',
                                                         'B03002_021'), geometry = FALSE, output = 'wide', shift_geo = FALSE, summary_var = 'S0101_C01_001') # can you make sense of what this code is doing? Either look at the head of census_dat or the help file for get_acs. Feel free to download other variables, just make sure you get the one about the proportion of people who are residents
head(census_dat) # which variable do you think we're going to use to link with the cces data?

census_dat %<>% dplyr::rename('countyfips' = 'GEOID', 'county' = 'NAME', 'foreign_total' = 'B05006_001E', 'recent_total' = 'B05007_002E',
                              'population' = 'summary_est')

m90 <- get_decennial(geography = "state", variables = "H043A001", year = 1990)
head(m90)


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

cen_dat <- getCensus(name = 'dec/sf1',
                     vintage = 2010,
                     region ="tract:*",
                     regionin = "state:25",
                     vars = c("NAME", "P001001","P011002" ))

cen_dat2010 <- getCensus(name = 'dec/sf1',
                         vintage = 2010,
                         region ="tract:*",
                         regionin = "state:25",
                         vars = c("NAME", var_names, "P004002", "P004003" ))
head(cen_dat2)

cen_dat2000 <- getCensus(name = 'dec/sf1',
                         vintage = 2010,
                         region ="zip code tabulation area:*",
                         #regionin = "state:25",
                         vars = c("NAME", "STATE", "COUNTY", 'TRACT',"P004002", "P004003" ))
head(cen_dat2)

top_surnames <- getCensus(name = "surname",
                          vintage = 2010,
                          vars = c("NAME", "COUNT", "PROP100K", "PCTWHITE", "PCTBLACK", "PCTAIAN", "PCTAPI", "PCTHISPANIC", "PCT2PRACE"),
                          RANK = "1:25", 
                          key = census_key)
head(top_surnames)
head(cen_dat)
