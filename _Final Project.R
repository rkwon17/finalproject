#### documentation ####
# Erin Mahoney, Rachel Kwon, Kevin Nazario
# S022, Spring 2019
# Final Project
# Topic: Racial Population Distribution in US Cities
# Note: See related RMD file to compile and run dashboard. 

#### set up ####
rm(list = ls())

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


#### set variables ####
vars1990 <- c('P0070001') #all white population
vars2000 <- c('P003003') #all white population
vars2010 <- c('P003002') #all white population

#### BOSTON - Pulls and Cleaning ####
# set counties and pull data from census
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

#### DC - Pulls and Cleaning ####
# pull data (no county necessary)
cendat_DC_2010 <- get_decennial(geography = "tract", variables = vars2010, year = 2010, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'District of Columbia')

cendat_DC_2000 <- get_decennial(geography = "tract", variables = vars2000, year = 2000, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'District of Columbia')

cendat_DC_1990 <- get_decennial(geography = "tract", variables = vars1990, year = 1990, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P0010001', state= 'District of Columbia')

# Change to %s
cendat_DC_2010$pct_white <- round(cendat_DC_2010$P003002 / cendat_DC_2010$summary_value, 3) * 100 
cendat_DC_2000$pct_white <- round(cendat_DC_2000$P003003 / cendat_DC_2000$summary_value, 3) * 100 
cendat_DC_1990$pct_white <- round(cendat_DC_1990$P0070001 / cendat_DC_1990$summary_value, 3) * 100 

# eliminate NAs
cendat_DC_2010 %<>% subset(!is.na(pct_white))
cendat_DC_2000 %<>% subset(!is.na(pct_white))
cendat_DC_1990 %<>% subset(!is.na(pct_white))

#### OAKLAND - Pulls and cleaning ####
# set counties and pull data from census
countylist_CA <- c('Alameda', 'Contra Costa', 'San Francisco', 'San Mateo', 'Marin', 'Santa Clara')

cendat_CA_2010 <- get_decennial(geography = "tract", variables = vars2010, year = 2010, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'California', county = countylist_CA)

cendat_CA_2000 <- get_decennial(geography = "tract", variables = vars2000, year = 2000, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'California', county = countylist_CA)

cendat_CA_1990 <- get_decennial(geography = "tract", variables = vars1990, year = 1990, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P0010001', state= 'California', county = countylist_CA)

# Changing to %
cendat_CA_2010$pct_white <- round(cendat_CA_2010$P003002 / cendat_CA_2010$summary_value, 3) * 100 
cendat_CA_2000$pct_white <- round(cendat_CA_2000$P003003 / cendat_CA_2000$summary_value, 3) * 100 
cendat_CA_1990$pct_white <- round(cendat_CA_1990$P0070001 / cendat_CA_1990$summary_value, 3) * 100 

# eliminate NAs
cendat_CA_2010 %<>% subset(!is.na(pct_white))
cendat_CA_2000 %<>% subset(!is.na(pct_white))
cendat_CA_1990 %<>% subset(!is.na(pct_white))

#### PORTLAND - Pulls and cleaning ####
# set counties and pull data from census
countylist_OR <- c('Columbia', 'Multnomah', 'Washington', 'Clackamas', 'Hood River')

cendat_OR_2010 <- get_decennial(geography = "tract", variables = vars2010, year = 2010, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Oregon', county = countylist_OR)

cendat_OR_2000 <- get_decennial(geography = "tract", variables = vars2000, year = 2000, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Oregon', county = countylist_OR)

cendat_OR_1990 <- get_decennial(geography = "tract", variables = vars1990, year = 1990, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P0010001', state= 'Oregon', county = countylist_OR)

# Changing to %
cendat_OR_2010$pct_white <- round(cendat_OR_2010$P003002 / cendat_OR_2010$summary_value, 3) * 100 
cendat_OR_2000$pct_white <- round(cendat_OR_2000$P003003 / cendat_OR_2000$summary_value, 3) * 100 
cendat_OR_1990$pct_white <- round(cendat_OR_1990$P0070001 / cendat_OR_1990$summary_value, 3) * 100 

# eliminate NAs
cendat_OR_2010 %<>% subset(!is.na(pct_white))
cendat_OR_2000 %<>% subset(!is.na(pct_white))
cendat_OR_1990 %<>% subset(!is.na(pct_white))

#### KNOXVILLE - Pulls and cleaning ####
# set counties and pull data from census
countylist_TN <- c('Knox', 'Anderson', 'Sevier', 'Blount')

cendat_TN_2010 <- get_decennial(geography = "tract", variables = vars2010, year = 2010, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Tennessee', county = countylist_TN)

cendat_TN_2000 <- get_decennial(geography = "tract", variables = vars2000, year = 2000, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Tennessee', county = countylist_TN)

cendat_TN_1990 <- get_decennial(geography = "tract", variables = vars1990, year = 1990, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P0010001', state= 'Tennessee', county = countylist_TN)

# Changing to %
cendat_TN_2010$pct_white <- round(cendat_TN_2010$P003002 / cendat_TN_2010$summary_value, 3) * 100 
cendat_TN_2000$pct_white <- round(cendat_TN_2000$P003003 / cendat_TN_2000$summary_value, 3) * 100 
cendat_TN_1990$pct_white <- round(cendat_TN_1990$P0070001 / cendat_TN_1990$summary_value, 3) * 100 

# eliminate NAs
cendat_TN_2010 %<>% subset(!is.na(pct_white))
cendat_TN_2000 %<>% subset(!is.na(pct_white))
cendat_TN_1990 %<>% subset(!is.na(pct_white))

#### SEATTLE - Pulls and Cleaning ####
# set counties and pull data from census
countylist_WA <- c('King', 'Snohomish', 'Kitsap', 'Island')

cendat_WA_2010 <- get_decennial(geography = "tract", variables = vars2010, year = 2010, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Washington', county = countylist_WA)

cendat_WA_2000 <- get_decennial(geography = "tract", variables = vars2000, year = 2000, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Washington', county = countylist_WA)

cendat_WA_1990 <- get_decennial(geography = "tract", variables = vars1990, year = 1990, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P0010001', state= 'Washington', county = countylist_WA)

# Changing to %
cendat_WA_2010$pct_white <- round(cendat_WA_2010$P003002 / cendat_WA_2010$summary_value, 3) * 100 
cendat_WA_2000$pct_white <- round(cendat_WA_2000$P003003 / cendat_WA_2000$summary_value, 3) * 100 
cendat_WA_1990$pct_white <- round(cendat_WA_1990$P0070001 / cendat_WA_1990$summary_value, 3) * 100 

# eliminate NAs
cendat_WA_2010 %<>% subset(!is.na(pct_white))
cendat_WA_2000 %<>% subset(!is.na(pct_white))
cendat_WA_1990 %<>% subset(!is.na(pct_white))

#### DETROIT - Pulls and Cleaning ####
# set counties and pull data from census
countylist_MI <- c('Oakland', 'Macomb', 'Wayne')

cendat_MI_2010 <- get_decennial(geography = "tract", variables = vars2010, year = 2010, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Michigan', county = countylist_MI)

cendat_MI_2000 <- get_decennial(geography = "tract", variables = vars2000, year = 2000, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P001001', state= 'Michigan', county = countylist_MI)

cendat_MI_1990 <- get_decennial(geography = "tract", variables = vars1990, year = 1990, geometry = TRUE, output = 'wide', shift_geo = FALSE, summary_var = 'P0010001', state= 'Michigan', county = countylist_MI)

# Changing to %
cendat_MI_2010$pct_white <- round(cendat_MI_2010$P003002 / cendat_MI_2010$summary_value, 3) * 100 
cendat_MI_2000$pct_white <- round(cendat_MI_2000$P003003 / cendat_MI_2000$summary_value, 3) * 100 
cendat_MI_1990$pct_white <- round(cendat_MI_1990$P0070001 / cendat_MI_1990$summary_value, 3) * 100 

# eliminate NAs
cendat_MI_2010 %<>% subset(!is.na(pct_white))
cendat_MI_2000 %<>% subset(!is.na(pct_white))
cendat_MI_1990 %<>% subset(!is.na(pct_white))

#### save datasets ####
save(cendat_MA_1990, cendat_MA_2000, cendat_MA_2010, cendat_DC_1990, cendat_DC_2000, cendat_DC_2010, cendat_CA_1990, cendat_CA_2000, cendat_CA_2010, cendat_OR_1990, cendat_OR_2000, cendat_OR_2010, cendat_WA_1990, cendat_WA_2000, cendat_WA_2010, cendat_TN_1990, cendat_TN_2000, cendat_TN_2010, cendat_MI_1990, cendat_MI_2000, cendat_MI_2010, file = 'C:/Users/Erin/Documents/GitHub/finalproject/SavedData.Rdata')


#### permutation files ####
# read csv files for year csv files. all these files are in your github path
year2000 <- read.csv("year2000.csv")
year1990 <- read.csv("year1990.csv")
year2010 <- read.csv("year2010.csv")

#### convert everything to dataframes for permutation tests-----------
year2000 <- as.data.frame(year2000)
year1990 <- as.data.frame(year1990)
year2010 <- as.data.frame(year2010)
cendat_MA_1990 <- as.data.frame(cendat_MA_1990)
cendat_MA_2000 <- as.data.frame(cendat_MA_2000)
cendat_MA_2010 <- as.data.frame(cendat_MA_2010)
#add years dataframe to census dataframe
testing_cendat_MA_2000 <- cbind(cendat_MA_2000,year2000)
testing_cendat_MA_1990 <- cbind(cendat_MA_1990,year1990)
testing_cendat_MA_2010 <- cbind(cendat_MA_2010,year2010)

#### subsetting dataframes for permutation testing-----------------------------------
sub_testing_cendat_MA_2000 <- as.data.frame(testing_cendat_MA_2000[,c('NAME','GEOID','year','pct_white')])
sub_testing_cendat_MA_2000 <- subset(sub_testing_cendat_MA_2000,select=c(NAME,GEOID,year,pct_white))
sub_testing_cendat_MA_1990 <- as.data.frame(testing_cendat_MA_1990[,c('NAME','GEOID','year','pct_white')])
sub_testing_cendat_MA_1990 <- subset(sub_testing_cendat_MA_1990,select=c(NAME,GEOID,year,pct_white))

sub_testing_cendat_MA_2010 <- as.data.frame(testing_cendat_MA_2010[,c('NAME','GEOID','year','pct_white')])
sub_testing_cendat_MA_2010 <- subset(sub_testing_cendat_MA_2010,select=c(NAME,GEOID,year,pct_white))


#### prepping for 3 permutations-----------------------------------------------------------
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

#### for loop for 1990-2000 ------------------------------------------------------------------------
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
#plot
plot1

#p value
pvalue1 <- 2*mean(mean_diffs <= observed_mean)
pvalue1


#### permutation for 2000-2010-------------------------------------------------------
set.seed(1)
testing_MA_2000_2010$year_permuted <- sample(testing_MA_2000_2010$year)
testing_MA_2000_2010[c('year', 'year_permuted')]

# ttest
R <- 10000
mean_diffs <- rep(NA, R)

# for loop for permuting
for(i in 1:R){
  testing_MA_2000_2010$year_permuted <- sample(testing_MA_2000_2010$year_permuted)
  means <- tapply(testing_MA_2000_2010$pct_white, testing_MA_2000_2010$year_permuted, mean,na.rm=TRUE)
  mean_diffs[i] <- means['2010'] - means['2000']
  if(i %% 1000 == 0) print(i)
}
means <- tapply(testing_MA_2000_2010$pct_white, testing_MA_2000_2010$year, mean,na.rm=TRUE)
observed_mean <- means['2010'] - means['2000']
mean_diffs <- data.frame(mean_diffs = mean_diffs)
plot2 <- ggplot(mean_diffs, aes(x = mean_diffs)) + geom_histogram() + geom_vline(xintercept = observed_mean, col = 'red', lty = 2) + ggtitle("Distribution of permuted mean differences: \nBoston 2000 & 2010 census")
#plot
plot2
#p-value
pvalue2 <- 2*mean(mean_diffs <= observed_mean)
pvalue2

#### permutation for 1990-2010-------------------------------------------------------
set.seed(1)
testing_MA_1990_2010$year_permuted <- sample(testing_MA_1990_2010$year)
testing_MA_1990_2010[c('year', 'year_permuted')]

# ttest
t.test(pct_white ~ year_permuted, testing_MA_1990_2010)
R <- 10000
mean_diffs <- rep(NA, R)

# for loop for permuting
for(i in 1:R){
  testing_MA_1990_2010$year_permuted <- sample(testing_MA_1990_2010$year_permuted)
  means <- tapply(testing_MA_1990_2010$pct_white, testing_MA_1990_2010$year_permuted, mean,na.rm=TRUE)
  mean_diffs[i] <- means['2010'] - means['1990']
  if(i %% 1000 == 0) print(i)
}
means <- tapply(testing_MA_1990_2010$pct_white, testing_MA_1990_2010$year, mean,na.rm=TRUE)
observed_mean <- means['2010'] - means['1990']
mean_diffs <- data.frame(mean_diffs = mean_diffs)
plot3 <- ggplot(mean_diffs, aes(x = mean_diffs)) + geom_histogram() + geom_vline(xintercept = observed_mean, col = 'red', lty = 2) + ggtitle("Distribution of permuted mean differences: \nBoston 1990 & 2010 census")
#plot
plot3
#p value
pvalue3 <- round(2*mean(mean_diffs <= observed_mean))
pvalue3

#### save permutation plots ####
save(plot1, plot2, plot3, file = 'C:/Users/Erin/Documents/GitHub/finalproject/SavedPlots.Rdata')

#### consolidate saved file ####
rm(list = ls())

load("SavedData.Rdata")
load("SavedPlots.Rdata")

save(cendat_MA_1990, cendat_MA_2000, cendat_MA_2010, cendat_DC_1990, cendat_DC_2000, cendat_DC_2010, cendat_CA_1990, cendat_CA_2000, cendat_CA_2010, cendat_OR_1990, cendat_OR_2000, cendat_OR_2010, cendat_WA_1990, cendat_WA_2000, cendat_WA_2010, cendat_TN_1990, cendat_TN_2000, cendat_TN_2010, cendat_MI_1990, cendat_MI_2000, cendat_MI_2010, plot1, plot2, plot3, file = 'C:/Users/Erin/Documents/GitHub/finalproject/Saved.Rdata')