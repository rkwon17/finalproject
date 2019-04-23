#playing around with zipcode data from ACS????? 


library(acs)
library(dplyr)
library(tidycensus)
library(tidyverse)

#store census key
census_api_key("0b289a21471b97cfafe34c8cb1d08069a851371b")
#set working directory
setwd("/Users/rachelkwon/Github/finalproject")

#load zipcode csv file
zipcode <- read.csv(file="ZIP_Codes.csv")
summary(zipcode)


#example
m90 <- get_decennial(geography = "state", variables = "H043A001", year = 1990)
#medium income based on each county of MA
ma_zip <- get_acs(geography = "zcta", 
              variables = c(medincome = "B19013_001"), 
              state = "MA")
#getting an error here
zips <- geo.make(state = "Illinois", key = "Chicago", zip.code = "*")
