#playing around with zipcode data from ACS????? 


library(acs)
library(dplyr)
library(tidycensus)
library(tidyverse)
library(totalcensus)
set_path_to_census("xxxxx/my_census_data")

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
ma_zip <- get_decennial(geography = "zcta", 
              variables = c(medincome = "B19013_001"), 
              state = "MA")

ma_zip1 <- get_decennial(geography = "zcta", 
                  variables="B19013_001",state="MA"
                )

#getting an error here
zips <- geo.make(state = "Illinois", key = "Chicago", zip.code = "*")


getCensus <- function(get, for_val, key){
  # API Documentation @
  # http://www.census.gov/prod/cen2010/doc/sf1.pdf
  # Take in selected columns to get (comma delim)
  # and for_val to determine geo
  # Allowed States c(paste("0",c(1:2,4:9),sep=""),
  # 10:13, 15:42,44:51,53:56,72)
  url <- paste("http://api.census.gov/data/2010/sf1?key=",
               key, "&get=", get, "&for=", for_val, sep="")
  print (paste("Accessing...", url))
  out <- readLines(textConnection(getURL(url)))
  #Clean up the data by removing [] and " characters
  clean_out <- gsub(pattern=",$", replacement = "",
                    x = gsub(pattern = '\\[|\\]|\\"|\\\\',"", out))
  header <- unlist(strsplit(clean_out[1],split=","))
  body <- clean_out[2:length(clean_out)]
  body_list <- strsplit(body,split=",")
  body_df <- data.frame(matrix(unlist(body_list),
                               nrow=length(body_list),
                               byrow=TRUE),
                        stringsAsFactors=FALSE)
  names(body_df) <- header
  return(body_df)
}

getCensus("P0030001", "zip+code+tabulation+area:*&in=state:01", key = "0b289a21471b97cfafe34c8cb1d08069a851371b" )
