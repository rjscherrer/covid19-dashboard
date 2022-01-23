library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

data_raw <- read.csv("./data/time_series_covid19_confirmed_global.csv")

# filter for european countries
european_countries <- c("Russia", "Germany", "United Kingdom", "France", "Italy", 
                        "Spain", "Ukraine", "Poland", "Romania", "Netherlands", 
                        "Belgium", "Czechia", "Greece", "Portugal", "Sweden", 
                        "Hungary", "Belarus", "Austria", "Serbia", "Switzerland",
                        "Bulgaria", "Denmark", "Finland", "Slovakia", "Norway",
                        "Ireland", "Croatia", "Moldova", "Bosnia and Herzegovina",
                        "Albania", "Lithuania", "North Macedonia", "Slovenia", 
                        "Latvia", "Kosovo", "Estonia", "Montenegro", "Luxembourg",
                        "Malta", "Iceland", "Andorra", "Monaco", "Liechtenstein",
                        "San Marino", "Holy See")

data_cleaned <- data_raw[data_raw$Country.Region %in% european_countries,]

# aggregate provinces
country_coordinates <- data_cleaned[data_cleaned$Province.State=="",]
data_cleaned <- data_cleaned %>% select(-Province.State)  
data_cleaned <- aggregate(. ~ Country.Region, data_cleaned, sum)
data_cleaned$Lat <- country_coordinates$Lat
data_cleaned$Long <- country_coordinates$Long

# separate data for easier processing
data_countries <- data_cleaned[, 1:3]

data_cases <- data_cleaned[, c(1,4:ncol(data_cleaned))] %>%
   tidyr::gather(key = "Date", value = "Cases.Total", -Country.Region) %>%
   dplyr::mutate(Date=as.Date(Date, format="X%m.%d.%y")) %>%
   dplyr::group_by(Country.Region) %>%
   dplyr::group_split() %>%
   setNames(data_countries$Country.Region)

a <- data_cases[["Albania"]] %>% add_column(Add_Column = 1)

# plot example
# ggplot(data=data_cases[data_cases$Country.Region=="Switzerland",], 
#        aes(x=c(1:nrow(ch)), y=Cases)) +
#    geom_line()

# clean up main memory
rm(list=setdiff(ls(), c("data_countries", "data_cases")))
