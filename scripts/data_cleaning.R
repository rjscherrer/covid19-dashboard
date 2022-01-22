library(dplyr)

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

