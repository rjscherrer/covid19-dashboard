library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

data_raw <- read.csv("./data/time_series_covid19_confirmed_global.csv")
data_iso <- read.csv("./data/iso_codes.csv")

# filter for european countries
european_countries <- c("Germany", "United Kingdom", "France", "Italy", 
                        "Spain", "Ukraine", "Poland", "Romania", "Netherlands", 
                        "Belgium", "Czechia", "Greece", "Portugal", "Sweden", 
                        "Hungary", "Belarus", "Austria", "Serbia", "Switzerland",
                        "Bulgaria", "Denmark", "Finland", "Slovakia", "Norway",
                        "Ireland", "Croatia", "Moldova", "Bosnia and Herzegovina",
                        "Albania", "Lithuania", "North Macedonia", "Slovenia", 
                        "Latvia", "Kosovo", "Estonia", "Montenegro", "Luxembourg",
                        "Malta", "Iceland", "Andorra", "Monaco", "Liechtenstein",
                        "San Marino", "Holy See")

data_cases <- data_raw[data_raw$Country.Region %in% european_countries,]

# aggregate provinces
country_coordinates <- data_cases[data_cases$Province.State=="",]
data_cases <- data_cases %>% select(-Province.State)  
data_cases <- aggregate(. ~ Country.Region, data_cases, sum)
data_cases$Lat <- country_coordinates$Lat
data_cases$Long <- country_coordinates$Long

# add row for europe
data_cases[nrow(data_cases)+1, ] <- NA
data_cases[nrow(data_cases), 1] <- "Europe"
data_cases[nrow(data_cases), 
             4:ncol(data_cases)] <- colSums(data_cases[4:ncol(data_cases)], 
                                              na.rm=TRUE)

# sort by region
data_cases <- data_cases[order(data_cases[,1]),]

# separate data for easier processing
data_countries <- data_cases[, 1:3] %>%
   merge(., data_iso, by = "Country.Region", all.x = TRUE)

# split data to list
data_cases <- data_cases[, c(1,4:ncol(data_cases))] %>%
   tidyr::gather(key = "Date", value = "Cases.Total", -Country.Region) %>%
   dplyr::mutate(Date=as.Date(Date, format="X%m.%d.%y")) %>%
   dplyr::group_by(Country.Region) %>%
   dplyr::group_split() %>%
   as.list() %>%
   setNames(data_countries$Country.Region)

# add column containing new cases
for (i in c(1:length(data_cases))) {
   cases.new <- c(data_cases[[i]]$Cases.Total[1], diff(data_cases[[i]]$Cases.Total))
   data_cases[[i]] <- add_column(data_cases[[i]], Cases.New=cases.new)
}

# save output to disk
save(data_countries, data_cases, file = "./data/data_cleaned.RData")

# clean up main memory
rm(list=setdiff(ls(), c("data_cases", "data_countries")))