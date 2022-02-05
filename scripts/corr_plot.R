library(corrplot)
library(tidyverse)

data_cases <- readRDS(file = "./data/data_cases.RDS")
data_combined <- map_dfr(data_cases, bind_rows) %>%
   dplyr::filter(Country.Region != "Europe")
data_combined <- data_combined[, c("Country.Region", "Rt.Most.Likely", "Date")]
data_combined <-  spread(data_combined, key = Country.Region, value = Rt.Most.Likely)
corrplot::corrplot(cor(data_combined[,2:ncol(data_combined)]))
