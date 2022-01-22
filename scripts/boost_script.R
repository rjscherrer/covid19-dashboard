library(dplyr)
library(readxl)
library(randomForest)
library(xlsx)
library(caret)
library(xgboost)

load("data/boost.Rdata")

boost_data <- read_excel("data/boost_data.xlsx")
boost_data <- boost_data[,-1]

predict_boosting = function(model, data, ndays, count = 1) {
  matr <- as.matrix(data[,-1])
  pred <- predict(boost, newdata = matr)

  data <- cbind(data[,1], pred, data[,c(2:5, 7:ncol(data))])
  colnames(data)[1:6] <- c("Country/Region", "T1", "T2", "T3", "T4", "T5")

  data$d_first <- ifelse(data$d_first > 0, data$d_first + 1, 0)
  data$d_Grossveranstaltung <- ifelse(data$d_Grossveranstaltung > 0, data$d_Grossveranstaltung + 1, 0)
  data$d_wirtschaft <- ifelse(data$d_wirtschaft > 0, data$d_wirtschaft + 1, 0)
  data$d_social_distancing <- ifelse(data$d_social_distancing > 0, data$d_social_distancing + 1, 0)
  data$d_kontakt <- ifelse(data$d_kontakt > 0, data$d_kontakt + 1, 0)
  data$d_ausgang <- ifelse(data$d_ausgang > 0, data$d_ausgang + 1, 0)

  if(count < ndays){
    count = count + 1
    predict_boosting(boost, data, ndays, count = count)
  }else{
    data$country_code <- c(
      "AUT", "BEL", "BIH",
      "HRV", "DNK", "FIN",
      "FRA", "DEU", "GRC",
      "ISL", "IRL", "ITA",
      "NLD", "NOR", "POL",
      "PRT", "RUS", "SVK",
      "SVN", "ESP", "SWE",
      "CHE", "UKR", "GBR"
    )
    return(data)
  }
}

# test <- predict_boosting(boost, boost_data, 10)