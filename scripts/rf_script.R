#RF Prediction script
library(dplyr)
library(readxl)
library(randomForest)
library(xlsx)

load("data/RF.Rdata")
rf_data <- read_excel("data/rf_data.xlsx")
rf_data <- rf_data[,-1]


predict_randomForest = function(model, data, ndays, count = 1){
  data$T0 <- stats::predict(model, data)
  data <- data[,c(1:14, ncol(data), 15:(ncol(data)-1))]
  
  #shift columns
  for(i in 15:ncol(data)){
    colnames(data)[i] <- paste("T", i-14, sep = "")
  }
  
  data$d_first <- ifelse(data$d_first > 0, data$d_first + 1, 0)
  data$d_Grossveranstaltung <- ifelse(data$d_Grossveranstaltung > 0, data$d_Grossveranstaltung + 1, 0)
  data$d_wirtschaft <- ifelse(data$d_wirtschaft > 0, data$d_wirtschaft + 1, 0)
  data$d_social_distancing <- ifelse(data$d_social_distancing > 0, data$d_social_distancing + 1, 0)
  data$d_kontakt <- ifelse(data$d_kontakt > 0, data$d_kontakt + 1, 0)
  data$d_ausgang <- ifelse(data$d_ausgang > 0, data$d_ausgang + 1, 0)
  
  
  #recursive call
  if(count < ndays){
    predict_randomForest(model, data, ndays, count = count + 1)
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

#rf_pred <- predict_randomForest(model_RF, rf_data, 10)