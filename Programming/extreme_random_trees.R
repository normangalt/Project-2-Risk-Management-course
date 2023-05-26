library(tidymodels)
library(readxl)
library(caret)
library(ranger)

#Reading and filtering data
data <- read_xlsx("PredictorData2018.xlsx")
data <- data[data$yyyymm>194912,]
data <- data[1:828, ]

colnames(data)[5] <- "bm"

#Converting data
data["ltr"] <- as.double(data$ltr)
data["tbl"] <- as.double(data$tbl)
data["AAA"] <- as.double(data$AAA)
data["BAA"] <- as.double(data$BAA)
data["infl"] <- as.double(data$infl)
data["ntis"] <- as.double(data$ntis)
data["bm"] <- as.double(data$bm)
data["svar"] <- as.double(data$svar)
data["DE12"] <- data["D12"]/data["E12"]
data["TSPREAD"] <- data["ltr"]-data["tbl"]
data["DSPREAD"] <- data["AAA"]-data["BAA"]
data["DRSPREAD"] <- data["AAA"]-data["ltr"]

data <- subset(data, select = c("ltr", "ntis", "svar", "infl",
                                "DRSPREAD", "D12", "bm", "tbl",
                                "DSPREAD", "E12",
                                "TSPREAD", "DE12", "Index", "yyyymm"))

#Computing R column
R <- unlist(log(data[-(1:1), "Index"] +
                  data[-(1:1), "D12"]/12) - log(data[-(length(data):length(data)), "Index"], base=10))

train_n <- as.integer(0.85*length(data$yyyymm))+1
data$R <- c(R, mean(R))

dtrain <- data[1:train_n, ]
dtest <- data[-(1:train_n), ]


R2<-function(y,pred) {
  1-sum((y-pred)^2)/sum((y-mean(y))^2) 
}

rmse<-function(y,pred) {
  sqrt(mean((y-pred)^2)) 
}
s <- function(x){
  return (x**d)
}
trControl <- trainControl(method="timeslice",
                          initialWindow = 100,
                          fixedWindow = TRUE,
                          horizon=1)
mtry_value = sqrt(12)

resamples <- sliding_window(dtrain[(1:length(dtrain$yyyymm)-1),])
final_model_ex_fit <- train(R~log(E12, base=10)**3 +
                              log(D12, base=10)**3 +
                              DE12**3 + svar**3 +
                              bm**3 + ntis**3 +
                              tbl**3 + ltr**3 +
                              TSPREAD**3 + DSPREAD**3 +
                              DRSPREAD**3 + infl**3,
                          dtrain,
                          method = "rf",
                          resamples=resamples,
                          splitrule = "extratrees",
                          trControl = trainControl(method="none"))

vip::vip(final_model_ex_fit)

ex_predictions <- predict(final_model_ex_fit, newdata=dtrain)
plot(unlist(ex_predictions), unlist(dtrain$R))

predictions <- array(dim = c(nrow(dtest)))
metrics_ex <- array(dim = c(nrow(dtest), 2))
for (index in 1:nrow(dtest)){
  predictions[index] <- predict(final_model_ex_fit, newdata=dtest[index,])

  start_<- index+1
  end_  <- train_n+index
  window_ <- data[(start_:end_), ]
  final_model_ex_fit <- train(R~log(E12, base=10)**3 +
                                log(D12, base=10)**3 +
                                DE12**3 + svar**3 +
                                bm**3 + ntis**3 +
                                tbl**3 + ltr**3 +
                                TSPREAD**3 + DSPREAD**3 +
                                DRSPREAD**3 + infl**3,
                              window_,
                              method = "rf",
                              splitrule = "extratrees",
                              trControl = trainControl(method="none"),
                              splitrule = "extratrees")

  rms<- rmse(window_$R, predict(final_model_ex_fit, newdata = window_))
  r2 <- R2(window_$R, predict(final_model_ex_fit, newdata = window_))
  
  metrics_ex[index, 1] <- rms
  metrics_ex[index, 2] <- r2
}
vip::vip(final_model_ex_fit)
plot(dtest$R, unlist(predictions))

sum(metrics_ex[,1])/length(metrics_ex[,1])
sum(metrics_ex[,2])/length(metrics_ex[,2])

mean_rtest <- mean(dtest$R)
SST <- sum((dtest$R-mean_rtest)**2)
SSR <- sum((dtest$R - unlist(predictions))**2)
rmse_tree <- sqrt(sum((dtest$R - unlist(predictions))^2)/length(dtest$R))
R2_tree <- 1-(SSR)/(SST)
R2_tree
rmse_tree
