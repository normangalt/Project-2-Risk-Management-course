library(readxl)

#Reading and filtering data
data <- read_xlsx("PredictorData2018.xlsx")
data <- data[data$yyyymm>197912,]
data <- data[1:468, ]

colnames(data)[5] <- "bm"

#Converting data
data["ltr"] <- as.double(data$ltr)
data["tbl"] <- as.double(data$tbl)
data["AAA"] <- as.double(data$AAA)
data["BAA"] <- as.double(data$BAA)
data["infl"] <- as.double(data$infl)
data["ntis"] <- as.double(data$ntis)
data["bm"] <- as.double(data$bm)
data["corpr"] <- as.double(data$corpr)
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
data$R <- c(R, mean(R))
#Spliting data on training and testing sets
train_n <- as.integer(0.85*length(data$yyyymm))+1

dtrain <- data[1:train_n, ]
dtest <- data[-(1:train_n), ]

R2<-function(y,pred) {
  #R2=1-SSR/SST
  1-sum((y-pred)^2)/sum((y-mean(y))^2) 
}

rmse<-function(y,pred) {
  sqrt(mean((y-pred)^2)) 
} 
s <- function(x){
  return (x**d)
}

trControl <- trainControl(method="timeslice",
                          initialWindow = train_n -1,
                          fixedWindow = TRUE,
                          horizon = 1)

#Elastic net regression
elastic <- train(R~log(E12, base=10)**3 +
                   log(D12, base=10)**3 +
                   DE12**3 + svar**3 +
                   bm**3 + ntis**3 +
                   tbl**3 + ltr**3 +
                   TSPREAD**3 + DSPREAD**3 +
                   DRSPREAD**3 + infl**3,
                 data,
                 method = "glmnet",
                 tuneGrid = expand.grid(alpha=seq(0, 1, by = 0.1), lambda=seq(0, 1, by = 0.01)),
                 trControl = trControl)

elastic
best_params <- elastic$bestTune
best_params

sum(elastic$results$RMSE[!is.na(elastic$results$RMSE)])/length(elastic$results$RMSE[!is.na(elastic$results$RMSE)])
sum(elastic$results$Rsquared[!is.na(elastic$results$Rsquared)])/length(elastic$results$Rsquared[!is.na(elastic$results$Rsquared)])

predictionsElasticTrain <- predict(ridge, 
                                 lambda=best_params$lambda,
                                 alpha =best_params$alpha,
                                 newdata = dtrain[-(length(dtrain):length(dtrain)),])

rmse(dtrain$R, predictionsElasticTrain)
R2(dtrain$R, predictionsElasticTrain)

dtest_rows <- nrow(dtest)

predictionsElastic <- 1:nrow(dtest)
metrics_elastic <- array(dim=c(nrow(dtest), 2))
for (index in 1:dtest_rows){
  prediction <- predict(ridge,
                        lambda=best_params$lambda,
                        alpha =best_params$alpha,
                        newdata = dtest[index, ])
  predictionsElastic[index] <- prediction
  
  start_<- index+1
  end_  <- train_n+index
  window <- data[(start_:end_), ]
  
  data[end_, "R"] <- prediction
  elastic <- train(R~log(E12, base=10)**3 +
                     log(D12, base=10)**3 +
                     DE12**3 + svar**3 +
                     bm**3 + ntis**3 +
                     tbl**3 + ltr**3 +
                     TSPREAD**3 + DSPREAD**3 +
                     DRSPREAD**3 + infl**3,
                  window,
                 method = "glmnet",
                 tuneGrid = expand.grid(alpha=best_params$alpha, lambda=best_params$lambda),
                 trControl=trainControl(method="none"))
  
  metrics_elastic[index, 1] <- sum((window$R - predict(elastic, newdata = window))^2)/length(window$ltr)
  metrics_elastic[index, 2] <- R2(window$R, predict(elastic, newdata = window,
                                                    alpha=best_params$alpha,
                                                    lambda=best_params$lambda))

}
elastic

plot(predictionsElastic, dtest$R)

sum(metrics_elastic[,1])/length(metrics_elastic[,1])
sum(metrics_elastic[,2])/length(metrics_elastic[,2])

rmse(dtest$R, predictionsElastic)
R2(dtest$R, predictionsElastic)
