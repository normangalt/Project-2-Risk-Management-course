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
r <- unlist(log(data[-(1:1), "Index"] +
                  data[-(1:1), "D12"]/12) - log(data[-(length(data):length(data)), "Index"], base=10))

data$R <- c(r, mean(r))
#Spliting data on training and testing sets
train_n <- as.integer(0.85*length(data$yyyymm))+1

dtrain <- data[1:train_n, ]
dtest <- data[-(1:train_n), ]

R2<-function(y,pred) {
  1-sum((y-pred)^2)/sum((y-mean(y))^2) 
}

rmse<-function(y,pred) {
  sqrt(mean((y-pred)^2)) 
} 

s <- function(x){
  x**d
}

library(glmnet)
library(caret)

trControl <- trainControl(method="timeslice",
                          initialWindow = train_n-1,
                          fixedWindow = TRUE,
                          horizon=1)
#LASSO regression
lasso <- train(R~log(E12, base=10)**3 +
                 log(D12, base=10)**3 +
                 DE12**3 + svar**3 +
                 bm**3 + ntis**3 +
                 tbl**3 + ltr**3 +
                 TSPREAD**3 + DSPREAD**3 +
                 DRSPREAD**3 + infl**3,
               data=dtrain,
               method = "glmnet",
               tuneGrid = expand.grid(alpha=1, lambda=seq(0.1, 1, by = 0.1)),
               trControl = trControl
               )

lasso$results
best_params <- lasso$bestTune

best_params
sum(lasso$results$RMSE[!is.na(lasso$results$RMSE)])/sum(!is.na(lasso$results$RMSE))
sum(lasso$results$Rsquared[!is.na(lasso$results$Rsquared)])/sum(!is.na(lasso$results$Rsquared))

predictionsLassoTrain <- predict(lasso, 
                                lambda=best_params$lambda,
                                alpha =best_params$alpha,
                                newdata = data[-(length(dtrain):length(dtrain)),])

rmse(data$R, predictionsLassoTrain)
R2(data$R, predictionsLassoTrain)

dtest_rows <- nrow(dtest)

predictionsLasso <- 1:nrow(dtest)
metrics_windows_lasso <- array(dim=c(nrow(dtest), 2))
for (index in 1:dtest_rows){
  prediction <- predict(lasso,
                        lambda= best_params$lambda,
                        alpha = best_params$alpha,
                        newdata = dtest[index, ])
  predictionsLasso[index] <- prediction

  start_<- index
  end_  <- train_n+index
  window <- data[(start_:end_), ]
  
  data[end_, "R"] <- prediction
  lasso <- train(R~log(E12, base=10)**3 +
                   log(D12, base=10)**3 +
                   DE12**3 + svar**3 +
                   bm**3 + ntis**3 +
                   tbl**3 + ltr**3 +
                   TSPREAD**3 + DSPREAD**3 +
                   DRSPREAD**3 + infl**3,
                 data=dtrain,
                 method = "glmnet",
                 trControl=trainControl(method = "none"),
                 tuneGrid = expand.grid(alpha=best_params$alpha, 
                 lambda=best_params$lambda))

  rms<- rmse(window$R, predict(lasso, newdata = window,
                               alpha=best_params$alpha,
                               lambda=best_params$lambda))
  r2 <- R2(window$R, predict(lasso, newdata = window,
                             alpha=best_params$alpha,
                             lambda=best_params$lambda))

  metrics_windows_lasso[index, 1] <- rms
  metrics_windows_lasso[index, 2] <- r2

}
lasso

plot(predictionsLasso, dtest$R)

sum(metrics_windows_lasso[,1])/length(metrics_windows_lasso[,1])
sum(metrics_windows_lasso[,2])/length(metrics_windows_lasso[,2])

rmse(dtest$R, predictionsLasso)
R2(dtest$R, predictionsLasso)

