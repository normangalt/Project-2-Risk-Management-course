library(readxl)
library(sigmoid)

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

data["D12"] <- log(data["D12"], base = 10)
data["E12"] <- log(data["E12"], base = 10)

data <- subset(data, select = c("ltr", "ntis", "svar", "infl",
                                "DRSPREAD", "D12", "bm", "tbl",
                                "DSPREAD", "E12",
                                "TSPREAD", "DE12", "Index", "yyyymm"))

#Computing R column
R <- unlist(log(data[-(1:1), "Index"] +
                  data[-(1:1), "D12"]/12) - log(data[-(length(data):length(data)), "Index"], base=10))

data <- subset(data, select = c("ltr", "ntis", "svar", "infl",
                                "DRSPREAD", "D12", "bm", "tbl",
                                "DSPREAD", "E12",
                                "TSPREAD", "DE12"))
data <- data**3

#Spliting data on training and testing sets
train_n <- as.integer(0.85*length(data$ltr))+1

data$r <- c(R, mean(R))

dtrain <- data[1:train_n, ]
dtest <- data[-(1:train_n), ]

R2<-function(y,pred) {
  1-sum((y-pred)^2)/sum((y-mean(y))^2) 
}

rmse<-function(y,pred) {
  sqrt(mean((y-pred)^2)) 
} 

library(neuralnet)
library(tidyverse)

dtrain <- as.data.frame(dtrain)
names(dtrain) <- make.names(names(dtrain))
learning_rates <- seq(0.1, 1, 0.1)

r2s <- array(dim = c(length(learning_rates)))
rmses <- array(dim = c(length(learning_rates)))
for (index in 1:length(learning_rates)){
  learnig_rate <- learning_rates[index]
  nn_shallow <- neuralnet(r~E12 +
                            D12 +
                            DE12 + svar +
                            bm + ntis +
                            tbl + ltr +
                            TSPREAD + DSPREAD +
                            DRSPREAD + infl,
                          data=dtrain,
                          hidden = 8,
                          learningrate = learnig_rate)

  r2 <- R2(unlist(nn_shallow$response), unlist(nn_shallow$net.result))
  rms <- rmse(unlist(nn_shallow$response), unlist(nn_shallow$net.result))
  
  r2s[index] <- rms
  rmses[index] <- r2
}

max(r2s)
mean(r2s)
min(r2s)

max(rmses)
mean(rmses)
min(rmses)

index <- which(min(r2s)==r2s)
learning_rate <- learning_rates[index]

nn_shallow <- neuralnet(r~E12 +
                          D12 +
                          DE12 + svar +
                          bm + ntis +
                          tbl + ltr +
                          TSPREAD + DSPREAD +
                          DRSPREAD + infl,
                        data=dtrain,
                        hidden = 8,
                        learningrate = learning_rate)

predictions_train <- predict(nn_shallow, newdata=dtrain)
plot(unlist(nn_shallow$response), unlist(nn_shallow$net.result))
plot(unlist(dtrain$r), predictions_train)

r2 <- R2(unlist(dtrain$r), predictions_train)
rms <- rmse(unlist(dtrain$r), predictions_train)

r2
rms

metrics_windows <- array(dim=c(nrow(dtest), 2))
predictions_nn <- array(dim=c(nrow(dtest)))
for (index in 1:nrow(dtest)){
  prediction <- predict(nn_shallow, newdata=dtest[index, ])
  predictions_nn[index] <- prediction

  start <- index
  end <- index + train_n

  window <- data[start:end, ]
  
  index_prediction <- train_n + index
  data[index_prediction, "r"] <- prediction
  nn_shallow <- neuralnet(r~E12 +
                            D12 +
                            DE12 + svar +
                            bm + ntis +
                            tbl + ltr +
                            TSPREAD + DSPREAD +
                            DRSPREAD + infl,
                          data=window,
                          hidden = 8,
                          learningrate = learning_rate)
  
  r2 <- R2(unlist(nn_shallow$response), unlist(nn_shallow$net.result))
  rms <- rmse(unlist(nn_shallow$response), unlist(nn_shallow$net.result))

  metrics_windows[index, 1] <- r2
  metrics_windows[index, 2] <- rms
}

mean(metrics_windows[, 1])
mean(metrics_windows[, 2])

R2(dtest$r, predictions_nn)
rmse(dtest$r, predictions_nn)

plot(unlist(nn_shallow$response), unlist(nn_shallow$net.result))
plot(dtest$r, predictions_nn)
