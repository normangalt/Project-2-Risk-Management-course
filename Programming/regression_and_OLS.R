library(readxl)

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
data$r <- c(R, mean(R))
#Spliting data on training and testing sets

train_n <- as.integer(0.85*length(data$yyyymm))+1

dtrain <- data[1:train_n, ]
dtest <- data[-(1:train_n), ]

d <- 3

R2<-function(y,pred) {
  1-sum((y-pred)^2)/sum((y-mean(y))^2)
}

rmse<-function(y,pred) {
  sqrt(mean((y-pred)^2)) 
} 

s <- function(x){
  return (x**d)
}

#Predictive regression
model <- lm(r~log(E12, base=10) +
              log(D12, base=10) +
              DE12 + svar +
              bm + ntis +
              tbl + ltr +
              TSPREAD + DSPREAD +
              DRSPREAD + infl,
            data=data[-(length(data):length(data)),])

summary(model)

sum((data$r - predict(model, newdata = data[-(length(data):length(data)), ]))^2)/length(data[-(length(data):length(data)), ]$ltr)
R2(data$r, predict(model,newdata=data[-(length(data):length(data)), ]))

plot(model)
plot(model$fitted.values, unlist(data[2:length(data$r)-1,]$r))
plot(predict(model,newdata=data), data$r)

data <- data[data$yyyymm>197912,]
data <- data[1:468, ]
train_n <- as.integer(0.85*length(data$yyyymm))+1

dtrain <- data[1:train_n, ]
dtest <- data[-(1:train_n), ]

#OLS regression
ols <- lm(r~s(log(E12, base=10)) +
              s(log(D12, base=10)) +
              s(DE12) + s(svar) +
              s(bm) + s(ntis) +
              s(tbl) + s(ltr) +
              s(TSPREAD) + s(DSPREAD) +
              s(DRSPREAD) + s(infl),
              data=dtrain[-(length(dtrain):length(dtrain)),])

summary(ols)

plot(ols)
plot(ols$fitted.values, unlist(dtrain[2:length(dtrain$r)-1,]$r))
plot(predict(ols,newdata=dtrain), dtrain$r)

rmse(dtrain$r, predict(ols, newdata = dtrain[-(length(dtrain):length(dtrain)),]))
R2(dtrain$r, predict(ols, newdata = dtrain[-(length(dtrain):length(dtrain)),]))

dtest_rows <- nrow(dtest)

predictionsOLS <- 1:nrow(dtest)
metrics_windows_OLS <- array(dim = c(dtest_rows, 2))
for (index in 1:nrow(dtest)){
  prediction <- predict(ols, newdata = dtest[index, ])
  predictionsOLS[index] <- prediction
  
  window <- data[index:train_n+index, ]
  
  data[train_n+index, "r"] <- prediction
  
  ols <- lm(r~log(E12, base=10)**3 +
              log(D12, base=10)**3 +
              DE12**3 + svar**3 +
              bm**3 + ntis**3 +
              tbl**3 + ltr**3 +
              TSPREAD**3 + DSPREAD**3 +
              DRSPREAD**3 + infl**3,
              data=window)

  rms<- rmse(window$r, ols$fitted.values)
  r2 <- R2(window$r, ols$fitted.values)

  metrics_windows_OLS[index, 1] <- rms
  metrics_windows_OLS[index, 2] <- r2
}

summary(ols)

plot(ols)
plot(ols$fitted.values, window$r)
plot(predictionsOLS, dtest$r)

sum(metrics_windows_OLS[,1])/length(metrics_windows_OLS[,1])
sum(metrics_windows_OLS[,2])/length(metrics_windows_OLS[,2])

rmse(dtest$r, predictionsOLS)
R2(dtest$r, predictionsOLS)
