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
r <- unlist(log(data[-(1:1), "Index"] +
                  data[-(1:1), "D12"]/12) - log(data[-(length(data):length(data)), "Index"], base=10))
data$R <- c(r, mean(r))
#Spliting data on training and testing sets
train_n <- as.integer(0.85*length(data$yyyymm))+1

dtrain <- data[1:train_n, ]
dtest <- data[-(1:train_n), ]

s <- function(x){
  return (x**d)
}
R2<-function(y,pred) {
  1-sum((y-pred)^2)/sum((y-mean(y))^2) 
}

rmse<-function(y,pred) {
  sqrt(mean((y-pred)^2)) 
} 
#PCA
library(corrr)
library(FactoMineR)
library(corrplot)
library(factoextra)
require("pls")

means <- colSums(dtrain)/length(dtrain$yyyymm)

dtrain_ <- subset(dtrain, select = c("ltr", "ntis", "svar", "infl",
                                     "DRSPREAD", "D12", "bm", "tbl",
                                     "DSPREAD", "E12",
                                     "TSPREAD", "DE12"))
dataframeTrain <- data.frame(dtrain_)

normalizedTrain <- (dtrain - colMeans(dataframeTrain))/sapply(dataframeTrain, sd)

correlation_matrix <- cor(normalizedTrain)
heatmap(correlation_matrix)

pca <- princomp(correlation_matrix)
pca

fviz_eig(pca, addlabels = TRUE)

fviz_pca_var(pca, col.var = "red")
fviz_cos2(pca, choice="var",axes=1)

fviz_pca_var(pca, col.var = "cos2",
             gradient.cols=c("orange",
                             "green",
                             "red"),
             repel=TRUE)

pcr_reg <- pcr(R~log(E12, base=10)**3 +
                 log(D12, base=10)**3 +
                 DE12**3 + svar**3 +
                 bm**3 + ntis**3 +
                 tbl**3 + ltr**3 +
                 TSPREAD**3 + DSPREAD**3 +
                 DRSPREAD**3 + infl**3,
               data = dtrain, scale = TRUE)

validationplot(pcr_reg, val.type="RMSEP")
coefplot(pcr_reg)

metrics_windows_pca <- array(dim= c(nrow(dtest), 3, 2))
predictionsPCA <- array(dim= c(nrow(dtest), 3))
metrics_pca <- array(dim= c(3, 2))
for (index in 1:nrow(dtest)){
  prediction <- predict(pcr_reg, newdata = dtest[index, ])
  prediction <- sum(prediction[!is.na(prediction)])
  predictionsPCA[index, 1] <- prediction
  prediction <- predict(pcr_reg, newdata = dtest[index, ])
  prediction <- mean(prediction[!is.na(prediction)])
  predictionsPCA[index, 2] <- prediction
  prediction <- predict(pcr_reg, newdata = dtest[index, ])
  prediction <- mean(prediction[!is.na(prediction)])
  predictionsPCA[index, 3] <- prediction

  start_<- index+1
  end_  <- train_n+index
  window <- data[(start_:end_), ]
  data[end_, "R"] <- mean(predictionsPCA[index, ])

  pcr_reg <- pcr(R~log(E12, base=10)**3 +
                   log(D12, base=10)**3 +
                   DE12**3 + svar**3 +
                   bm**3 + ntis**3 +
                   tbl**3 + ltr**3 +
                   TSPREAD**3 + DSPREAD**3 +
                   DRSPREAD**3 + infl**3,
                 data = window, scale = TRUE, ncomp=3)

  rms<- rmse(window$R, pcr_reg$fitted.values)
  r2 <- R2(window$R, pcr_reg$fitted.values)

  metrics_windows_pca[index, 1, 1] <- rms
  metrics_windows_pca[index, 1, 2] <- r2
  
  pcr_reg <- pcr(R~log(E12, base=10)**3 +
                   log(D12, base=10)**3 +
                   DE12**3 + svar**3 +
                   bm**3 + ntis**3 +
                   tbl**3 + ltr**3 +
                   TSPREAD**3 + DSPREAD**3 +
                   DRSPREAD**3 + infl**3,
                 data = window, scale = TRUE, ncomp=5)
  
  rms<- rmse(window$R, pcr_reg$fitted.values)
  r2 <- R2(window$R, pcr_reg$fitted.values)

  metrics_windows_pca[index, 2, 1] <- rms
  metrics_windows_pca[index, 2, 2] <- r2

  pcr_reg <- pcr(R~log(E12, base=10)**3 +
                   log(D12, base=10)**3 +
                   DE12**3 + svar**3 +
                   bm**3 + ntis**3 +
                   tbl**3 + ltr**3 +
                   TSPREAD**3 + DSPREAD**3 +
                   DRSPREAD**3 + infl**3,
                 data = window, scale = TRUE,ncomp=10)
  
  rms<- rmse(window$R, pcr_reg$fitted.values)
  r2 <- R2(window$R, pcr_reg$fitted.values)
  
  metrics_windows_pca[index, 3, 1] <- rms
  metrics_windows_pca[index, 3, 2] <- r2
}

plot(predictionsPCA[,1], dtest$R)
metrics_pca[1, 1] <- rmse(dtest$R, predictionsPCA[, 1])
metrics_pca[1, 2] <- R2(dtest$R, predictionsPCA[, 1])

plot(predictionsPCA[,2], dtest$R)
metrics_pca[2, 1] <- rmse(dtest$R, predictionsPCA[, 2])
metrics_pca[2, 2] <- R2(dtest$R, predictionsPCA[, 2])

plot(predictionsPCA[,3], dtest$R)
metrics_pca[3, 1] <- rmse(dtest$R, predictionsPCA[, 3])
metrics_pca[3, 2] <- R2(dtest$R, predictionsPCA[, 3])

sum(metrics_windows_pca[,1,1])/length(sum(metrics_windows_pca[,1,1]))
sum(metrics_windows_pca[,1,2])/length(sum(metrics_windows_pca[,1,2]))

sum(metrics_windows_pca[,2,1])/length(sum(metrics_windows_pca[,2,1]))
sum(metrics_windows_pca[,2,2])/length(sum(metrics_windows_pca[,2,2]))

sum(metrics_windows_pca[,3,1])/length(sum(metrics_windows_pca[,3,1]))
sum(metrics_windows_pca[,3,2])/length(sum(metrics_windows_pca[,3,2]))

print(metrics_pca[1, ])
print(metrics_pca[2, ])
print(metrics_pca[3, ])

