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
