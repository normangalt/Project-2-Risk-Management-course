library(parsnip)
library(tune)
library(readxl)
library(dials)
library(rpart)
library(rsample)

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
R <- unlist(log(data[-(1:1), "Index"] +
                  data[-(1:1), "D12"]/12) - log(data[-(length(data):length(data)), "Index"], base=10))

train_n <- as.integer(0.85*length(data$yyyymm))+1

data$R <- c(R, mean(R))
dtrain <- data[1:train_n, ]
dtest <- data[-(1:train_n), ]

#Computing R column

rtrain <- R[1:train_n-1]
rtest <- R[train_n:length(R)]

R2<-function(y,pred) {
  #R2=1-SSR/SST
  1-sum((y-pred)^2)/sum((y-mean(y))^2) 
}

rmse<-function(y,pred) {
  sqrt(mean((y-pred)^2)) 
}
s <- function(x){
  x**3
}

randomForest <- rand_forest(mtry = sqrt(as.integer(length(data) -2)),
                            trees = tune(),
                            min_n=tune(),
                            mode= "regression",
                            engine = "ranger")
randomForest <- set_engine(randomForest, "ranger", importance = "impurity")

resamples <- sliding_window(dtrain[(1:length(dtrain$yyyymm)-1),])
tree_grid <- grid_random(parameters(randomForest), grid_size = 8)
grid_tuning <- tune_grid(randomForest, 
                         R~log(E12, base=10)**3 +
                           log(D12, base=10)**3 +
                           DE12**3 + svar**3 +
                           bm**3 + ntis**3 +
                           tbl**3 + ltr**3 +
                           TSPREAD**3 + DSPREAD**3 +
                           DRSPREAD**3 + infl**3,
                         resamples=resamples,
                         grid=tree_grid)

collect_metrics(grid_tuning)
autoplot(grid_tuning)

best_parameters_rf <- select_best(grid_tuning)
final_rf <- finalize_model(randomForest, best_parameters_rf)

tree_fit <- fit(final_rf, R~log(E12, base=10)**3 +
                  log(D12, base=10)**3 +
                  DE12**3 + svar**3 +
                  bm**3 + ntis**3 +
                  tbl**3 + ltr**3 +
                  TSPREAD**3 + DSPREAD**3 +
                  DRSPREAD**3 + infl**3,
                data=dtrain[(1:length(dtrain$yyyymm)-1),])
vip::vip(tree_fit)
tree_fit

plot(unlist(predict(tree_fit, new_data=dtrain)), unlist(dtrain$R))

R2(dtrain[(1:length(dtrain$yyyymm)-1),]$R, predict(tree_fit, new_data = dtrain[(1:length(dtrain$yyyymm)-1),]))
sum((dtrain[(1:length(dtrain$yyyymm)-1),]$R - predict(tree_fit, new_data = dtrain[(1:length(dtrain$yyyymm)-1),]))^2)/length(window$ltr)

tree_predictions <- array(nrow(dtest))
metrics_rf <- array(dim = c(nrow(dtest), 2))
for (index in 1:nrow(dtest)){
  tree_predictions[index] <- predict(tree_fit, new_data=dtest[index, ])
  
  start_<- index+1
  end_  <- train_n+index
  window <- data[(start_:end_), ]
  
  data[end_, "R"] <-tree_predictions[index]
  tree_fit <- fit(final_rf,
                  R~log(E12, base=10)**3 +
                    log(D12, base=10)**3 +
                    DE12**3 + svar**3 +
                    bm**3 + ntis**3 +
                    tbl**3 + ltr**3 +
                    TSPREAD**3 + DSPREAD**3 +
                    DRSPREAD**3 + infl**3,
                  data=window)
  
  rms<- sum((window$R - predict(tree_fit, new_data = window))^2)/length(window$ltr)
  r2 <- R2(window$R, predict(tree_fit, new_data = window))
  
  metrics_rf[index, 1] <- rms
  metrics_rf[index, 2] <- r2
}

sum(metrics_rf[,1])/length(metrics_rf[,1])
sum(metrics_rf[,2])/length(metrics_rf[,2])

mean_rtest <- mean(rtest)
SST <- sum((rtest-mean_rtest)**2)
SSR <- sum((rtest - unlist(tree_predictions))**2)
rmse_tree <- sqrt(sum((rtest - unlist(tree_predictions))^2)/length(rtest))
R2_tree <- 1-(SSR)/(SST)
R2_tree
rmse_tree

plot(rtest, tree_predictions)
plot(rtest, data[399:468,]$R)
plot(rtest, data[329:398,]$R)
