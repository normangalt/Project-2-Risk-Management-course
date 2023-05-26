library(tidymodels)
library(xgboost)
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

train_n <- as.integer(0.85*length(data$yyyymm))+1

dtrain <- data[1:train_n, ]
dtest <- data[-(1:train_n), ]

#Computing R column
R <- unlist(log(data[-(1:1), "Index"] +
                  data[-(1:1), "D12"]/12) - log(data[-(length(data):length(data)), "Index"], base=10))

data$R <- c(R, mean(R))

rtrain <- R[1:train_n-1]
rtest <- R[train_n:length(R)]
R2<-function(y,pred) {
  #R2=1-SSR/SST
  1-sum((y-pred)^2)/sum((y-mean(y))^2) 
}

rmse<-function(y,pred) {
  sqrt(mean((y-pred)^2)) 
}

boosted_tree_xg <- boost_tree(mode = "regression",
                              trees = 500,
                              learn_rate = tune(),
                              tree_depth = tune(),
                              sample_size = tune())

boosted_tree_xg <- set_engine(boosted_tree_xg, "xgboost")
tune_grid_boosting <- grid_random(parameters(boosted_tree_xg),
                                   size = 8)

dtrain_ <- dtrain[(1:length(dtrain$yyyymm)-1),]
dtrain_$R <- rtrain
resamples <- sliding_window(dtrain_)

dtrain <- subset(dtrain, select = c("ltr", "ntis", "svar", "infl",
                                "DRSPREAD", "D12", "bm", "tbl",
                                "DSPREAD", "E12",
                                "TSPREAD", "DE12"))

tree_training_xg <- data.frame(dtrain[(1:length(dtrain$ltr)-1),])
tree_training_xg$R <- rtrain
tuned_tree_xg <- tune_grid(boosted_tree_xg,
                           R~log(E12, base=10)**3 +
                             log(D12, base=10)**3 +
                             DE12**3 + svar**3 +
                             bm**3 + ntis**3 +
                             tbl**3 + ltr**3 +
                             TSPREAD**3 + DSPREAD**3 +
                             DRSPREAD**3 + infl**3,
                        grid = tune_grid_boosting,
                        resamples = resamples)

collect_metrics(tuned_tree_xg)
tuned_tree_xg
autoplot(tuned_tree_xg)

dtrain <- subset(dtrain, select = c("ltr", "ntis", "svar", "infl",
                                    "DRSPREAD", "D12", "bm", "tbl",
                                    "DSPREAD", "E12",
                                    "TSPREAD", "DE12"))

tree_training_xg <- data.frame(dtrain[(1:length(dtrain$ltr)-1),])
tree_training_xg$R <- rtrain

best_parameters_xg <- select_best(tuned_tree_xg)
final_model_xg <- finalize_model(boosted_tree_xg, best_parameters_xg)

xg_predictions <- array(dim=c(nrow(dtest)))
final_model_xg_fit <- fit(final_model_xg,
                          R~log(E12, base=10)**3 +
                            log(D12, base=10)**3 +
                            DE12**3 + svar**3 +
                            bm**3 + ntis**3 +
                            tbl**3 + ltr**3 +
                            TSPREAD**3 + DSPREAD**3 +
                            DRSPREAD**3 + infl**3,
                          data=tree_training_xg)

vip::vip(final_model_xg_fit)

plot(rtrain, unlist(predict(final_model_xg_fit, new_data=tree_training_xg)))

mean(final_model_xg_fit$fit$evaluation_log$training_rmse)

R2(tree_training_xg$R, predict(final_model_xg_fit, new_data = tree_training_xg))
sum((tree_training_xg$R - predict(final_model_xg_fit, new_data = tree_training_xg))^2)/length(tree_training_xg$ltr)

metrics_xg <- array(dim=c(nrow(dtest), 2))
for (index in 1:nrow(dtest)){
  xg_predictions[index] <- predict(final_model_xg_fit, new_data=dtest[index, ])
  
  
  start_<- index+1
  end_  <- train_n+index
  window <- data[(start_:end_), ]

  data[end_, "R"] <-xg_predictions[index]
  final_model_xg_fit <- fit(final_model_xg,
                            R~log(E12, base=10)**3 +
                              log(D12, base=10)**3 +
                              DE12**3 + svar**3 +
                              bm**3 + ntis**3 +
                              tbl**3 + ltr**3 +
                              TSPREAD**3 + DSPREAD**3 +
                              DRSPREAD**3 + infl**3,
                            data=window)

  rms<-sum((window$R - predict(final_model_xg_fit, new_data = window))^2)/length(window$ltr)
  r2 <- R2(window$R, predict(final_model_xg_fit, new_data = window))

  metrics_xg[index, 1] <- rms
  metrics_xg[index, 2] <- r2
}

vip::vip(final_model_xg_fit)

sum(metrics_xg[,1])/length(metrics_xg[,1])
sum(metrics_xg[,2])/length(metrics_xg[,2])

mean_rtest <- mean(rtest)
SST <- sum((rtest-mean_rtest)**2)
SSR <- sum((rtest - unlist(xg_predictions))**2)
rmse_tree <- sqrt(sum((rtest - unlist(xg_predictions))^2)/length(rtest))
R2_tree <- 1-(SSR)/(SST)
R2_tree
rmse_tree

plot(rtest, xg_predictions)
