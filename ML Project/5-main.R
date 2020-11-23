machine_train <- function(dataset)
{ 
  library(tidyverse)
  library(caret)
  library(xgboost)
  library(ROSE)
  library(pROC)
  library(Ckmeans.1d.dp)
  library(ggplot2)     # to plot
  library(gridExtra)   # to put more
  library(grid) 
  
  source('2-gridsearch.R')
  source('3-xgb_model.R')
  source('4-metrics.R')
  # grid search
  grid_results <- gridsearch(data=data)
  # results of grid search
  data <- grid_results$data
  best_tune <- grid_results$best_tune
  # XGB train
  xgb_results <- xgb_model(data=data,best_tune=best_tune)
  
  # XGB results
  model <- xgb_results$model
  dtest <- xgb_results$dtest
  label_test <- xgb_results$label_test
  data_train <- xgb_results$data_train
  
  # Metrics
  metrics_results <- metrics(model,dtest,label_test,data_train)
  # print feature importance
  print(metrics_results$importance)

}

