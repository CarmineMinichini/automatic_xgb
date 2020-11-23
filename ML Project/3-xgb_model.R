xgb_model <- function(data,best_tune)
{
  data <- data
  best_tune <- best_tune
  
  # split data in train/test
  trainIndex <- createDataPartition(data[,ncol(data)],p=0.75,list=FALSE)
  data_train <- data[trainIndex,] #training data (75% of data)
  data_test <-  data[-trainIndex,] # testing data (25% of data)
  
  # divide data & label
  #train
  data_train <- data_train[,1:ncol(data_train)-1]
  label_train <- data_train[,ncol(data_train)]
  # test
  data_test <- data_test[,1:ncol(data_test)-1]
  label_test <- data_test[,ncol(data_test)]

    # Transform data
  # train 
  data_train <- as.matrix(data_train)
  label_train <- as.numeric(label_train)
  # test
  data_test<- as.matrix(data_test)
  label_test <- as.numeric(label_test)
  
  cat(paste("",
            paste("Number of observations in train set:",nrow(data_train)),
            paste("Number of observations in test set:",nrow(data_test)),
            "___________________________________________________________________",
            "XGB TRAIN",
            "___________________________________________________________________",
            "Training XGB on train data:",
            " ",
            sep='\n\n'))
  
  # XGB matrix
  dtrain <-  xgb.DMatrix(as.matrix(data_train),label=label_train)
  dtest <- xgb.DMatrix(as.matrix(data_test),label=label_test)
  ########## XGB MODEL
  model <- xgboost(data= dtrain, 
                   objective = "binary:logistic",
                   # paramaters
                   max_depth = best_tune$max_depth,
                   nrounds=500,
                   colsample_bytree = best_tune$colsample_bytree,
                   gamma = best_tune$gamma,
                   min_child_weight = best_tune$min_child_weight,
                   eta = best_tune$eta, 
                   subsample = best_tune$subsample,
                   print_every_n = 50,
                   # others
                   verbose=1,
                   nthread = 4)
  
  cv  <-  xgb.cv(data = dtrain, 
                 nround = 100, 
                 print_every_n= 10,
                 verbose = TRUE,
                 metrics = list("rmse"),
                 nfold = 5, 
                 nthread = 4,
                 objective = "binary:logistic",
                 prediction=F)
  
  
  out <- list(dtest = dtest,
              data_train = data_train,
              label_test = label_test,
              model = model)
  
  return(out)
}