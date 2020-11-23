gridsearch <- function(data)
{
  source('1-dataframe_detector.R')
  check <- dataframe_detector(data=data)
  
  if(check==TRUE){
    # escludi la variabile target
    target_column <- data[,ncol(data)]
    # prendi tutte le colonne eccetto il target
    data <- data[,1:ncol(data)-1]
    # one hot encoding sulle variabili categoriche
    dmy <- dummyVars(" ~ .", data = data)
    train_data <- data.frame(predict(dmy, newdata = data))
    # ripristina il dataframe originario
    data <- cbind(train_data,target_column)
  }
  cat(paste(" ",
            "Data correctly encoded.",
            " ",
            "___________________________________________________________________",
            "",
            "GRID SEARCH RESULTS",
            "___________________________________________________________________",
            sep="\n"))
  ####################### GRID SEARCH
  # split dataframe in train part
  trainIndex <- createDataPartition(data[,ncol(data)],p=0.75,list=FALSE)
  data_train <- data[trainIndex,] #training data (75% of data)
  
  grid_train = data_train
  # rendere factor variabile target se non lo è
  grid_train[,ncol(grid_train)] = factor(grid_train[,ncol(grid_train)])
  # rilivellare la variabile target
  levels(grid_train[,ncol(grid_train)]) <- c("X0","X1")

  # grid parameters
  xgb_grid_1 = expand.grid(
    nrounds = 10,
    eta = seq(2,10,by=1)/10,
    max_depth = c(6, 8, 10),
    gamma = 0,
    subsample = c(0.5, 0.75, 1),
    min_child_weight = c(1,2) ,
    colsample_bytree = c(0.3,0.5)
  )
  
  # pack the training control parameters
  xgb_trcontrol_1 = trainControl(
    method = "cv",
    number = 2,
    search='grid',
    verboseIter = FALSE,
    returnData = TRUE,
    returnResamp = "all", # save losses across all models
    classProbs = FALSE, # set to TRUE for AUC to be computed
    summaryFunction = defaultSummary,
    allowParallel = TRUE,
  )
  
  xgb_train_1 = train(
    x = as.matrix(grid_train[,-ncol(grid_train)]),
    y = factor(grid_train[,ncol(grid_train)]),
    trControl = xgb_trcontrol_1,
    tuneGrid = xgb_grid_1,
    method = "xgbTree",
  )
  
  best_tune <- xgb_train_1$bestTune
  
  results <- xgb_train_1$results
  
  trained_model <- xgb_train_1

  cat(paste("",
            paste('With an accuracy of:',results[rownames(best_tune),"Accuracy"]),
            'Best GRIDSEARCH Hyperparameters:',
            '',
            sep='\n\n'))
  
  rownames(best_tune) <- 'Value'
  print(t(best_tune))
  
  # out dataframe
  out <- list(best_tune = best_tune,
              data = data)
}

prova <- gridsearch(data=data)





