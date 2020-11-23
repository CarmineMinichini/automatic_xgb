metrics <- function(model,dtest,label_test,data_train)
{
  # read data
  label_test <- label_test
  data_train <- data_train
  
  
  pred <- predict(model,dtest)
  prediction <- as.numeric(pred > 0.5)
  cm <- confusionMatrix(factor(prediction),factor(label_test),positive="1")
  
  cm_d <- as.data.frame(cm$table)
  # confusion matrix statistics as data.frame
  cm_st <-data.frame(cm$overall)
  # round the values
  cm_st$cm.overall <- round(cm_st$cm.overall,2)
  colnames(cm_st) <- ('Statistics')
  
  # here we also have the rounded percentage values
  cm_p <- as.data.frame(prop.table(cm$table))
  cm_d$Perc <- round(cm_p$Freq*100,2)
  
  # plotting the matrix
  cm_d_p <- ggplot(data = cm_d, aes(x = Prediction , y =  Reference, fill = Freq))+
    geom_tile() +
    geom_text(aes(label = paste("",Freq,":",Perc,"%")), color = 'white', size = 3) +
    theme_light() +
    scale_fill_gradient(low = "#3C3838", high = "#338076") + 
    scale_x_discrete(position = "top") +
    guides(fill=FALSE) 
  # plotting the stats
  cm_st_p <-  tableGrob(cm_st)
  
  # all together
  conf_mat = grid.arrange(cm_d_p, cm_st_p,nrow = 1, ncol = 2, 
                          top=textGrob("Confusion Matrix and Statistics",
                                       gp=gpar(fontsize=20,font=1)))
  cat(paste("___________________________________________________________________",
            'Features Importance:'," ",sep='\n\n'))
  
  # Features importance
  importance_matrix <- xgb.importance(colnames(data_train), model = model)
  #print(importance_matrix)
  
  
  out = list(confusion_matrix = cm_d,
             importance = importance_matrix)
  
  return(out)

}