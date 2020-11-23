dataframe_detector <- function(data)
{
  for(col in 1:ncol(data)){
    check <- is.factor(data[,col])
  }
  if(any(check==TRUE)){
    cat(paste("There is at least one factor column in dataframe.",
              "Those columns are identified as factor columns:",
              '',
              sep="\n\n"))
    print(data %>% Filter(f = is.factor) %>% names)
  }
  else{
    print('There aren\'t factor columns in dataframe')
  }
  return(check)
}
