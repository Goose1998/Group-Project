lmBoot <- function(inputData,y,ycol, nboot){
  
  col_names <- colnames(inputData)
  col_names <- col_names[-ycol]
    
  
  for(i in 1:nboot){
    
    # resample our data with replacement
    bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
    bootResults<- matrix(NA,nrow =(length(col_names)+1),ncol = nboot)
    
    # fit the model under this alternative reality
    bootLM <- lm(paste(paste(deparse(substitute(y))," ~",sep = ""),paste(col_names,collapse = " + "),collapse = " "), data = bootData)
    
    # store the coefs
    bootResults[,i] <- coef(bootLM)
    

  } # end of i loop
  
  return(bootResults)
  
}
# testing
#testing2

# https://www.math.ucla.edu/~anderson/rw1001/library/base/html/paste.html