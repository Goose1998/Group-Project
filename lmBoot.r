library(parallel)
library(profvis)
ncores <- detectCores()-1
myClust <- makeCluster(ncores-1, type = "PSOCK")



lmBoot <- function(inputData,y,ycol, nboot){
  
  col_names <- colnames(inputData)
  col_names <- col_names[-ycol]
  bootResults<- matrix(NA,nrow =(length(col_names)+1),ncol = nboot)
  colnames(bootResults) <- c("intercept",col_names)
    
  getCoefs <- function(i){
    
    # resample our data with replacement
    bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
    
    
    # fit the model under this alternative reality
    bootLM <- lm(paste(paste(deparse(substitute(y))," ~",sep = ""),paste(col_names,collapse = " + "),collapse = " "), data = bootData)
    
    # store the coefs
    bootResults[,i] <- coef(bootLM)
  }
  
    bootCoefs <- foreach(i=1:nboot) %dopar% getCoefs(i)

   # end of i loop
  
  return(bootResults)
  
}
# testing
#testing2

# https://www.math.ucla.edu/~anderson/rw1001/library/base/html/paste.html
# https://www.dummies.com/programming/r/how-to-name-matrix-rows-and-columns-in-r/