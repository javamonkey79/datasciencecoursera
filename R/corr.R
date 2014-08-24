corr <- function(directory, threshold = 0) {
  
  filenames <- list.files(directory, pattern="*.csv", full.names=TRUE)
  csv <- lapply(filenames, read.csv)
  
  correlations <- c()
  
  sapply(csv, function(x){
    complete <- sum(complete.cases(x))
    if(complete > 0 && complete > threshold){
      correlation <- cor(x$nitrate,x$sulfate,use="complete.obs")
      correlations <<- c(correlations, correlation)
    }
  })
  
  correlations  
}