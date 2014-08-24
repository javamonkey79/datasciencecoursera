complete <- function(directory, id = 1:332) {
  result<-sapply(id, function(i){
    csvFileName<-sprintf("%s/%.3d.csv", directory, i)
    csv<-read.csv(csvFileName)
    
    #     c(i,sum(lapply(csv[,2,3], function(x){!is.na(x)})))
    c(i,sum(complete.cases(csv)))  
  })
  
  id = result[1,]
  nobs = result[2,]
  data.frame(id, nobs)
}