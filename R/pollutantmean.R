pollutantmean <- function(directory, pollutant, id = 1:332) {
  pollutants<-c()
  for(i in id){
    csvFileName<-sprintf("%s/%.3d.csv", directory, i)
    csv<-read.csv(csvFileName)
    pollutants<-c(pollutants, csv[[pollutant]])
  }
  round(mean(pollutants, na.rm=TRUE), 3)
}