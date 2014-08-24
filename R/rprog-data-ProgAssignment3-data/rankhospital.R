rankhospital <- function(state, outcome, num = "best") {
  outcomes <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=F, na.strings = c("NA" , "Not Available" ) )
  outcomeByState <- split(outcomes, outcomes$State)
  outcomeState <- outcomeByState[[state]]
  if(length(outcomeState) == 0){
    stop("invalid state")  
  }
  
  outcomeCol <- make.names(paste("Hospital.30.Day.Death..Mortality..Rates.from.",simpleCap(outcome),sep=""), unique=TRUE)
  outcomesForState <- outcomeState[[outcomeCol]]
  if(length(outcomesForState) == 0){
    stop("invalid outcome")  
  }
  
  outcomeState <- outcomeState[order(outcomeState[,outcomeCol], outcomeState[,"Hospital.Name"]),]
  outcomeState <- outcomeState[complete.cases(outcomeState[,outcomeCol]),]
  
  if(num == "best"){
    num <- 1
  }
  if(num == "worst"){
    num <- nrow(outcomeState)
  }
  
  outcomeState[num,]$Hospital.Name

}

#http://stackoverflow.com/a/6364905/27657
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}