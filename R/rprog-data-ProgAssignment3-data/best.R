best <- function(state, outcome) {
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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

  outcomeState[suppressWarnings(which.min(outcomesForState)),"Hospital.Name"]
  
}

#http://stackoverflow.com/a/6364905/27657
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}