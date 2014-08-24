rankall <- function(outcome, num = "best") {
  outcomes <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=F, na.strings = c("NA" , "Not Available" ) )
  outcomeCol <- make.names(paste("Hospital.30.Day.Death..Mortality..Rates.from.",simpleCap(outcome),sep=""), unique=TRUE)
  
  outcomesForState <- outcomes[[outcomeCol]]
  if(length(outcomesForState) == 0){
    stop("invalid outcome")  
  }
  
  outcomes <- outcomes[order(outcomes[,"State"], outcomes[,outcomeCol], outcomes[,"Hospital.Name"]),]
  #the split contains unknown states - an improvement would be to remove them
  outcomeByState <- split(outcomes, outcomes$State)
  
  if(num == "best"){
    num <- 1
  }

  state <- numeric(length(outcomeByState))
  hospital <- character(length(outcomeByState))
  
  sapply(outcomeByState, function(outcomesForState){
    completeOutcomesForState <- outcomesForState[complete.cases(outcomesForState[,outcomeCol]),]
    if(num == "worst"){
      num <- nrow(completeOutcomesForState)
    }
    state <<- append(state, completeOutcomesForState[1, "State"])
    hospital <<- append(hospital, completeOutcomesForState[num, "Hospital.Name"])
   })

  #subset is a bit of a hack to account for some bogus values that worked thier way in to the vectors
  observations <- subset(data.frame(hospital, state), state != 0)
}

#http://stackoverflow.com/a/6364905/27657
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}