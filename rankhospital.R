rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomeData <- read.csv(file = "outcome-of-care-measures.csv", colClasses = "character")
  possibleOutcomes <- c("heart attack", "heart failure", "pneumonia")
  ## Check that state and outcome are valid
  if(sum(outcomeData$State == state) == 0)
  {
    stop("invalid state")
  }
  if(sum(possibleOutcomes == outcome) == 0)
  {
    stop("invalid outcome")
  }
  if(outcome == possibleOutcomes[1])
  {
    field <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  }
  if(outcome == possibleOutcomes[2])
  {
    field <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  }
  if(outcome == possibleOutcomes[3])
  {
    field <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  suppressWarnings(rates <- as.numeric(outcomeData[outcomeData$State == state,field]))
  hospitals <- outcomeData[outcomeData$State == state,"Hospital.Name"]
  ratesClean <- rates[!is.na(rates)]
  lowestRate <- min(ratesClean)
  names <- hospitals[rates == lowestRate]
  namesResult <- names[!is.na(names)]
  namesResult
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}
