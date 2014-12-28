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
  suppressWarnings(outcomeData[,field] <- as.numeric(outcomeData[,field]))
  hospitals <- outcomeData[outcomeData$State == state & !is.na(outcomeData[field]),c("Hospital.Name",field,"State")]
  
  index <- order(hospitals[field], hospitals["Hospital.Name"])
  hospitalsOrdered <- hospitals[index,]
  maxNumber = nrow(hospitalsOrdered)
  if(num == "worst")
  {
    numIndex <- maxNumber
  }
  else if(num == "best")
  {
    numIndex <- 1
  }
  else
  {
    numIndex <- as.numeric(num)
  }
  
 #names <- hospitals[rates == compareRate]
 if(numIndex <= maxNumber)
 {
  namesResult <- hospitalsOrdered[numIndex,"Hospital.Name"]
  namesResult
 }
 else
 {
   NA
 }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}
