## Best Hospital in a State

## Input:
## States are the 2-character format.
## Acceptable outcome values are "heart attack", "heart failure", and "pneumonia".

## Evaluation:
## Hospitals with no data (NA) for an outcome are to be excluded.
## Ties in mortality rate should order alphabetically and return the first from that order.

## Source Data:
## The document, "outcome-of-care-measures.csv", provides the data.

best <- function(state, outcome) {
  ## Read outcome data
  outcome <- data.frame(read.csv("Data/outcome-of-care-measures.csv", colClasses = "character"))
  
  ## Check that state and outcome are valid entries
  ## If either is invalid then throw an error using the "stop" method.
  ## If state is wrong the message should be "invalid state"
  ## If outcome is wrong the message should be "invalid outcome"
  
  ## Return hospital name in that state with lowest 30-day death rate
}