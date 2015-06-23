## Ranking Hospitals in All States.

## Input:
## Acceptable outcome values are "heart attack", "heart failure", and "pneumonia".
## Acceptable num values are "best", integer, and "worst".

## Evaluation:
## Hospitals with no data (NA) for an outcome are to be excluded.
## Ties in mortality rate should order alphabetically and return the first from that order.

## Source Data:
## The document, "outcome-of-care-measures.csv", provides the data.

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome <- data.frame(read.csv("Data/outcome-of-care-measures.csv", colClasses = "character"))
  
  ## Check that outcome is valid
  ## If outcome is wrong the message should be "invalid outcome"
  
  ## For each state, find the hospital of the given rank (ie. "best", 20, "worst")
  ## If number is provided and larger than the number of hospitals in a state then
  ## return NA for the state.
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}
