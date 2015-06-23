## Write a function called "best" that takes two arguments: the 2-character state and an outcome name.
## The function is to read from "outcome-of-care-measures.csv" returning a character vector with the
## hospital name that has the best (lowest) 30-day mortality rate.
## Sample outcomes are "heart attack", "heart failure", and "pneumonia".
## Hospitals with no data (NA) for an outcome are to be excluded.
## Ties in mortality rate should order alphabetically and return the first from that order.

best <- function(state, outcome) {
  ## Read outcome data
  
  ## Check that state and outcome are valid entries
  ## If either is invalid then throw an error using the "stop" method.
  ## If state is wrong the message should be "invalid state"
  ## If outcome is wrong the message should be "invalid outcome"
  
  ## Return hospital name in that state with lowest 30-day death rate
}