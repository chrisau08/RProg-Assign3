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
  first <- NULL
  
  ## Read outcome data
  data <- data.frame(read.csv("Data/outcome-of-care-measures.csv", colClasses = "character"))
  
  ## Check that state and outcome are valid entries
  if(outcome %in% c("heart attack","heart failure","pneumonia")){
    if(state %in% state.abb){
      ## Use IF statement to provide the appropriate outcome information.
      if(outcome == "heart attack"){        
        ## Subset the data to only include observations for the given state and heart attack
        HeartAttack <- data.frame(subset(data, data$State == state & data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available", select = c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")))
        
        ## Change the Mortality values to a numeric
        HeartAttack$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(HeartAttack$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
        
        ## Order data frame by Mortality then Hospital by Name
        ordHA <- HeartAttack[with(HeartAttack, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name)),]
        
        ## Take the first record which has the lowest Mortality rate
        first <- head(ordHA$Hospital.Name, 1)
      }
      else{
        if(outcome == "heart failure"){
          ## Subset the data to only include observations for the given state and heart failure
          HeartFailure <- data.frame(subset(data, data$State == state & data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available", select = c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")))
          
          ## Change the Mortality values to a numeric
          HeartFailure$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(HeartFailure$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
          
          ## Order data frame by Mortality then Hospital by Name
          ordHF <- HeartFailure[with(HeartFailure, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name)),]
          
          ## Take the first record which has the lowest Mortality rate
          first <- head(ordHF$Hospital.Name, 1)
        }
        else{
          ## Subset the data to only include observations for the given state and pneumonia
          Pneumonia <- data.frame(subset(data, data$State == state & data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available", select = c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")))
          
          ## Change the Mortality values to a numeric
          Pneumonia$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(Pneumonia$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
          
          ## Order data frame by Mortality then Hospital by Name
          ordP <- Pneumonia[with(Pneumonia, order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name)),]
          
          ## Take the first record which has the lowest Mortality rate
          first <- head(ordP$Hospital.Name, 1)
        }
      }
    }
    else{
      ## If state is wrong the message should be "invalid state"
      stop("invalid state")
    }
  }
  else{
    ## If outcome is wrong the message should be "invalid outcome"
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  first
}