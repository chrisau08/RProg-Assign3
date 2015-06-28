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
  byState <- data.frame(hospital = character(),
                        state = character())
  
  ## Read outcome data
  data <- data.frame(read.csv("Data/outcome-of-care-measures.csv", colClasses = "character"))
  
  states <- sort(c(state.abb, "DC", "PR","VI", "GU"))
  
  ## Check that state and outcome are valid entries
  if(outcome %in% c("heart attack","heart failure","pneumonia")){
      ## Use IF statement to provide the appropriate outcome information.
      if(outcome == "heart attack"){        
        for(st in states){
          ## Subset the data to only include observations for the given state and heart attack
          HeartAttack <- data.frame(subset(data, data$State == st & data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available", select = c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")))
          
          ## Change the Mortality values to a numeric
          HeartAttack$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(HeartAttack$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
          
          ## Order data frame by Mortality then Hospital by Name
          ordHA <- HeartAttack[with(HeartAttack, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name)),]
          ordered <- cbind(ordHA, Rank = 1:nrow(ordHA))
          
          ## Return hospital name in that state with the given rank (ie. "best", 20, "worst")
          if(num == "best"){
            selection <- head(ordered$Hospital.Name, 1)
          }
          else{
            if(num == "worst"){
              selection <- tail(ordered$Hospital.Name, 1)
            }
            else{
              selection <- subset(ordered, ordered$Rank == num, select = "Hospital.Name")
              selection <- head(selection$Hospital.Name, 1)
            }
          }
          
          ## If number is provided and larger than the number of hospitals in a state then
          ## return NA for the state.
          if(length(selection) == 0){
            selection <- NA
          }
          
          byState <- rbind(byState, data.frame(hospital = selection, state = st))
        }
      }
      else{
        if(outcome == "heart failure"){        
          for(st in states){
            ## Subset the data to only include observations for the given state and heart attack
            HeartFailure <- data.frame(subset(data, data$State == st & data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available", select = c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")))
            
            ## Change the Mortality values to a numeric
            HeartFailure$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(HeartFailure$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
            
            ## Order data frame by Mortality then Hospital by Name
            ordHF <- HeartFailure[with(HeartFailure, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name)),]
            ordered <- cbind(ordHF, Rank = 1:nrow(ordHF))
            
            ## Return hospital name in that state with the given rank (ie. "best", 20, "worst")
            if(num == "best"){
              selection <- head(ordered$Hospital.Name, 1)
            }
            else{
              if(num == "worst"){
                selection <- tail(ordered$Hospital.Name, 1)
              }
              else{
                selection <- subset(ordered, ordered$Rank == num, select = "Hospital.Name")
                selection <- head(selection$Hospital.Name, 1)
              }
            }
            
            ## If number is provided and larger than the number of hospitals in a state then
            ## return NA for the state.
            if(length(selection) == 0){
              selection <- NA
            }
            
            byState <- rbind(byState, data.frame(hospital = selection, state = st))
          }
        }
        else{        
          for(st in states){
            ## Subset the data to only include observations for the given state and heart attack
            Pneumonia <- data.frame(subset(data, data$State == st & data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available", select = c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")))
            
            ## Change the Mortality values to a numeric
            Pneumonia$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(Pneumonia$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
            
            ## Order data frame by Mortality then Hospital by Name
            ordP <- Pneumonia[with(Pneumonia, order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name)),]
            ordered <- cbind(ordP, Rank = 1:nrow(ordP))
            
            ## Return hospital name in that state with the given rank (ie. "best", 20, "worst")
            if(num == "best"){
              selection <- head(ordered$Hospital.Name, 1)
            }
            else{
              if(num == "worst"){
                selection <- tail(ordered$Hospital.Name, 1)
              }
              else{
                selection <- subset(ordered, ordered$Rank == num, select = "Hospital.Name")
                selection <- head(selection$Hospital.Name, 1)
              }
            }
            
            ## If number is provided and larger than the number of hospitals in a state then
            ## return NA for the state.
            if(length(selection) == 0){
              selection <- NA
            }
            
            byState <- rbind(byState, data.frame(hospital = selection, state = st))
          }
        }
      }
    
  }
  else{
    ## If outcome is wrong the message should be "invalid outcome"
    stop("invalid outcome")
  }
  ## For each state, find the hospital of the given rank (ie. "best", 20, "worst")
  ## If number is provided and larger than the number of hospitals in a state then
  ## return NA for the state.
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  byState
}
