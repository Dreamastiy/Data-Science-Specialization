setwd('C:/Users/Dmi/Documents/R/Data/rprogdataProgAssignment3data')
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
# head(outcome)
# 
# outcome[, 11] <- as.numeric(outcome[, 11])
# ## You may get a warning about NAs being introduced; that is okay
# hist(outcome[, 11])

best <- function(state, outcome) {
     ## Read outcome data
     if (outcome == 'heart attack'){
          colNum <- 11          
     } else {
          if(outcome == 'heart failure')
          {
               colNum <- 17
          } else {
               if (outcome=='pneumonia'){
                    colNum <- 23
               }
               else{
                    
               }
          }
          
     }
     ## Check that state and outcome are valid
     ## Return hospital name in that state with lowest 30-day death
     ## rate
}