setwd('C:/Users/Dmi/Documents/R/Data/rprogdataProgAssignment3data')
# outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
# head(outcome)
# 
# outcome[, 11] <- as.numeric(outcome[, 11])
# ## You may get a warning about NAs being introduced; that is okay
# hist(outcome[, 11])
# best('AL', 'heart attack')

best <- function(state, outcome) {
     #setwd('C:/Users/Dmi/Documents/R/Data/rprogdataProgAssignment3data')
     oocm <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     ## Read outcome data
     if (outcome == 'heart attack'){
          colNum <- 11          
     } else {
          if(outcome == 'heart failure')
          {
               colNum <- 17
          } else {
               if (outcome =='pneumonia'){
                    colNum <- 23
               }
               else {
                    stop('invalid outcome')
               }
          }
          
     }
     
     outcomeName <- colnames(oocm)[colNum]
     
     if (state %in% oocm$State){
          
     } else {
          stop('invalid state')
     }
     
     smalloocm <- oocm[c('Hospital.Name',  'State')]
     temp <- as.numeric(oocm[,colNum])
     
     smalloocm <- cbind(smalloocm, Disease = temp)
     statedata <- smalloocm[smalloocm$State == state, ]
     minvalue <- min(statedata$Disease, na.rm=T)
     
     resultUnO <- statedata$Hospital.Name[(statedata$Disease == minvalue)&!(is.na(statedata$Disease))]
     resultO <- resultUnO[sort.list(resultUnO)]
#     statedata$Disease == minvalue
     
#     min <- min(smalloocm[(smalloocm$State == state)], na.rm=F)
#     smalloocm <- oocm[c('Hospital.Name', outcomeName,'State')]
#     smalloocm[2] <- as.numeric(smalloocm[2])
#     smalloocm <- smalloocm[!(outcomeName=='Not Available')]
     #smalloocm[outcomeName] <- as.numeric(smalloocm[outcomeName])
     
     ##tapply(as.numeric(smalloocm[outcomeName]), smalloocm$State, min)
     
     ## StateList <- split(oocm, oocm$State, oocm[outcomeName], drop=T)[state]
     ## Check that state and outcome are valid
     ## Return hospital name in that state with lowest 30-day death
     ## rate
}