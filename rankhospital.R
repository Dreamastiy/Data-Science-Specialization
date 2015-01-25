setwd('C:/Users/Dmi/Documents/R/Data/rprogdataProgAssignment3data/')

rankhospital <- function(state, outcome, num) {
     oocm <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
     statewodata <- statedata[!is.na(statedata$Disease),]
     statewodata <- statewodata[order(statewodata[,'Disease'], statewodata[,'Hospital.Name']),]   
     
     wrstrank <- length(statewodata$Disease)
     
     if(num == 'best'){
          rank <- 1          
     } else {
          if(num == 'worst'){
               rank <- wrstrank
          } else {
               if (is.numeric(num)){
                    if(num > wrstrank){
                         return(NA)
                    } else {
                         rank <- num
                    }
               }
               else {
                    stop("invalid rank!")
               }
                    
          }
     }
     
     statewodata$Hospital.Name[rank]
     
     
     
#     minvalue <- min(statedata$Disease, na.rm=T)
#      
#      resultUnO <- statedata$Hospital.Name[(statedata$Disease == minvalue)&!(is.na(statedata$Disease))]
#      resultO <- resultUnO[sort.list(resultUnO)]

}