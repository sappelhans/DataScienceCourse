rankall <- function(outcome, num = "best") {
    
    ## Read outcome data
    prefix <- "Hospital.30.Day.Death..Mortality..Rates.from."
    condition <- switch(outcome,
                        "heart attack" = "Heart.Attack",
                        "heart failure" = "Heart.Failure",
                        "pneumonia" = "Pneumonia")
    outcomeColName <- paste(prefix,condition, sep = "")
    data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
    
    ## Check that the outcome is valid
    if ( !(outcomeColName %in% colnames(data)) ) {
        stop("invalid outcome")
    }

    ## For each state, find the hospital of the given rank
    ##states <- data.frame(state=datasets::state.abb)
    rankall <- data.frame()
    states <- sort(unique(data$State))
    for (curState in states) {
        stateData <- subset(data[,c("Hospital.Name","State",outcomeColName)],State==curState)
        suppressWarnings( stateData[,outcomeColName] <- as.numeric(stateData[,outcomeColName]) )
        stateData <- stateData[complete.cases(stateData[,outcomeColName]),]
        hosp.scores <- order(stateData[,outcomeColName],stateData[,"Hospital.Name"])
        stateDataRanked <- stateData[hosp.scores,]
        
        if (num == "best") {
            hosp <- stateDataRanked[1,"Hospital.Name"]
        } else if (num == "worst") {
            hosp <- stateDataRanked[nrow(stateDataRanked),"Hospital.Name"]
        } else {
            hosp <- stateDataRanked[num,"Hospital.Name"]
        }
        newrow <- data.frame(hospital=hosp,state=curState)
        rankall <- rbind(rankall,newrow)
 
    }
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    rownames(rankall) <- rankall$state
    rankall
}