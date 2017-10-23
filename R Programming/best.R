best <- function(state, outcome) {
    ## Read outcome data
    prefix <- "Hospital.30.Day.Death..Mortality..Rates.from."
    condition <- switch(outcome,
           "heart attack" = "Heart.Attack",
           "heart failure" = "Heart.Failure",
           "pneumonia" = "Pneumonia")
    outcomeColName <- paste(prefix,condition, sep = "")
    data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
    stateData <- subset(data,State==state)
    
    
    ## Check that state and outcome are valid
    if ( !nrow(stateData) > 0 ) {  
        stop("invalid state")
    }
    if ( !(outcomeColName %in% colnames(stateData)) ) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest 30-day death rate
    suppressWarnings( stateData[,outcomeColName] <- as.numeric(stateData[,outcomeColName]) )
    best <- stateData[which.min(stateData[,outcomeColName]),"Hospital.Name"]
    best
}