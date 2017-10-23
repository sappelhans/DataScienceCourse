rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    prefix <- "Hospital.30.Day.Death..Mortality..Rates.from."
    condition <- switch(outcome,
                        "heart attack" = "Heart.Attack",
                        "heart failure" = "Heart.Failure",
                        "pneumonia" = "Pneumonia")
    outcomeColName <- paste(prefix,condition, sep = "")
    data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")

    ## Check that state and outcome are valid
    if ( !sum(data$State == state) > 0 ) {  
        stop("invalid state")
    }
    if ( !(outcomeColName %in% colnames(data)) ) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    stateData <- subset(data[,c("Hospital.Name","State",outcomeColName)],State==state)
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
    hosp
    
}