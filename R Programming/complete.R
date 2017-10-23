
complete <- function(directory, id = 1:332) {
  files <- paste(directory,"/",str_pad(id,3,side = "left","0"),".csv",sep = "")
  idFiles <- data.frame(id,files,stringsAsFactors = FALSE)
  
  nobsData <- data.frame()
  #colnames(nobsData) <- c("id","nobs")
  
  for (i in 1:nrow(idFiles) ) {
    curData <- read.csv(idFiles[i,2])
    newrow <- data.frame(id=idFiles[i,1],nobs=sum(complete.cases(curData)))
    nobsData <- rbind(nobsData,newrow)
  }
  nobsData
  
  ## Returns data frame:
  ## id nobs
  ## 1  117
  ## 2  1041
  ##
  ## Where nobs is the number of complete cases
}
