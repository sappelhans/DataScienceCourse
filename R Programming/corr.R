
corr <- function(directory, threshold = 0, id =1:332) {
  files <- paste(directory,"/",str_pad(id,3,side = "left","0"),".csv",sep = "")
  
   results <- numeric()  
   for (csv_file in files) {
    curData <- read.csv(csv_file)
    curData <- curData[complete.cases(curData),]
    if (nrow(curData) > threshold) {
      results <- c(results, cor(curData[,c("nitrate")],curData[,c("sulfate")]))  
    }
    
  }
  results
  
  ## Returns a numeric vector of correlations
}