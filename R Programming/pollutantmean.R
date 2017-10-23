library(stringr)

pollutantmean <- function(directory, pollutant, id = 1:332) {
  # Create list of filenames
  files <- paste(directory,"/",str_pad(id,3,side = "left","0"),".csv",sep = "")
  
  # Iterate over files and add to dataframe
  allData <- data.frame()
  for (csv_file in files) {
    curData <- read.csv(csv_file)
    allData <- rbind(allData,curData)
  }

  # Calc mean of selected pollutant
  mean(allData[[pollutant]], na.rm = TRUE)
}