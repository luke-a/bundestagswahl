
readPollData <- function(since=as.Date("2013-08-15"), datadirName="data", fileName="prognosen.csv") {
  
  readPollCSV <- function() {
    df <- read.table(file.path(datadirName,fileName), 
                     sep="\t", 
                     header=T, 
                     encoding="UTF-8", 
                     colClasses=c("Date", "factor", rep("numeric", 8)))
  }
  
  getLatestPolls <- function() {
    if (class(since)=="Date")
      df[df$date >= since,]
    else if (is.numeric(since))
      head(df, since)
  }
  
  df <- readPollCSV()
  latestpolls <- getLatestPolls() 
  return (latestpolls)
  
}