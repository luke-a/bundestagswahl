readWahlrecht <- function(since=as.Date("2013-08-15")) { 

  # local data from http://www.wahlrecht.de/umfragen/index.htm
  filenames <- list.files(path="data", pattern="wahlrecht.*csv")
  polllist <- list()
  for (filename in filenames) {
    # extract "allensbach" from "wahlrecht.de_allensbach.csv"
    institut <- sub(".*?_(.*)\\.csv", "\\1", filename)
    # read data.frame into list
    polllist[[institut]] <- cbind(read.csv2(file.path("data", filename)), inst=institut)
  }
  
  
  percStringsToNumeric <- function(df) { 
    processColumn <- function(x) {
      # convert strings to numerical values ('38,2 %' --> 38.2) 
      x[x==""] <- NA
      # comma
      x <- sub(",", "\\.", x)
      # percentage symbol
      x <- sub("\\s?%", "", x)
      return(as.numeric(x))
    }
    # process only these columns
    columns <- c("cdu", "spd", "gru", "fdp", "lin", "pir", "son", "befragte")
    # 23,2% to 23.2
    df[,columns] <- apply(df[,columns], 2, FUN=processColumn)
    return(df)
  }
  
  select <- function(df) {
    # normalize first column name from 'X' or 'Datum' to 'date'
    names(df)[1] <- "date"
    # select rows, drop "zeitraum" etc.
    df <- df[,c("date", "inst", "CDU.CSU", "SPD", "GRÜNE", "FDP", "LINKE", "PIRATEN", "Sonstige", "Befragte")]
    # rename
    names(df) <- c("date", "inst", "cdu", "spd", "gru", "fdp", "lin", "pir", "son", "befragte")
    return(df)
  }
  
  # select and name relevant columns
  polllist <- lapply(polllist, FUN=select) 
  
  # convert string to numeric (23,2 % to 23.2)
  polllist <- lapply(polllist, FUN=percStringsToNumeric) 
  
  # convert list to data.frame and delete row.names
  df <- do.call(rbind, lapply(polllist, as.data.frame))
  row.names(df) <- NULL
  
  # order data.frame by date
  df$date <- as.Date(df$date, format="%d.%m.%Y")
  df <- df[order(df$date, decreasing=T),]
  
  
  
  # subset
  latestpolls <- df[df$date >= since,]
  
  # remove rows with all NA
  NArows <- which(apply(latestpolls, 1, function(x) all(is.na(x))))
  latestpolls <- latestpolls[-NArows,]
  return(latestpolls)

}