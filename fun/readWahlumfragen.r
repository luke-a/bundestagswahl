readWahlumfragen <- function(since=as.Date("2013-08-15"), url="http://www.wahlumfragen.org/bundestagswahl/wahlumfragen_bundestagswahl.php", useLocalData=TRUE) {
  
  
  percStringsToNumeric <- function(x) {
    # convert strings to numerical values ('38,2 %' --> 38.2) 
    x[x=="N/A"] <- NA
    x <- as.numeric(sub("(\\d{1,2}),(\\d{1})\\s%", "\\1.\\2", x))
    return(x)
  }
  
  
  # if we want to pull fresh poll data from the web ... 
  if (!useLocalData) {
    # http://www.wahlumfragen.org/bundestagswahl/wahlumfragen_bundestagswahl.php
    url <- "file:///C:/Users/Rene/AppData/Roaming/Mozilla/Firefox/Profiles/vgaf5cd2.default/ScrapBook/data/20130924004429/index.html"
    doc = htmlParse(url, encoding = "UTF-8")
    tab <- readHTMLTable(doc, which=4, header=T)
    # correct encoding errors in header fields and rename at the same time
    names(tab) <- c("date", "cdu", "spd", "fdp", "lin", "gru", # VerÃ¶ffentlichung --> Veröffentlichung, ...
                    "pir", "afd", "son", "inst", "comment")
    # convert string to date
    tab$date <- as.Date(tab$date, format="%d.%m.%Y")
    
    # convert strings to numerical values ('38,2 %' --> 38.2) 
    tab[,2:9] <- apply(tab[,2:9], 2, FUN=percStringsToNumeric)
    # save to disk
    write.csv(tab, "data/wahlumfragen.csv", row.names=F)
    
    
    
    # else if we want to read older data from file ... 
  } else if (useLocalData) {
    tab <- read.csv("data/wahlumfragen.csv", colClasses=c("Date", rep("numeric",8), rep("factor", 2)))
  }  
  
  
  # select and order columns
  tab <- tab[,c("date", "inst", "cdu", "spd", "gru", "fdp", "lin", "pir", "afd", "son")]

  # select rows
  tab <- tab[tab$date >= since,]
  
  return(tab)
}