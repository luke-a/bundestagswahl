require(XML)
require(ggplot2)
require(reshape2)

# READ latest data before the election, print head
{
  # read CSV, get a subset of the latest entries 
  # (polls since date x or latest x polls)
  
  # 
  # # vis4.net
  # source(file.path("fun", "readData.r"))
  # latestpolls <- readPollData(20)
  # head(latestpolls, 15)
  # 
  # # wahlumfragen.org
  # source(file.path("fun", "readWahlumfragen.r"))
  # latestpolls <- readWahlumfragen(since="2013-08-15")
  # head(latestpolls, 15)
  
  
  # Wahlrecht.de
  source(file.path("fun", "readWahlrecht.de.r"))
  latestpolls <- readWahlrecht(since="2013-08-15")
  head(latestpolls, 15)
}

# PLOT all institutes in in graph
{
  # function to create plot
  source(file.path("fun", "plotPrognosen.r"))
  
  # unique institute names
  institutes <- levels(latestpolls$inst)
  
  # plot
  windows(1920/90, 1080/90)
  plotPrognosen(latestpolls, drawCI=F, main=sprintf("Punkte: Prognosen zur Bundestagswahl 2013 ( 6 Parteien, %s Umfrageinstitute)  |  Linien: Endergebnis", length(institutes)))
}

# PLOTS per poll institute
{

  # function to create plot
  source(file.path("fun", "plotPrognosen.r"))
  
  # unique institute names
  institutes <- levels(latestpolls$inst)
  
  windows(1920/90, 1080/90, title="Ohne CIs")
  layout(matrix(c(1,2,3,4,  
                  5,6,7,8), byrow=T, ncol=4, nrow=2))
  for (institute in institutes) {
    pollstoplot <- latestpolls[latestpolls$inst==institute,]
    plotPrognosen(pollstoplot, drawCI=F, main=paste0(institute, " (n ~ ", round(mean(pollstoplot$befragte, na.rm=T)), ")"))
  }
  
  
  windows(1920/90, 1080/90, title="Mit CIs")
  layout(matrix(c(1,2,3,4,  
                  5,6,7,8), byrow=T, ncol=4, nrow=2))
  for (institute in institutes) {
    pollstoplot <- latestpolls[latestpolls$inst==institute,]
    plotPrognosen(pollstoplot, main=paste0(institute, " (n ~ ", round(mean(pollstoplot$befragte, na.rm=T)), ")"))
  }


}
