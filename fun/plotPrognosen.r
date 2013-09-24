plotPrognosen <- function(latestpolls, drawGuides=F, drawCI=T, ...) {
  kuerzel <- c("cdu", "spd", "gru", "fdp", "lin", "pir", "son")
  parteifarben <- c(cdu="#000000", spd="#D71F1D", gru="#0A8000", fdp="#FFDD00", lin="#BE3075", pir="#FF8800")
  stimmanteile <- c(cdu.csu=34.1+7.4, spd=25.7, gru=8.4, fdp=4.8, lin=8.6, pir=2.2)
  waehler <- 44289652 # http://www.bundeswahlleiter.de/de/bundestagswahlen/BTW_BUND_13/ergebnisse/bundesergebnisse/index.html
  schätzfehler <- t(apply(latestpolls, 1, function(x) {  1.96*sqrt(  as.numeric(x[3:9])/100*(1-as.numeric(x[3:9]) /100) / as.numeric(x[10]) * sqrt((waehler-as.numeric(x[10]))/(waehler-1)) )*100 } ))
  
  
  # ylimrange <- c(min(as.matrix(latestpolls[,kuerzel]), na.rm=T), max(as.matrix(latestpolls[,kuerzel]), na.rm=T))
  ylimrange <- c(0,43)
  
  
  plot(cdu~date, data=latestpolls, type="n", ylim=ylimrange, xlab="Zeit", ylab="Prognosen", ...)
  
  
  usr <- par("usr"); polygon(x=c(rep(usr[1],2),rep(usr[2],2)), y=c(usr[3],5,5,usr[3]), border=NULL, col="gray") # "basement" < 5% hurdle
  if (drawGuides) {
    abline(h=seq(ylimrange[1], ylimrange[2], 2.5), col="lightgray", lty="dotted", lwd=.8) # minor-y-guides
    abline(h=seq(0, ylimrange[2], 5), col="lightgray", lty=1, lwd=1.5) # major-y-guides
  }
  axis(side=2, at = 5, label = "5", col.axis = "black", las=3, cex.axis = 1, font = 2, lwd=2) # annotate 5% hurdle
  box() # redraw to put it on top
  
  parteifarbe <- c(NA, NA, parteifarben)
  for (x in 1:nrow(latestpolls)) 
    for (y in 3:8) {
      points(x=latestpolls[x,1], y=latestpolls[x,y], pch=19, col=parteifarbe[y])
      if (drawCI) {
        arrows(x0=latestpolls[x,1], 
             y0=latestpolls[x,y],
             y1=latestpolls[x,y]+schätzfehler[x,y-2], col=parteifarbe[y],
             length=.05, angle=90)
        arrows(x0=latestpolls[x,1], 
               y0=latestpolls[x,y],
               y1=latestpolls[x,y]-schätzfehler[x,y-2], col=parteifarbe[y],
               length=.05, angle=90)
      }
    }
  
  
  abline(h=stimmanteile, col=parteifarben)
  
  
}