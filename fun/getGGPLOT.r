getGGPLOT <- function() {
  latestpolls.long <- melt(latestpolls, id.vars=c("date", "inst"), measure.vars=names(latestpolls[,-(1:2)]), variable.name="partei", value.name="prognose")
  parteifarben <- c("#000000", "#D71F1D", "#0A8000", "#FFDD00", "#BE3075", "#FF8800")
  ergebnis <- data.frame(partei=levels(latestpolls.long$partei)[-(7:8)],
                         ergebnis=c(41.5, 25.7, 8.4, 4.8, 8.6, 2.2),
                         col=parteifarben)
  
  p <- ggplot(subset(latestpolls.long, !partei %in% c("son", "afd")), aes(x=date, y=prognose, colour=partei))
  p <- p + scale_y_continuous(breaks=seq(0,45,5), minor_breaks=seq(0,45,1)) 
  p <- p + scale_colour_manual(values=parteifarben)
  p <- p + geom_point(size=3)
  # p <- p + geom_smooth()
  for (x in 1:nrow(ergebnis)) p <- p + geom_hline(yintercept=ergebnis$ergebnis[x], colour=parteifarben[x])
  p <- p + facet_wrap(~inst, ncol=5) + theme_bw() + theme(panel.grid.minor.y=element_line(colour="lightgray"))
  p <- p + labs(x="Umfrage-Datum", 
                y="Prognose", 
                title="Letzte Wahlprognosen nach Instituten (Punkte) und Ergebnisse der Bundestagswahl 2013 (Linien)   |   Daten: vis4.net, bit.ly/wahlprognosen",
                colour="Parteikürzel")
  return(p)
}