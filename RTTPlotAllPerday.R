library(ggplot2)
library(plyr)

manadNames <- c("Januari","Februari","Mars","April","Maj","Juni","Juli","Augusti","September","Oktober","November","December")

ttPlotRiddag<-function(rid,dag,manad,desc){
  tt <- rttPerday_frame[rttPerday_frame$Ruttid %in% rid & rttPerday_frame[,4] == dag & rttPerday_frame[,5] == manad,]
  
  print(
    ggplot(
      tt,
      aes(x=Tid15,
          y=Hastighet,
          colour=factor(Trängselrutt), group=Trängselrutt)
    )
    + geom_line()
    + geom_point()
    + scale_colour_discrete(name="Delrutter")
    + ylab("Medelhastighet kmh")
    + xlab("Klockslag")
    + ggtitle(paste(paste(paste(paste(paste("Medelhastighet per delrutt för ", desc), " "), dag), " "), manadNames[as.numeric(as.character(manad))]))
  )
}

ruttBredKista <- c(200,201,202,203,204,205,206,207,208,90007,90008,90009,90010,90011,90012,90013)
ruttKistaNorrt <- c(90000,90001,90002,90003,90004,90005,90006)
ruttLugnNyb <- c(217,218,219,220)

iter <- unique(rttPerday_frame[,c("Månad","Dag")])
iter2 <- unique(rttPerday_frame[rttPerday_frame$Månad %in% c(3,4,5,6,7,8,9,10,11,12) & rttPerday_frame$Ruttid %in% c(90000,90001,90002,90003,90004,90005,90006),][, c("Månad","Dag")])


pdf(file="Bredäng-Kista 2013.pdf", width=10, paper="a4r")
for (i in 1:nrow(iter)){ ttPlotRiddag(ruttBredKista, iter[i,2],iter[i,1],"Bredäng - Kista")}
dev.off()

pdf(file="Kista-Norrtull 2013.pdf", width=10, paper="a4r")
for (i in 1:nrow(iter2)){ ttPlotRiddag(ruttKistaNorrt, iter2[i,2],iter2[i,1],"Kista - Norrtull")}
dev.off()

pdf(file="Lugnet-Nyboda 2013.pdf", width=10, paper="a4r")
for (i in 1:nrow(iter)){ ttPlotRiddag(ruttLugnNyb, iter[i,2],iter[i,1],"Lugnet - Nyboda")}
dev.off()


