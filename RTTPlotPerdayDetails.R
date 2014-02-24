library(ggplot2)
library(plyr)

manadNames <- c("Januari","Februari","Mars","April","Maj","Juni","Juli","Augusti","September","Oktober","November","December")

ttPlotRidManadPerdag<-function(rid,manad){
  tt <- rttPerday_frame[rttPerday_frame$Ruttid == rid & rttPerday_frame[,5] == manad & rttPerday_frame[,7] != 0,]
  if (nrow(tt) != 0) {
  print(
    ggplot(
      tt,
      aes(x=Tid15,
          y=as.numeric(as.character(Hastighet)),
          colour=factor(Dag), group=Dag)
    )
    + geom_line()
    + ylim(0,max(as.numeric(as.character(tt$Hastighet))))
    + geom_point()
    + scale_colour_discrete(name="Dagar i månaden", limits = 
                              ddply(rttPerday_frame[rttPerday_frame$Ruttid == rid & rttPerday_frame$Månad == manad,], 
                                    .(dag = as.numeric(as.character(Dag))), 
                                    summarize, mean = mean(as.numeric(as.character(Dag))))$dag)
    + ylab("Medelhastighet kmh")
    + xlab("Klockslag")
    + ggtitle(paste(paste(paste("Medelhastighet för delrutt ", tt$Trängselrutt), " i "), manadNames[as.numeric(as.character(manad))]))
  )
  }
}

ruttBredKista <- c(200,201,202,203,204,205,206,207,208,90007,90008,90009,90010,90011,90012,90013)
ruttKistaNorrt <- c(90000,90001,90002,90003,90004,90005,90006)
ruttLugnNyb <- c(217,218,219,220)
blipManader <- c(3,4,5,6,7,8,9,10,11,12)

niter1 <- unique(rttPerday_frame[rttPerday_frame$Ruttid %in% ruttBredKista, c("Ruttid","Månad")])
niter2 <- unique(rttPerday_frame[rttPerday_frame$Ruttid %in% ruttKistaNorrt & rttPerday_frame$Månad %in% blipManader, c("Ruttid","Månad")])
niter3 <- unique(rttPerday_frame[rttPerday_frame$Ruttid %in% ruttLugnNyb, c("Ruttid","Månad")])

pdf(file="Bredäng-Kista detalj 2013.pdf", width=10, paper="a4r")
for (i in 1:nrow(niter1)){ ttPlotRidManadPerdag(niter1[i,1],niter1[i,2])}
dev.off()

pdf(file="Kista-Norrtull detalj 2013.pdf", width=10, paper="a4r")
for (i in 1:nrow(niter2)){ ttPlotRidManadPerdag(niter2[i,1],niter2[i,2])}
dev.off()

pdf(file="Lugnet-Nyboda detalj 2013.pdf", width=10, paper="a4r")
for (i in 1:nrow(niter3)){ ttPlotRidManadPerdag(niter3[i,1],niter3[i,2])}
dev.off()
