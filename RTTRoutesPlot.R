library(ggplot2)
library(plyr)

manadNames <- c("Januari","Februari","Mars","April","Maj","Juni","Juli","Augusti","September","Oktober","November","December")

ttPlotRid<-function(rid){
  tt <- rtt_frame[rtt_frame$routeid == rid,]
  
  print(
    ggplot(
      tt,
      aes(x=tid15,
          y=avgV15,
          linetype=factor(manad), colour=factor(manad), group=manad)
    )
    + geom_line()
    + geom_point()
  )
}

ttPlotRidmanad<-function(rid,manad){
  tt <- rtt_frame[rtt_frame$routeid %in% rid & rtt_frame$manad == manad,]
  
  print(
    ggplot(
      tt,
      aes(x=tid15,
          y=avgV15,
          colour=factor(ruttDesc), group=ruttDesc)
    )
    + geom_line()
    + geom_point()
    + scale_colour_discrete(name="Delrutter")
    + ylab("Medelhastighet kmh")
    + xlab("Klockslag")
    + ggtitle(paste("Medelhastighet per delrutt för Kista - Norrtull i ", manadNames[manad]))
  )
}

ttPlotRiddag<-function(rid,dag,manad){
  tt <- rttPerday_frame[rttPerday_frame$Ruttid %in% rid & rttPerday_frame$Dag == dag & rttPerday_frame$Månad == manad,]
  
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
    + ggtitle(paste(paste(paste("Medelhastighet per delrutt för Kista - Norrtull ", dag), " ",), manadNames[manad]))
  )
}

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

ttPlotRidManadPerdag<-function(rid,manad){
  tt <- rttPerday_frame[rttPerday_frame$Ruttid == rid & rttPerday_frame[,5] == manad,]
  
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

ttPlotRidManadPerdagSmooth<-function(rid,manad){
  tt <- rttPerday_frame[rttPerday_frame$Ruttid == rid & rttPerday_frame[,5] == manad,]
  
  print(
    ggplot(
      tt,
      aes(x=Tid15,
          y=as.numeric(as.character(Hastighet)),
          colour=factor(Dag), group=Dag)
    )
    + geom_smooth(method="loess", level=0)
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

#max(as.numeric(as.character(tt$Hastighet)))