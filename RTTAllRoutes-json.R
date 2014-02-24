library(RJSONIO)
library(RCurl)

rtt <- fromJSON(getURL('http://10.1.7.19/ps02/STRESSData/api/RTT/All'))
rtt_frame <- do.call(rbind.data.frame, rtt)