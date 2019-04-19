trainPlot<-function(train, peakdist=225){
  twitch<-train$twitch
  peaks<-getPeaks(twitch, peakdist, plot=FALSE)
  peakset<-peaks$peaks
  valleyset<-peaks$valleys
  
  npeaks<-nrow(peakset)
  
  valley0<-valleyset[1:npeaks-1,2]
  valleyf<-valleyset[2:npeaks,2]
  valleyspan<-valleyf-valley0
  blspan<-quantile(valleyspan, probs=.8)
  
  bl1.start<-valley0[1]-blspan*2
  bl1.start[bl1.start<1]<-1
  bl1.end<-bl1.start+blspan
  
  bl2.start<-max(peakset[,2])+blspan*2
  bl2.end<-bl2.start+blspan
  
  bl1<-mean(twitch[bl1.start:bl1.end])
  bl2<-mean(twitch[bl2.start:bl2.end])
  
  blcol<-rgb(225/255,226/255,96/255,.5)
  xleft<-c(bl1.start, bl2.start)
  xright<-c(bl1.end, bl2.end)
  ytop<-c(max(twitch[bl1.start:bl1.end]), max(twitch[bl2.start:bl2.end]))
  ybottom<-c(min(twitch[bl1.start:bl1.end]), min(twitch[bl2.start:bl2.end]))
  
  plot(twitch)
  abline(v=peakset[,2], col="red")
  abline(v=valleyset[,2], col="blue")
  
  rect(xleft, ybottom, xright, ytop, col=c(blcol,blcol))
  segments(bl1.start+blspan/2, bl1, bl2.start+blspan/2, bl2)
  textx<-((bl2.end-bl1.end)/2)+100
  texty<-abs(bl1-bl2)/2 + min(bl1, bl2)
  text<-"sliding baseline"
  sw<-strwidth(text)
  sh<-strheight(text)
  frame<-100
  rect(textx-sw/2-frame, texty-sh/2-frame, textx+sw/2+frame, texty+sh/2+frame, col="white")
  
  text(textx, y=texty, labels="moving baseline")
}
