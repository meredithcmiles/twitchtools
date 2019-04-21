trainPlot<-function(train, line=FALSE, ...){
  twitch<-train$twitch
  stim<-train$stim
  name<-train$name
  hairy<-paste("hawo", c(1:12), sep="")
  downy<-paste("dowo", c(1:12), sep="")
  accel<-c(hairy, downy)
  peaks<-getPeaks(train=train, ...)
  peakset<-peaks$peaks
  valleyset<-peaks$valleys
  baseline<-peaks$baseline$moving
  
  bl1.start<-peaks$baseline$bl1[1]
  bl1.end<-peaks$baseline$bl1[2]
  bl2.start<-peaks$baseline$bl2[1]
  bl2.end<-peaks$baseline$bl2[2]
  
  blcol<-rgb(225/255,226/255,96/255,.5)
  xleft<-c(bl1.start, bl2.start)
  xright<-c(bl1.end, bl2.end)
  ytop<-c(peaks$baseline$bl1[4], peaks$baseline$bl2[4])
  ybottom<-c(peaks$baseline$bl1[3], peaks$baseline$bl2[3])
  
  if(line==TRUE){
    plot(twitch, type="l", xlab=name)
  } else {
    plot(twitch, xlab=name)
  }
  
  abline(v=peakset[,2], col="red")
  abline(v=valleyset[,2], col="blue")
  
  rect(xleft, ybottom, xright, ytop, col=c(blcol,blcol))
  segments(bl1.start+blspan/2, baseline[1], bl2.start+blspan/2, baseline[length(baseline)])
  textx<-((bl2.end-bl1.end)/2)+100
  texty<-abs(bl1-bl2)/2 + min(bl1, bl2)
  text<-"sliding baseline"
  sw<-strwidth(text)
  sh<-strheight(text)
  frame<-100
  rect(textx-sw/2-frame, texty-sh/2-frame, textx+sw/2+frame, texty+sh/2+frame, col="white")
  
  text(textx, y=texty, labels="moving baseline")
}
