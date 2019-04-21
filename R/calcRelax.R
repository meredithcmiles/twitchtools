# called externally to get percent relaxation for a single train
# called internally for batch processing in percentRelax

calcRelax<-function(train, thresh, peakdist, samp.rate=8000){

  twitch<-train$twitch
  stim<-train$stim
  name<-train$name
  
  hairy<-paste("hawo", c(1:12), sep="")
  downy<-paste("dowo", c(1:12), sep="")
  accel<-c(hairy, downy)
  
  peaks<-getPeaks(train, thresh, peakdist)
  peakset<-peaks$peaks
  valleyset<-peaks$valleys

  last.val<-max(valleyset[,2])
  valleyset<-valleyset[1:nrow(peakset),]
  npeaks<-nrow(peakset)
  
  valley0<-valleyset[1:npeaks-1,2]
  valleyf<-valleyset[2:npeaks,2]
  valleyspan<-valleyf-valley0
  blspan<-mean(valleyspan)
  
  bl1.end<-min(valley0)-(blspan/3)
  bl1.start<-bl1.end-(blspan/2)
  bl1.start[bl1.start<1]<-1
  if(bl1.start==1){
    bl1.end<-blspan/2
  }
  
  bl2.start<-last.val+blspan
  bl2.end<-bl2.start+(blspan/2)
  
  bl1<-mean(twitch[bl1.start:bl1.end])
  bl2<-mean(twitch[bl2.start:bl2.end])
  
  if(name %in% accel==TRUE && nrow(valleyset)>19){
    valleyset<-valleyset[1:19,]
    peakset<-peakset[1:19,]
  }
  
  baseline<-seq(bl1, bl2, length.out = nrow(peakset))
  
  t.stim<-findStim(stim)
  t.stim<-t.stim[order(t.stim)]/samp.rate
  t.peak<-peakset[,2]/samp.rate
  t.valley<-valleyset[,2]/samp.rate
  
  if(length(t.stim)!=length(t.valley)){
    t.stim<-t.stim[1:length(t.valley)]
  }

  peak.amp<-peakset[,1]
  valley.amp<-valleyset[,1]
  
  relax100<-baseline-valley.amp
  peaktovalley<-peak.amp-valley.amp
  percent.relax<-peaktovalley/relax100
  percent.relax[which(percent.relax>1)]<-1
  
  contract.time<-(t.valley-t.stim)*1000
  relax.time<-(t.peak-t.valley)*1000
  
  contour.upper<-peak.amp
  contour.upper[which(percent.relax==1)]<-baseline[which(percent.relax==1)]
  contour.lower<-valley.amp
  
  traindata<-data.frame("stim"=rep(name, times=length(t.peak)), "twitch"=1:length(t.peak), percent.relax, contract.time, relax.time, baseline, contour.upper, contour.lower)
  
  return(traindata)
}
