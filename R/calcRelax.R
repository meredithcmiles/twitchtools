# called externally to get percent relaxation for a single train
# called internally for batch processing in percentRelax

calcRelax<-function(train, peakdist){

  twitch<-train$twitch
  stim<-train$stim
  name<-train$name
  
  peaks<-getPeaks(twitch, peakdist, plot=FALSE)
  peakset<-peaks$peaks
  valleyset<-peaks$valleys

  npeaks<-nrow(peakset)
  
  valley0<-valleyset[1:npeaks-1,2]
  valleyf<-valleyset[2:npeaks,2]
  
  blspan<-quantile(valleyf-valley0, probs=.8)
  
  bl1.start<-blspan-blspan/2
  bl1.end<-blspan+blspan/2
  
  bl2.start<-max(peakset[,2]+blspan)
  bl2.end<-bl2.start+blspan
  
  baseline<-seq(bl1, bl2, length.out = nrow(peakset))
  
  t.stim<-findStim(stim)[,2]
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
