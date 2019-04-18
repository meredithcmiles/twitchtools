# diagnostic function to test different peak-finding parameters on a target pulse train

trainPlot<-function(train, peakdist){
  stim<-train$stim
  train<-train$twitch
  thresh<-min(train)+(max(train)-min(train))/2
  
  peakset<-findpeaks(train,minpeakheight=thresh, minpeakdistance = peakdist, sortstr = FALSE) # starter peakfinding function
  peakset<-peakset[order(peakset[,2]),]
  
  valleyset<-getValleys(train, peakdist)
  valleyset<-valleyset[order(valleyset[,2]),]
  
  npeaks<-nrow(peakset) # record the number of peaks
  
  t.stim<-findStim(stim)[,2]
  t.stim<-t.stim[order(t.stim)]
  
  peakcenter<-peakset[,2] # start index of each peak
  
  peakstart<-peakcenter-200 # we'll look back 250 samples
  peakstart[peakstart<0]<-0 
  
  peakend<-peakcenter+200 # and we'll look forward 250 samples
  peakend[peakend>length(train)]<-length(train)
  
  peakstats<-matrix(NA, nrow=npeaks, ncol=5) # we'll rely on statistics to check if our peaks were accurate
  
  for (j in 1:npeaks){ # stats for every peak!
    start<-peakstart[[j]]
    end<-peakend[[j]]
    peakstats[j,1]<-mean(train[start:end]) # mean amplitude across the ID'd peak
    peakstats[j,2]<-sd(train[start:end])   # sd
    peakstats[j,3]<-min(train[start:end]) # min
    peakstats[j,4]<-max(train[start:end])  # max
    peakstats[j,5]<-peakstats[j,4]-peakstats[j,3] # range
  }
  
  # false peaks picked up from the baseline have LOW SD and HIGH MEAN compared to true peaks
  # we'll cluster our data based on this fact
  clust.mean<-Ckmeans.1d.dp(peakstats[,1], k=2, y=1)$cluster
  keep.mean<-vector(mode="logical", length=length(clust.mean))
  keep.mean[which(clust.mean==1)]<-TRUE
  clust.sd<-Ckmeans.1d.dp(peakstats[,2], k=2, y=1)$cluster
  keep.sd<-vector(mode="logical", length=length(clust.mean))
  keep.sd[which(clust.sd==2)]<-TRUE
  
  bl1.end<-peakset[min(which(keep.sd==TRUE))-1,2]
  if(bl1.end-500>1){
    bl1.start<-bl1.end-500
  } else {
    bl1.start<-1
  }
  bl1<-mean(train[bl1.start:bl1.end])
  
  bl2.start<-peakset[max(which(keep.sd==TRUE))+1,2]
  bl2<-mean(train[bl2.start:length(train)])
  
  rmv<-peakset[!keep.mean,]
  peakset<-peakset[keep.mean,]

  rmv.peaks.start<-peakset[peakset[,2]<t.stim[1],2]
  last.stim<-t.stim[length(t.stim)]
  rmv.peaks.back<-peakset[peakset[,2]>last.stim+300,2]
  
  rmv.peaks<-c(rmv.peaks.start, rmv.peaks.back)
  peakset<-peakset[!peakset[,2] %in% rmv.peaks,]
  
  peak.0<-peakset[1,2]
  valley.0<-valleyset[1,2]
  peak.f<-peakset[nrow(peakset),2]
  valley.f<-valleyset[nrow(valleyset),2]
  
  if (peak.f<valley.f){
    valleyset<-valleyset[-nrow(valleyset),]
  }
  
  if(peak.0<valley.0){
    peakset<-peakset[-1,]
  }

  if (nrow(valleyset)==nrow(peakset)+1){
    
    valleyset<-valleyset[-nrow(valleyset),]
    
  }
  
  if(bl1<peakset[1,1]){
    bl1<-peakset[1,1]
  }
  
  if(bl2<peakset[nrow(peakset),1]){
    bl2<-peakset[nrow(peakset),1]
  }
  
  plot(train)
  abline(v=peakset[,2], col="red")
  abline(v=valleyset[,2], col="blue")
  abline(h=c(bl1, bl2))
}
