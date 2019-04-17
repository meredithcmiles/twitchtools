# internal function called to quickly extract a pruned set of muscle contraction peaks from one stimulation train

getPeaks<-function(train, peakdist=400){
  thresh<-min(train)+(max(train)-min(train))/2
  
  peakset<-findpeaks(train,minpeakheight=thresh, minpeakdistance = peakdist, sortstr = FALSE) # starter peakfinding function
  peakset<-peakset[order(peakset[,2]),]
  
  npeaks<-nrow(peakset) # record the number of peaks
  peakcenter<-peakset[,2] # start index of each peak
  
  peakstart<-peakcenter-250 # we'll look back 250 samples
  peakstart[peakstart<0]<-0 
  
  peakend<-peakcenter+250 # and we'll look forward 250 samples
  peakend[peakend>length(train)]<-length(train)
  
  ## COLLECT STATS OVER EACH PEAK   
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
  
  if (all.equal(keep.sd, keep.mean)==FALSE){
    stop("Cluster approach failed.")
  }
  
  rmv<-peakset[!keep.mean,]
  baseline<-mean(rmv[1:3,1])
  peakset<-peakset[keep.mean,]
  return(peakset)
}
