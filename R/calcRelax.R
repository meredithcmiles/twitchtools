# called externally to get percent relaxation for a single train
# called internally for batch processing in percentRelax

calcRelax<-function(train, stim, peakdist, name, mode="stim"){
    # find peaks and valleys
    thresh<-min(train)+(max(train)-min(train))/2
    
    peakset<-findpeaks(train,minpeakheight=thresh, minpeakdistance = peakdist, sortstr = FALSE) # starter peakfinding function
    peakset<-peakset[order(peakset[,2]),]
    
    valleyset<-getValleys(train, peakdist)
    valleyset<-valleyset[order(valleyset[,2]),]
    
    npeaks<-nrow(peakset) # record the number of peaks
    
    t.stim<-findStim(stim)[,2]
    t.stim<-t.stim[order(t.stim)]
    
    if (mode=="cluster"){
      
      peakcenter<-peakset[,2] # start index of each peak
      
      peakstart<-peakcenter-250 # we'll look back 250 samples
      peakstart[peakstart<0]<-0 
      
      peakend<-peakcenter+250 # and we'll look forward 250 samples
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
      
      rmv<-peakset[!keep.mean,]
      baseline<-mean(rmv[1:3,1])
      peakset<-peakset[keep.mean,]
      
    } else {
      rmv.peaks.start<-peakset[peakset[,2]<t.stim[1],]
      rmv.peaks.back<-peakset[peakset[,2]>t.stim[length(t.stim)],]
      
      rmv.peaks<-c(rmv.peaks.start[,2], rmv.peaks.back[,2])
      peakset<-peakset[!peakset[,2] %in% rmv.peaks,]
      
      baseline.start<-mean(rmv.peaks.start[,1])
      baseline.back<-mean(rmv.peaks.back[,1])
      baseline<-mean(c(baseline.start, baseline.back))
    }
    
    peak.0<-peakset[1,2]
    valley.0<-valleyset[1,2]
    peak.f<-peakset[nrow(peakset),2]
    valley.f<-valleyset[nrow(valleyset),2]
    
    if(peak.0<valley.0){
      peakset<-peakset[-1,]
    }
    
    if (peak.f<valley.f){
      valleyset<-valleyset[-nrow(valleyset),]
    }
    
    if (nrow(valleyset)==nrow(peakset)+1){
      valleyset<-valleyset[-nrow(valleyset),]
      
    } else if (nrow(valleyset)!=nrow(peakset)) {
      stop("Mismatched valleys and peaks.")
    }
    
    t.peak<-peakset[,2]/samp.rate
    t.valley<-valleyset[,2]/samp.rate
    
    peak.amp<-peakset[,1]
    valley.amp<-valleyset[,1]
    base<-rep(baseline, times=length(t.peak))
    
    relax100<-base-valley.amp
    peaktovalley<-peak.amp-valley.amp
    percent.relax<-peaktovalley/relax100
    percent.relax[which(percent.relax>1)]<-1
    
    contract.time<-(t.valley-t.stim[-length(t.stim)]/samp.rate)*1000
    relax.time<-(t.peak-t.valley)*1000
    
    traindata<-data.frame("stim"=rep(name, times=length(t.peak)), "twitch"=1:length(t.peak), percent.relax, contract.time, relax.time)
    
    return(traindata)
}
