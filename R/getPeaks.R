getPeaks<-function(train, thresh=2.25, peakdist=225){
  twitch<-train$twitch
  stim<-train$stim
  name<-train$name
  hairy<-paste("hawo", c(1:12), sep="")
  downy<-paste("dowo", c(1:12), sep="")
  accel<-c(hairy, downy)
  valleyset<-getValleys(train=train, thresh=thresh, peakdist=peakdist)
  valley.loc<-valleyset[,2]
  n<-length(valley.loc)
  
  # search for a peak between each of the valleys
  start.loc<-valley.loc[1:n-1]
  end.loc<-valley.loc[2:n]
  search.window<-matrix(c(start.loc, end.loc), ncol=2, nrow=n-1)

  peakset<-matrix(NA, nrow=n-1, ncol=2)
  
  for (i in 1:nrow(peakset)){
    start<-search.window[i,1]
    end<-search.window[i,2]
    
    slice<-twitch[start:end]
    index<-start:end
    
    peaks<-findpeaks(twitch[start:end], minpeakheight = quantile(twitch, probs=0.1), minpeakdistance=100)
    peaks[,2]<-peaks[,2]+start
    
    peak.loc<-min(index[which(slice==max(slice))])
    peak.height<-max(peaks[,1])
    
    peakset[i,1]<-peak.height
    peakset[i,2]<-peak.loc
  }

  blspan<-500
  
  bl1.end<-min(valleyset[,2])-blspan
  bl1.start<-bl1.end-blspan
  
  bl2.start<-max(peakset[,2])+blspan*1.5
  bl2.end<-bl2.start+blspan
  
  bl1.start[bl1.start<1]<-1
  
  bl1<-mean(twitch[bl1.start:bl1.end])
  bl2<-mean(twitch[bl2.start:bl2.end])
  
  baseline<-seq(bl1, bl2, length.out=nrow(valleyset))

  bl1.max<-max(twitch[bl1.start:bl1.end])
  bl1.min<-min(twitch[bl1.start:bl1.end])
  bl2.max<-max(twitch[bl2.start:bl2.end])
  bl2.min<-min(twitch[bl2.start:bl2.end])
  
  bl1<-c(bl1.start, bl1.end, bl1.min, bl1.max)
  bl2<-c(bl2.start, bl2.end, bl2.min, bl2.max)
  
  baseline<-list("moving"=baseline, "static"=mean(c(bl1, bl2)), "bl1"=bl1, "bl2"=bl2)
  
    return(list("peaks"=peakset, "valleys"=valleyset, "baseline"=baseline))
}
