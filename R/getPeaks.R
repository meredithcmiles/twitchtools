getPeaks<-function(twitch, peakdist, plot=c(FALSE, TRUE)){
  thresh<-min(twitch)+(max(twitch)-min(twitch))/1.8
  valleyset<-getValleys(twitch, peakdist)
  valleyset<-valleyset[order(valleyset[,2]),]
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
    
    peaks<-findpeaks(twitch[start:end], minpeakheight = thresh, minpeakdistance=100)
    peaks[,2]<-peaks[,2]+start

    peak.height<-max(slice)
    peak.loc<-index[which(slice==max(slice))]
    
    peakset[i,1]<-peak.height
    peakset[i,2]<-peak.loc
  }
  
  valleyset<-valleyset[1:n-1,]
  
  if (plot==TRUE){
    plot(twitch, type="l")
    abline(v=peakset[,2], col="red")
    abline(v=valleyset[,2], col="blue")
    return(list("peaks"=peakset, "valleys"=valleyset))
  } else if (plot==FALSE){
    return(list("peaks"=peakset, "valleys"=valleyset))
  }
}
