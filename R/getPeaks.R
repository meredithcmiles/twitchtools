getPeaks<-function(train, peakdist){
  thresh<-min(train)+(max(train)-min(train))/1.8
  valleyset<-getValleys(train, peakdist)
  valley.loc<-valleyset[,2]
  
  # search for a peak after each valley
  start.loc<-valley.loc[1:length(valley.loc)-1]
  end.loc<-valley.loc[2:length(valley.loc)]
  
  peakset<-matrix(NA, nrow=length(start.loc), ncol=4)
  
  for (i in 1:length(start.loc)){
    start<-start.loc[i]
    end<-end.loc[i]
    
    peakset[i,]<-findpeaks(train[start:end], minpeakheight = thresh, npeaks=1)
  }
  valleyset<-valleyset[1:length(peakset),]
  return(list("peaks"=peakset, "valleys"=valleyset))
}
