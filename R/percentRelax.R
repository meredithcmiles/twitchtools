# calculates percent relaxation for a batch of twitch trains

percentRelax<-function(trains, ntrains=length(trains), samp.rate=8000, ...){
  
  for(i in 1:ntrains){
    train<-trains[[i]]
    
    if(length(peakdist)>1){
      dist<-peakdist[i]
    } else {
      dist<-peakdist
    }
    
    train.out<-calcRelax(train, ...)
    
    if(i==1){
      output<-train.out
    } else {
      output<-rbind(output, train.out)
    }
  }
  return(output)
}
