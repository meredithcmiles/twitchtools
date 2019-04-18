# calculates percent relaxation for a batch of twitch trains

percentRelax<-function(wave, trains, peakdist=220, ntrains=length(trains)){

  samp.rate<-wave@samp.rate
  
  for(i in 1:ntrains){
    train<-trains[[i]]$twitch
    stim<-trains[[i]]$stim
    name<-trains[[i]]$name
    
    train.out<-calcRelax(train, stim, name)
    
    if(i==1){
      output<-train.out
    } else {
      output<-rbind(output, train.out)
    }
  }
  return(output)
}
