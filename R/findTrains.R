findTrains<-function(wave, names=NULL, len=13000){
  
  if (is.list(wave)==TRUE){
      waves<-wave
    for(i in 1:length(wave)){
      wave<-waves[[i]]
      
      twitch=wave@left
      samp.rate<-wave@samp.rate
      stim=wave@right
      
      stim.smooth<-as.vector(env(stim, f = samp.rate, envt = "abs", msmooth=c(125, 0.8), norm = TRUE, plot=FALSE))
      stim.smooth[stim.smooth>.5]<-1
      stim.smooth[stim.smooth<.5]<-0
      t.end<-length(stim)/samp.rate
      
      time<-seq(0, t.end, length.out=length(stim.smooth))
      samples<-round(seq(1, length(twitch), length.out=length(stim.smooth)))
      
      smooth.out<-matrix(c(samples, time, stim.smooth), nrow=length(stim.smooth), ncol=3, dimnames=list(NULL, c("samples", "sec", "stim")))
      
      trainpeaks<-findpeaks(smooth.out[,3], minpeakheight=0.5, minpeakdistance=100)
      stimpeaks<-findpeaks(smooth.out[,3], minpeakheight=0.5, minpeakdistance=10)
      
      t.next<-c(stimpeaks[2:(nrow(stimpeaks)),2], NA)
      stim.int<-t.next-stimpeaks[,2]
      
      ntrains<-nrow(trainpeaks)

      if(is.null(names)==FALSE){
        names.out<-names[1:ntrains]
      }

      train.start<-smooth.out[trainpeaks[,3],1]-1500
      train.end<-train.start+len
      
      train.start[train.start<0]<-0
      train.end[train.end>length(twitch)]<-length(twitch)
      
      trains<-vector(mode="list", length=ntrains)
      
      for (j in 1:ntrains){
        start<-train.start[j]
        end<-train.end[j]
        twitch.out<-twitch[start:end]
        stim.out<-stim[start:end]
        
        if(is.null(names)==FALSE){
          name<-names.out[j]
          trains[[j]]<-list("twitch"=twitch.out, "stim"=stim.out, "name"=name)
        } else {
          trains[[j]]<-list("twitch"=twitch.out, "stim"=stim.out)
        }
      }
      if(i==1){
        trains.out<-trains
      } else {
        trains.out<-append(trains.out, trains)
      }
      names<-names[-(1:ntrains)]
    } ## END MULTI-TRAIN SECTION
    
  } else {
    twitch=wave@left
    samp.rate<-wave@samp.rate
    stim=wave@right
    
    # smooth out stim
    stim.smooth<-as.vector(env(stim, f = samp.rate, envt = "abs", msmooth=c(125, 0.8), norm = TRUE, plot=FALSE))
    stim.smooth[stim.smooth>.5]<-1
    stim.smooth[stim.smooth<.5]<-0
    t.end<-length(stim)/samp.rate
    
    # but now we need to keep the timescales tractable
    time<-seq(0, t.end, length.out=length(stim.smooth))
    samples<-round(seq(1, length(twitch), length.out=length(stim.smooth)))
    
    smooth.out<-matrix(c(samples, time, stim.smooth), nrow=length(stim.smooth), ncol=3, dimnames=list(NULL, c("samples", "sec", "stim")))
    
    
    trainpeaks<-findpeaks(smooth.out[,3], minpeakheight=0.5, minpeakdistance=100)
    stimpeaks<-findpeaks(smooth.out[,3], minpeakheight=0.5, minpeakdistance=10)
    
    t.next<-c(stimpeaks[2:(nrow(stimpeaks)),2], NA)
    stim.int<-t.next-stimpeaks[,2]
    
    ntrains<-nrow(trainpeaks)
    
    train.start<-smooth.out[trainpeaks[,3],1]-1500 # get start point for trains in original sample units
    train.end<-train.start+len # get endpoint for trains in original sample units
    
    train.start[train.start<0]<-0
    train.end[train.end>length(twitch)]<-length(twitch)

    trains.out<-vector(mode="list", length=ntrains)
    
    for (i in 1:ntrains){
      start<-train.start[i]
      end<-train.end[i]
      twitch.out<-twitch[start:end]
      stim.out<-stim[start:end]
      
      if(is.null(names)==TRUE){
        trains.out[[i]]<-list("twitch"=twitch.out, "stim"=stim.out)
      } else {
        name<-names[i]
        trains.out[[i]]<-list("twitch"=twitch.out, "stim"=stim.out, "name"=name)
      }
    }
  }
   return(trains.out) 
}
