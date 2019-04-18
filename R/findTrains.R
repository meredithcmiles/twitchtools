# scans a wav file (in tuneR's Wave format) for pulse trains
# for now, only works with 2-channel files
# must contain the stimulation in one channel and the response in another
# uses the stimulation as basis for extraction

findTrains<-function(wave, l=100, names=NULL){
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
    
    
    stimpeaks<-findpeaks(smooth.out[,3], minpeakheight=0.5, minpeakdistance=100)
    
    ntrains<-nrow(stimpeaks)
    
    train.start<-smooth.out[stimpeaks[,3]-10,1] # get start point for trains in original sample units
    train.end<-smooth.out[stimpeaks[,4]+l,1] # get endpoint for trains in original sample units
    
    twitches<-vector(mode="list", length=ntrains)
    stimulations<-vector(mode="list", length=ntrains)
    baseline<-vector(mode="numeric", length=ntrains)
    
    for (i in 1:ntrains){
      start<-train.start[i]
      end<-train.end[i]
      twitch.out<-twitch[start:end]
      stim.out<-stim[start:end]
      
      twitches[[i]]<-twitch.out
      stimulations[[i]]<-stim.out
      baseline[i]<-mean(twitch[start+200:start+1000])
    }
    
    if(is.null(names)==TRUE){
      return(list("twitch"=twitches, "stim"=stimulations, "baseline"=baseline))
    } else {
      return(list("twitch"=twitches, "stim"=stimulations, "baseline"=baseline, "names"=names))
    }
}
