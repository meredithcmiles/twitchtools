# scans a wav file (in tuneR's Wave format) for pulse trains
# currently needs a 2-channel file

findTrains<-function(wave){
  twitch=wave@left
  stim=wave@right
  samp.rate<-wave@samp.rate
  
  # smooth out stim
  stim.smooth<-as.vector(env(stim, f = samp.rate, envt = "abs", msmooth=c(100, 0.8), norm = TRUE, plot=FALSE))
  stim.smooth[stim.smooth>.5]<-1
  stim.smooth[stim.smooth<.5]<-0
  t.end<-length(stim)/samp.rate
  
  # but now we need to keep the timescales tractable
  time<-seq(0, t.end, length.out=length(stim.smooth))
  samples<-round(seq(1, length(twitch), length.out=length(stim.smooth)))
  
  smooth.out<-matrix(c(samples, time, stim.smooth), nrow=length(stim.smooth), ncol=3, dimnames=list(NULL, c("samples", "sec", "stim")))
  
  
  stimpeaks<-findpeaks(smooth.out[,3], minpeakheight=0.5, minpeakdistance=175)
  
  train.start<-smooth.out[stimpeaks[,3]-10,1] # get start point for trains in original sample units
  train.end<-smooth.out[stimpeaks[,4]+10,1] # get endpoint for trains in original sample units
  
  trains<-vector(mode="list", length=length(train.start))
  
  for (i in 1:length(trains)){
    trains[[i]]<-data.frame("twitch"=twitch[train.start[i]:(train.end[i]+samp.rate)],"stim"=stim[train.start[i]:(train.end[i]+samp.rate)])
  }
  
  return(trains)
}
