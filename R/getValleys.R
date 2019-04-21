# internal function call
# extract valleys from muscle twitch train

getValleys<-function(train, thresh=1.25, peakdist=225){
  stim<-train$stim
  twitch<-train$twitch*-1
  threshold<-min(twitch)+(max(twitch)-min(twitch))/thresh
  valleyset<-findpeaks(twitch,minpeakheight=threshold, minpeakdistance = peakdist, sortstr = FALSE)
  valleyset<-valleyset[order(valleyset[,2]),]
  valleyset[,1]<-valleyset[,1]*-1
  
  stimset<-findStim(stim)
  stim0<-min(stimset)
  stimf<-max(stimset)
  
  stim0<-stim0-50
  valleyset<-valleyset[!valleyset[,2]<stim0,]
  
  ind<-which(valleyset[,2]>stimf)
  if(length(ind)>1){
    drop<-min(ind)+1
    valleyset<-valleyset[1:drop,]
  }
  return(valleyset)
}
