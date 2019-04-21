# internal function call
# extract valleys from muscle twitch train

getValleys<-function(train, mode="findpeaks", thresh=1.25, peakdist=225){
  stim<-train$stim
  t.stim<-findStim(stim)
  n<-length(t.stim)
  stim0<-min(t.stim)
  stimf<-max(t.stim)
  
  if(mode=="min"){
    window.start<-t.stim
    window.end<-c(t.stim[2:n], t.stim[n]+(t.stim[n]-t.stim[n-1]))
    
    valleyset<-matrix(NA, nrow=n, ncol=2)
    
    for(i in 1:n){
      slice<-twitch[window.start[i]:window.end[i]]
      valleyset[i,1]<-min(slice)
      valleyset[i,2]<-which(slice==min(slice))+window.start[i]
    }
  } else if (mode=="findpeaks"){
    twitch.inv<-train$twitch*-1
    threshold<-min(twitch.inv)+(max(twitch.inv)-min(twitch.inv))/thresh
    valleyset<-findpeaks(twitch.inv,minpeakheight=threshold, minpeakdistance = peakdist, sortstr = FALSE)[,1:2]
    valleyset<-valleyset[order(valleyset[,2]),]
    valleyset[,1]<-valleyset[,1]*-1
    
    stim0<-stim0-50
    valleyset<-valleyset[!valleyset[,2]<stim0,]
    ind<-which(valleyset[,2]>stimf)
    
    if(length(ind)>1){
      drop<-min(ind)+1
      valleyset<-valleyset[1:drop,]
    }
  }
  return(valleyset)
} 
