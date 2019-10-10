# takes a stimulation train and locates the time of each stimulation

findStim<-function(stim){
  thresh<-min(stim)+(max(stim)-min(stim))/2
  n.over <- length(stim[stim>thresh])
  n.under <- length(stim[stim<thresh])

  if(n.over > n.under){
    stim <- -stim + max(stim)
    scl <- 1/max(stim)
    stim <- scale(stim, center=0.5)[,1]
  }

  stimpeaks<-findpeaks(stim, minpeakheight=thresh, minpeakdistance=100)
  t.stim<-stimpeaks[,2]
  return(t.stim[order(t.stim)])
}
