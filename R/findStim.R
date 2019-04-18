# internal function call
# peakfinder tuned to detect stimulations from channel 2

findStim<-function(stim){
  thresh<-min(stim)+(max(stim)-min(stim))/2
  stimpeaks<-findpeaks(stim, minpeakheight=0.5, minpeakdistance=100)
  return(stimpeaks)
}
