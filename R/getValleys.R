# internal function call
# extract valleys from muscle twitch train

getValleys<-function(train, peakdist=400){
  train<-train*-1
  thresh<-min(train)+(max(train)-min(train))/2
  valleyset<-findpeaks(train.inv,minpeakheight=thresh, minpeakdistance = peakdist, sortstr = FALSE)
  return(valleyset)
}
