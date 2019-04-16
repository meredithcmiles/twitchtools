# takes a time-series vector as input
# computes summary stats (mean, sd) over a sliding window
# user supplies window length (default=2000 samples) and overlap % (default=0.5)
# mostly used by other functions

sliding.stats<-function(wav, wl=2000, prop=0.5){
  
  len<-round(length(wav), digits=-3)
  i<-1
  nsteps<-round((1/prop)*round(len/wl))
  output<-matrix(NA, nrow=nsteps, ncol=3)
  colnames(output)=c("n", "avg.dB", "stdev")
  
  windows<-matrix(0, nrow=wl, ncol=nsteps)
  
  for (i in 1:nsteps){
    if(i==1){ 
      start=1
      end=wl
    } else {
      start=end-prop*wl
      end=start+wl-1
    }
    
    window<-wav[start:end]
    windows[,i]=window
    
    output[i,1]=i
    output[i,2]=mean(window)
    output[i,3]=sd(window)
    i=i+1
  }
  
  return(list("output"=output, "windows"=windows))
  
}
