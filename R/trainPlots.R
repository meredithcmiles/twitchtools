# batch plotting for multiple trains
# will eventually be integrated in one plotting function

trainPlots<-function(trains, ...){
  n<-length(trains)
  plotme<-1:n
  
  if(n>6){
    plotme<-sample(plotme, size=6, replace=FALSE)
    par(mfrow=c(3, 2))
    
  } else if(n==1){
    par(mfrow=c(1,1))
    trainPlot(trains[[1]])
    break
    
  } else if (n==2){
    par (mfrow=c(2,1))
    trainPlot(trains[[1]])
    trainPlot(trains[[2]])
    break
    
  } else if (2>n&&n<5){
    par(mfrow=c(2,2))
    
  } else {
    par=mfrow(c(3, 2))
  }
  
  for (i in 1:length(plotme)){
    t<-plotme[i]
    trainPlot(trains[[t]], line=TRUE, ...)
  }
}
