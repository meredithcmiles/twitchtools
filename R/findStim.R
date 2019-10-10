# findTrains is the main workhorse function for processing large files

# if you're having issues, remember to try different values for:

### 'len' (train length)
##### INCREASE if trains are getting cut off before they're finished
##### DECREASE if the algorithm is lumping multiple trains into one

### 'train.interval' (distance between trains)
##### INCREASE if trains are getting lumped
##### DECREASE if trains are getting cut out (or add more baseline to your raw file)

findTrains<-function(wave, names=NULL, len=13000, train.interval = 100){

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

      trainpeaks<-findpeaks(smooth.out[,3], minpeakheight=0.5, minpeakdistance=train.interval)
      stimpeaks<-findpeaks(smooth.out[,3], minpeakheight=0.5, minpeakdistance=10)

      t.next<-c(stimpeaks[2:(nrow(stimpeaks)),2], NA)
      stim.int<-t.next-stimpeaks[,2]

      ntrains<-nrow(trainpeaks)
      train.start<-smooth.out[trainpeaks[,3],1]-(.25*len)
      train.end<-train.start+len

      train.start[train.start<0]<-0
      train.end[train.end>length(twitch)]<-length(twitch)

      if(is.null(names)==FALSE){
        names.out<-names[1:ntrains]
      }

      trains<-vector(mode="list", length=ntrains)

      for (j in 1:ntrains){
        start<-train.start[j]
        end<-train.end[j]
        twitch.out<-twitch[start:end]
        stim.out<-stim[start:end]

        if(min(twitch.out)<0){
          twitch.out<-twitch.out+min(twitch.out)+1
        }

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
    stim=scale(wave@right)[,1]

    thresh<-min(stim)+(max(stim)-min(stim))/2
    n.over <- length(stim[stim>thresh])
    n.under <- length(stim[stim<thresh])

    if(n.over > n.under){
      stim <- -stim + max(stim)
      stim <- scale(stim, center=0.5)[,1]
    }

  # smooth out stim
  stim.smooth <- as.vector(env(stim, f = samp.rate, envt = "abs", msmooth=c(75, 0.8), norm = FALSE, plot=FALSE))
  thresh<-min(stim.smooth)+(max(stim.smooth)-min(stim.smooth))/2

  stim.smooth[stim.smooth<=thresh]<-0
  stim.smooth[stim.smooth>thresh]<-1

  t.end<-length(stim)/samp.rate

  # but now we need to keep the timescales tractable
  time<-seq(0, t.end, length.out=length(stim.smooth))
  samples<-round(seq(1, length(twitch), length.out=length(stim.smooth)))

  smooth.out<-matrix(c(samples, time, stim.smooth), nrow=length(stim.smooth), ncol=3, dimnames=list(NULL, c("samples", "sec", "stim")))

  trainpeaks<-findpeaks(smooth.out[,3], minpeakheight=0.5, minpeakdistance=train.interval)
  stimpeaks<-findpeaks(smooth.out[,3], minpeakheight=0.5, minpeakdistance=10)

    t.next<-c(stimpeaks[2:(nrow(stimpeaks)),2], NA)
    stim.int<-t.next-stimpeaks[,2]

    ntrains<-nrow(trainpeaks)

    train.start<-smooth.out[trainpeaks[,3],1]-(.25*len)
    train.end<-train.start+len

    train.start[train.start<0]<-0
    train.end[train.end>length(twitch)]<-length(twitch)

    trains.out<-vector(mode="list", length=ntrains)

    for (i in 1:ntrains){
      start<-train.start[i]
      end<-train.end[i]
      twitch.out<-twitch[start:end]
      stim.out<-stim[start:end]

      if(min(twitch.out)<0){
        twitch.out<-twitch.out+min(twitch.out)+1
      }

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
