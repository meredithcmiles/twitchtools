# this is the main peak-finding function. It uses benchmarks from findStim and getValleys
# to extract peaks from each contraction-relaxation cycle.

# baseline extraction is sliding by default, accounting for variation in the background trace
# due to breathing, etc.

getPeaks<-function(train, thresh=2.25, peakdist=225, bloffset=c(200,200), ...){
  
# BEGIN: setuptra
  twitch<-train$twitch
  stim<-train$stim
  name<-train$name
  hairy<-paste("hawo", c(1:12), sep="")
  downy<-paste("dowo", c(1:12), sep="")
  accel<-c(hairy, downy)
  valleyset<-getValleys(train=train, thresh=thresh, peakdist=peakdist, mode="min")
  valley.loc<-valleyset[,2]
  n<-length(valley.loc)

  # search for a peak between each of the valleys
  start.loc<-valley.loc[1:n-1]

  t.stim <- findStim(stim)
  stim.int <- t.stim[2:length(t.stim)] - t.stim[1:length(t.stim)-1]
  end.loc <- valley.loc[2:n] - stim.int[1:length(stim.int)-1]/5
  search.window<-matrix(c(start.loc, end.loc), ncol=2, nrow=n-1)

  peakset<-matrix(NA, nrow=n-1, ncol=2)

  for (i in 1:nrow(peakset)){
    start<-search.window[i,1]
    end<-search.window[i,2]

    slice<-twitch[start:end]
    index<-start:end

    peaks<-findpeaks(twitch[start:end], minpeakheight = quantile(twitch, probs=0.1), minpeakdistance=100)
    peaks[,2]<-peaks[,2]+start

    peak.loc<-min(index[which(slice==max(slice))])
    peak.height<-twitch[peak.loc]

    peakset[i,1]<-peak.height
    peakset[i,2]<-peak.loc
  }

  #bl1.end<-min(valleyset[,2])-blspan
  stimset <- findStim(stim)
  stimint <- stimset[2:length(stimset)] - stimset[1:length(stimset)-1]
  blspan <- max(stimint)/2

  bl1.end <- min(findStim(stim))-blspan-bloffset[1]
  bl1.start<-bl1.end-blspan-bloffset[1]

  if(bl1.start < 1) {
    bl1.start <- 1
    bl1.end <- round(blspan/3)
  }

  bl2.start<-max(findStim(stim))+2*blspan+bloffset[2]
  bl2.end<-bl2.start+blspan+bloffset[2]

  if(bl2.end > length(twitch)){
    bl2.start <- length(twitch)-round(blspan/3)
    bl2.end <- length(twitch)
  }

  bl1<-mean(twitch[bl1.start:bl1.end])
  bl2<-mean(twitch[bl2.start:bl2.end])

  baseline<-seq(bl1, bl2, length.out=nrow(valleyset))

  bl1.max<-max(twitch[bl1.start:bl1.end])
  bl1.min<-min(twitch[bl1.start:bl1.end])
  bl2.max<-max(twitch[bl2.start:bl2.end])
  bl2.min<-min(twitch[bl2.start:bl2.end])

  bl1<-c(bl1.start, bl1.end, bl1.min, bl1.max)
  bl2<-c(bl2.start, bl2.end, bl2.min, bl2.max)

  baseline<-list("moving"=baseline, "static"=mean(c(bl1, bl2)), "bl1"=bl1, "bl2"=bl2, "blspan" = blspan)

  return(list("peaks"=peakset, "valleys"=valleyset, "baseline"=baseline))
}
