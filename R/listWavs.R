# a very simple function to pull a list of wav files in the working directory
# mostly exists to make code more readable
# original utility from warbleR vignettes: https://cran.r-project.org/web/packages/warbleR/

listWavs<-function(){
  wavs<-list.files(pattern=".wav$", ignore.case=TRUE)
  return(wavs)
}
