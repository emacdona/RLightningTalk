library(sound)
sample   <- loadSample('tm.wav')
sFreq    <- rate(sample)
nBits    <- bits(sample)
snd      <- sound(sample)

getPortion <- function(sample, sFreq, start, duration) {
   startSample <- start * sFreq
   endSample   <- startSample + duration * sFreq - 1
   return(sample[startSample:endSample])
}

#findStart
fs <- function(start,end) {
   return(getPortion(sample, sFreq, start, end))
}


#playFrom
pf <- function(start,end){
   play(fs(start,end))
}

sampleFrag = fs(38.4, 15.9)
plot(timeArray, s1, type='l', col='black', xlab='Time (ms)', ylab='Amplitude')

#source('lt.R')
