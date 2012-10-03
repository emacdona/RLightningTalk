library(sound)
library(matlab)
library(plyr)

# Turkish March -- Mozart
sample   <- loadSample('../data/tm.wav')
sFreq    <- rate(sample)
nBits    <- bits(sample)
snd      <- sound(sample)

# Get a new sample that is a portion of the given one
getPortion <- function(sample, sFreq, start, duration) {
   startSample <- start * sFreq
   endSample   <- startSample + duration * sFreq - 1
   return(sample[startSample:endSample])
}

#Some helper functions to help me find the portion I'm looking for
#findStart
fs <- function(start,end) {
   return(getPortion(sample, sFreq, start, end))
}

#playFrom
pf <- function(start,end){
   play(fs(start,end))
}

#Now I've found the portion I want, so time to get to work
sampleFrag = fs(38.4, 15.9)
sampleFragSound = sound(sampleFrag)

plotTimeDomain <- function(sample) {
   f     <- rate(sample)
   snd   <- sound(sample)
   s1    <- snd[1,]
   timeArray <- (0:(length(s1)-1))
   plot(timeArray, s1, type='l', col='black', xlab='Time (ms)', ylab='Amplitude')
}

plotFrequencyDomain <- function(sample) {
   snd   <- sound(sample)
   s1    <- snd[1,]
   n     <- length(s1)
   p     <- fft(s1)
   nUniquePts <- ceiling((n+1)/2)
   p <- p[1:nUniquePts]
   p <- abs(p)
   p <- p/n
   p <- p^2

   if (n %% 2 > 0){
      p[2:length(p)] <- p[2:length(p)]*2 # we've got odd number of points fft
   } else {
      p[2: (length(p) -1)] <- p[2: (length(p) -1)]*2 # we've got even number of points fft
   }

   freqArray <- (0:(nUniquePts-1)) * (sampFreq / n) #  create the frequency array 
   plot(freqArray/1000, 10*log10(p), type='l', col='black', xlab='Frequency (kHz)', ylab='Power (dB)')
}

plotFreqDomain <- function(sample) {
   plot(timeArray, s1, type='l', col='black', xlab='Time (ms)', ylab='Amplitude')
}

sinusoid <- function(x, amplitude, phase, frequency) {
   return(amplitude * sin( (2 * pi * frequency * x) + (phase * pi / 180)))
}

sinusoids <- function(x0, x1, sinusoids){
   x = getSamplePoints(x0, x1, max(sinusoids[,3]));
   y = vector(length=length(x), mode="numeric")
   for(i in seq(sinusoids[,1])){
      y <- y + sinusoid(x, sinusoids[i,1], sinusoids[i,2], sinusoids[i,3]);
   }
   return(list(x=x,y=y))
}

getSamplePoints <- function(x0, x1, frequency){
   #Nyquist says to use this many samples...
   n <- (2*frequency) * (x1 - x0);
   #But, Nyquist was reconstructing analog signals, not connecting dots with straight lines.
   n <- 50*n;
   return(linspace(x0,x1,n));
}

psin <- function(x0, x1, amplitude, phase, frequency) {
   x <- getSamplePoints(x0, x1, frequency);
   plot(x,sinusoid(x,amplitude,phase,frequency), type='l', col='red');
}

makeSinusoids <- function(amplitudes, phases, frequencies){
   return(matrix(append(amplitudes, append(phases, frequencies)), ncol=3));
}

#source('lt.R')
