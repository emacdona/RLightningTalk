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

fourierSeries <- function(a,b,T0,n){
   sinusoids <- matrix(nrow = 2*(n+1), ncol = 3);
   for(i in 0:n){
      sinusoids[2*i+1,1] <- a(i);
      sinusoids[2*i+1,2] <- 90;
      sinusoids[2*i+1,3] <- i/T0;

      sinusoids[2*i+2,1] <- b(i);
      sinusoids[2*i+2,2] <- 0;
      sinusoids[2*i+2,3] <- i/T0;
   }
   return(sinusoids);
}

plotFS <- function(x0, x1, dbIndex, n){
   ss <- sinusoids(
            x0, x1,
            fourierSeries(
               coefficients[[dbIndex]][["an"]],
               coefficients[[dbIndex]][["bn"]],
               coefficients[[dbIndex]][["T0"]],
               n
            ));
   plot( ss[['x']], 
         ss[['y']], 
         type='l', col="blue");
   lines( ss[['x']], 
         sapply(
            ss[['x']],
            coefficients[[dbIndex]][["trueFn"]]
         ),
         type='l', col="red");
}

#Database of interesting coefficients
coefficients <- list(
   triangleWave = list(
      an = function(n){
         return(0);
      },
      bn = function(n){
         if(n == 0){
            return(0);
         }
         return((8/((n^2)*(pi^2)))*sin(n*pi/2));
      },
      T0 = 2,
      trueFn = function(x){
         x <- x + 0.5;
         x <- x %% 2;
         if( ( floor(x) %% 2 ) == 0 ){
            return( 2*x - 1 );
          }
         return( -2*(x-1) + 1 );
      }
   ),
   squareWave = list(
      an = function(n){
         if(n == 0){
            return(1/2);
         }
         return((2/(n*pi))*sin((n*pi)/2));
      },
      bn = function(n){
         return(0);
      },
      T0 = 2*pi,
      trueFn = function(x){
         x0 <- abs(x) %% (2*pi);
         if( (x0 < pi/2) || (x0 > 3*pi/2) ){
            return(1);
         }
         else{
            return(0);
         }
      }
   )
);

#TODO:
#Add labels to the plot datbase.
#Add a callback to add arbitrary plot decorations
#Add a callback to get arbitrary plot configurations (axis height and width, for example)

#source('lt.R')
