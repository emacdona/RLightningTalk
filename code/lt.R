library(matlab)

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
);
class(triangleWave) <- c("FSApprox", "FSApproxPlot");

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
);
class(squareWave) <- c("FSApprox", "FSApproxPlot");

Sinusoid <- function(amplitude, phase, frequency){
   s <- list(  amplitude=amplitude, 
               phase=phase, 
               frequency=frequency,
               f=function(x){
                  return (amplitude * sin( (2 * pi * frequency * x) + (phase * pi / 180)))
               });
   class(s) <- c("Sinusoid");
   return(s);
}

getPlotParams <- function(x, ...){
   UseMethod("getPlotParams");
}

getPlotParams.FSApprox <- function(fsa){
   return(list(
      xlab=expression(x),
      ylab=expression(y)
   ));
}

print.FSApprox <- function(fsa){
   cat("T0: ", fsa$T0, "\n\n");
   cat("an:", "\n"); 
   print(fsa$an);
   cat("\nbn:", "\n");
   print(fsa$bn);
   cat("\ntrueFn:", "\n");
   print(fsa$trueFn);
}

sinusoids.FSApprox <- function(fsa){
}

plot.FSApprox <- function(fsa,x0,x1,n){
   ss <- sinusoids(
            x0, x1,
            fourierSeries(fsa$an, fsa$bn, fsa$T0, n));
   plot( ss$x, ss$y, 
         type='l', col="blue", 
         xlab=getPlotParams(fsa)$xlab, 
         ylab=getPlotParams(fsa)$ylab);
   lines( ss$x, 
         sapply( ss$x, fsa$trueFn),
         type='l', col="red");
   legend(  "topright", 
            legend=sapply(
               c(bquote(F[.(n)](x)), bquote(f(x))),
               as.expression
            ), 
            fill=c("blue", "red"),
            y.intersp=1.5,
            bg="white");
}

Fn.FSApprox <- function(fsa,x){
   function(x){
   }
}

#source('lt.R')
