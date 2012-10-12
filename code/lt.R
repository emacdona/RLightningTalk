library(matlab)

getSamplePoints <- function(x0, x1, frequency){
   #Nyquist says to use this many samples...
   n <- (2*frequency) * (x1 - x0);
   #But, Nyquist was reconstructing analog signals, not connecting dots with straight lines.
   n <- 50*n;
   return(linspace(x0,x1,n));
}

triangleWave = list(
   T0 = 2,
   an = function(n){
      return(0);
   },
   bn = function(n){
      if(n == 0){
         return(0);
      }
      return((8/((n^2)*(pi^2)))*sin(n*pi/2));
   },
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
   T0 = 2*pi,
   an = function(n){
      if(n == 0){
         return(1/2);
      }
      return((2/(n*pi))*sin((n*pi)/2));
   },
   bn = function(n){
      return(0);
   },
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

plot.FSApprox <- function(fsa,x0,x1,n){
   x <- getSamplePoints(x0,x1,n/fsa$T0) ;
   plot( x, Fn(fsa,n)(x), 
         type='l', col="blue", 
         xlab=getPlotParams(fsa)$xlab, 
         ylab=getPlotParams(fsa)$ylab);
   lines( x, 
         sapply( x, fsa$trueFn),
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

#Boilerplate for: "Fn is a virutal function. I hope Object <x> provides a definition."
Fn <- function(x, ...){
   UseMethod("Fn");
}

Fn.FSApprox <- function(fsa,N){
   return(
      function(x){
         return(
            summation(
               function(n) {
                  return(
                     (fsa$an(n) * sin( (2 * pi * n/fsa$T0 * x) + (90 * pi / 180))) +
                     (fsa$bn(n) * sin(  2 * pi * n/fsa$T0 * x  )) 
                  );
               }, 
               0,N
            )
         );
      }
  );
}

summation <- function(f, n, N){
   sum <- 0;
   for(i in n:N){
      sum <- sum + f(i);
   }
   return(sum);
}

#source('lt.R')
