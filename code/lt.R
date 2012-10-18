library(matlab) # So we have linspace()

#----------------------------------------------------------------------
# FSApprox Constructor
#----------------------------------------------------------------------
makeFSApprox <- function(T0, an, bn, trueFn){
   fsApprox <- list(
      T0 = T0,
      an = an,
      bn = bn,
      trueFn = trueFn
   );
   class(fsApprox) <- c("FSApprox");
   return(fsApprox);
}

#----------------------------------------------------------------------
# Create some instances of the FSApprox class
#----------------------------------------------------------------------
triangleWave = makeFSApprox(
   2,
   function(n){
      return(0);
   },
   function(n){
      if(n == 0){
         return(0);
      }
      return((8/((n^2)*(pi^2)))*sin(n*pi/2));
   },
   function(t){
      t <- t + 0.5;
      t <- t %% 2;
      if( ( floor(t) %% 2 ) == 0 ){
         return( 2*t - 1 );
       }
      return( -2*(t-1) + 1 );
   }
);

squareWave = makeFSApprox(
   2*pi,
   function(n){
      if(n == 0){
         return(1/2);
      }
      return((2/(n*pi))*sin((n*pi)/2));
   },
   function(n){
      return(0);
   },
   function(t){
      t0 <- abs(t) %% (2*pi);
      if( (t0 < pi/2) || (t0 > 3*pi/2) ){
         return(1);
      }
      else{
         return(0);
      }
   }
);

#----------------------------------------------------------------------
# Virutal function declarations for FSApprox class
#----------------------------------------------------------------------
Fn <- function(obj, ...){
   UseMethod("Fn");
}

getPlotParams <- function(obj, ...){
   UseMethod("getPlotParams");
}

#----------------------------------------------------------------------
# FSApprox methods
#
# FSApprox is a class for representing Fourier Series Approximations
# of functions
#----------------------------------------------------------------------

print.FSApprox <- function(fsa){
   cat("T0: ", fsa$T0, "\n\n");
   cat("an:", "\n"); 
   print(fsa$an);
   cat("\nbn:", "\n");
   print(fsa$bn);
   cat("\ntrueFn:", "\n");
   print(fsa$trueFn);
}

plot.FSApprox <- function(fsa,t0,t1,n,
                          before=function(){}, 
                          after=function(){})
{
   t <- getSamplePoints(t0,t1,n/fsa$T0) ;
   before();
   p <- plot( t, Fn(fsa,n)(t), 
         type='l', col="blue", 
         xlab=getPlotParams(fsa)$xlab, 
         ylab=getPlotParams(fsa)$ylab);
   lines( t, 
         sapply( t, fsa$trueFn),
         type='l', col="red");
   legend(  "topright", 
            legend=sapply(
               c(bquote(F[.(n)](t)), bquote(x(t))),
               as.expression
            ), 
            fill=c("blue", "red"),
            y.intersp=1.5,
            bg="white");
   after();
}

getPlotParams.FSApprox <- function(fsa){
   return(list(
      xlab=expression(t),
      ylab=""
   ));
}

Fn.FSApprox <- function(fsa,N){
   return(
      function(t){
         return(
            summation(
               function(n) {
                  return(
                     (fsa$an(n) * sin( (2 * pi * n/fsa$T0 * t) + (90 * pi / 180))) +
                     (fsa$bn(n) * sin(  2 * pi * n/fsa$T0 * t  )) 
                  );
               }, 
               0,N
            )
         );
      }
  );
}

#-------------------
#
# Helper functions
#
#-------------------

#---------------------------------------------------------------------------------------
# getSamplePoints
#
# For a given starting point (x0), a given end point (x1), and a given frequency, return 
# a vector starting at x0 and ending at x1 that has enough sample points to plot a 
# signal with the given frequency
#---------------------------------------------------------------------------------------
getSamplePoints <- function(t0, t1, frequency){
   #Nyquist says to use this many samples...
   n <- (2*frequency) * (t1 - t0);
   #But, Nyquist was reconstructing analog signals, not connecting dots with straight lines.
   n <- 50*n;
   return(linspace(t0,t1,n));
}

#---------------------------------------------------------------------------------------
# summation 
#
# Returns: f(n) + f(n+1) + ... + f(N-1) + f(N)
#---------------------------------------------------------------------------------------
summation <- function(f, n, N){
   sum <- 0;
   for(i in n:N){
      sum <- sum + f(i);
   }
   return(sum);
}

pdf("squareWave10.pdf")
plot(squareWave,-2*pi,2*pi,10);

pdf("squareWave50.pdf")
plot(squareWave,-2*pi,2*pi,50);

pdf("triangleWave3.pdf")
plot(triangleWave, -pi/2, pi/2, 3);

pdf("triangleWave20.pdf")
plot(triangleWave, -pi/2, pi/2, 20);

pdf("borderedSquareWave50.pdf")
plot( squareWave,-2*pi,2*pi,50,
      function(){par(bg="white")},
      function(){box("figure")});

pdf("borderedTriangleWave3.pdf")
plot( triangleWave, -pi/2, pi/2, 3,
      function(){par(bg="white")},
      function(){box("figure")});

#source('lt.R')
