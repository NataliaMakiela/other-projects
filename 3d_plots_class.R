### 3dchart class that enables 3D plots creation. 
# Class has to be defined in a following way:
# f <- chart(function, xrange, yrange, ...)
# for example:
# f1 <- chart(
#   f = 1 + 5 * exp(-x ^ 2 - y ^ 2) ,
#   xrange = c(-5, 5),
#   yrange = c(-5, 5),
#   n = 100,
#   col = "blue", alpha = 0.2)
###

rm(list=ls())
library(rgl)
library(rlang)

##class 3dchart
chart <- function(f, xrange, yrange, n=100, ...){
  object <- list()
  
  x <- seq(xrange[1], xrange[2], length.out = n) 
  y <- seq(yrange[1], yrange[2], length.out = n)
  
  f1 <- substitute(f)
  f2 <- function(x, y) { eval(f1) }
  
  object$x <- x
  object$y <- y
  object$z <- outer(x, y, f2)
  
  attr(object, "funkcja") <- f1
  
  params <- list(...)
  
  if (any(!params == 'xlab')){
    params$xlab <- ""
  }
  if (any(!params == 'ylab')){
    params$ylab <- ""
  }
  if (any(!params == 'zlab')){
    params$zlab <- ""
  }
  
  attr(object, "params") <- params
  class(object) <- "3dchart"
  return(object)
}

print.3dchart <- function(x){
  cat('|\n|\tf(x, y) = ')
  print(attr(x, "funkcja"))
  cat('|')
}

plot.3dchart <- function(..., plotOptions=NULL){
  rgl.open()
  
  for (option in plotOptions){
      eval(option,envir = environment())
  }  
  
  charts <- list(...)
  
  c<-charts[[1]]
  exec("persp3d", x=c$x, y=c$y, c$z,  !!!attr(c, "params"))
  
  if (length(charts)>1){
    for (i in charts[2:length(charts)]){
      exec("persp3d", x=i$x, y=i$y, i$z, add = TRUE, !!!attr(i, "params"))
    }
  }
}

##example
f1 <- chart(
  f = 1 + 5 * exp(-x ^ 2 - y ^ 2) ,
  xrange = c(-5, 5),
  yrange = c(-5, 5),
  n = 100,
  col = "blue", alpha = 0.2)

f1
plot(f1)


viewPointMatrix <- matrix(
  c(0.91853338, -0.29720816,  0.26069823,  0.00000000,  0.39085239,  0.58357424,
    -0.71181136,  0.00000000,  0.05941933,  0.75571710,  0.65219706,  0.00000000,
    0.00000000,  0.00000000,  0.00000000,  1.00000000), 4, 4)


plot(f1,
     plotOptions = list(
       par3d(windowRect = 50 + c(0, 0, 1000, 1000)), 
       rgl.bg(color = rgb(.95, .95, .95, .2)), 
       rgl.viewpoint(userMatrix = viewPointMatrix) 
     )
)

f2 <- chart(
  sin(y ^ 2) + cos(x * y),
  c(-5, 5),
  c(-5, 5),
  n = 100,
  alpha= 0.6, col = "red", aspect = c(1, 1, .2),
  xlab = "x values", ylab = "y values")

f2

plot(f2,
     plotOptions = list(
       par3d(windowRect = 50 + c(0, 0, 1000, 1000)), 
       rgl.bg(color = rgb(.97, .97, .97, .2)), 
       rgl.viewpoint(userMatrix = viewPointMatrix) 
     )
)


f1 <- chart(
  f = 1 + 5 * exp(-x ^ 2 - y ^ 2) ,
  xrange = c(-5, 5),
  yrange = c(-5, 5),
  n = 100,
  col = "blue", alpha = 0.6, xlab = "x axis")


plot(f2, f1, 
     plotOptions = list(
       par3d(windowRect = 50 + c(0, 0, 1000, 1000)),
       rgl.bg(color = "white"),
       rgl.viewpoint(userMatrix = viewPointMatrix)
     )
)

