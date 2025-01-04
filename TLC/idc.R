z <- matrix(rep(0,times=4),2)
x0 <- c(-5,10)
y0 <- c(-5,10)
xg <- seq(min(x0),max(x0),length.out = 501)
persp(
  x0,y0,M,
  zlim=c(0,.5),
  theta = 0,
  phi = -0,
  box = T,
  ticktype="detailed",nticks = 10,
  r=0,
  d=180,
  ylab = "mu",
  xlab = "Sn/n"
) -> ccc
lines(trans3d(xg,xg,xg*0,ccc),col=2)
lines(trans3d(xg,xg+1.96,xg*0,ccc),col=2)
lines(trans3d(xg,xg-1.96,xg*0,ccc),col=2)

ss <- 10
n <- 100
alp <- .05
idc <- function(xm) xm + rev((-1)^(0:1))*qnorm(1-alp/2)*ss/sqrt(n)

xm <- 1


idc(xm)
lines(trans3d(xg,xg*0,dnorm(xg,xm,ss/sqrt(n)),ccc),col=4)

lines(trans3d(c(xm,xm),c(xm,xm),c(0,dnorm(xm,xm,ss/sqrt(n))),ccc),lty=2)
lines(
  trans3d(
    c(idc(xm)[1],idc(xm)[1]),
    c(idc(xm)[1],idc(xm)[1]),
    c(0,dnorm(idc(xm)[1],xm,ss/sqrt(n))),
    ccc),
  lty=2
)

lines(
  trans3d(
    c(idc(xm)[2],idc(xm)[2]),
    c(idc(xm)[2],idc(xm)[2]),
    c(0,dnorm(idc(xm)[2],xm,ss/sqrt(n))),
    ccc),
  lty=2
)

lines(
  trans3d(
    idc(xm),
    c(xm,xm),
    c(0,0),
    ccc),
  lwd=2,
  col=3
)
