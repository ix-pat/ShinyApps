sss <- function(input,graf){

  eb <- function(m, v) {
    mu <- m/8; var <- v/8
    alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
    beta <- alpha * (1 / mu - 1)
    return(c(alpha, beta))
  }
  ebi <- function(a,b){
    mu <- a/(a+b)
    v  <- a*b/(a+b)^2/(a+b+1)
    return(c(mu,v))
  }
  
  
  f1 <- function(x) 1/8*dbeta(x/8,a1,b1)
  f2 <- function(x) 1/8*dbeta(x/8,a2,b2)
  fm <- function(x) alm*f1(x) + (1-alm)*f2(x)
  F1 <- function(x) pbeta(x/8,a1,b1)
  F2 <- function(x) pbeta(x/8,a2,b2)
  Fm <- function(x) alm*F1(x) + (1-alm)*F2(x)
  
  m1  <- input$m1
  v1  <- input$v1^2    
  m2  <- input$m2
  v2  <- input$v2^2    
  mm  <- input$mm
  alm <- input$alpha
  it  <- input$int  
  sl  <- input$sl
  mod <- input$mod
  a1 <- eb(m1,v1)[1]
  b1 <- eb(m1,v1)[2]
  a2 <- eb(m2,v2)[1]
  b2 <- eb(m2,v2)[2]
  
  dx <- 2^(3-mm)
  
  x <- seq(0,8,by=dx)
  n <- length(x)
  
  Fm(x)
  pdx <- diff(Fm(x))
  xg <- x[1:(2^(mm))]+dx/2
  
  if (it[1]>=0) xinf <- max(x[(x-it[1])<=0])
  if (it[1]<0) xinf<-0
  if (it[2]>=8) xsup <- 8 else xsup <- min(x[(x-it[2])>=0])
  it2 <- c(xinf,xsup)
  
  pd <- stepfun(x,c(0,pdx/dx,0))
  Pd <- stepfun(c(0,x),c(0,Fm(x),1))
  xg <- x[1:(2^(mm))]#+dx/2

  if (graf=="dist")  {
  plot(c(-1,x),c(0,Fm(x+dx)),type='s',axes=F,xlim=c(-.5 ,8),ylim=c(0,1),xlab='x',ylab='F(x)')
  curve(Pd,it[1],it[2],lwd=2,col=2,add=T,n=501)
  abline(1,0,lty=2)
  axis(1,at = 0:8,cex.axis=1.5);axis(2)
  segments(it,c(0,0),x1 = it,Pd(it),lty=2)
  segments(it,Pd(it),c(0,0)-1,Pd(it),lty=2)
  #arrows(0,Fm(it[1]),0,Fm(it[2]),code=4)
  
  if (input$sl) arrows(.2,Pd(it)[1],.2,Pd(it)[2],code = 3)
  if (input$sl) text(.3,mean(Fm(it2)),round(Pd(it[2])-Pd(it[1]-1e-10),2),cex=2,col=4)
  } else {
    plot(c(0,x),c(0,pdx/dx,0),type='s',xlim=c(-.5,8),axes=F,lwd=1,col=4,ylim=c(0,min(10,max(pdx,pdx/dx))+.1),xlab='x',ylab='f(x)')
    curve(pd(x),from =xinf,to = xsup,n = 201,type = "h",col="grey",add=T)
    points(xg, pdx,type='h',xlim=c(-.5,8),lwd=3,col=2)
    if (mm < 4) text(x[-n],pdx+.05,round(pdx,2),cex=1.5,adj = c(0,1))
    axis(1,at = 0:8,cex.axis=1.5);axis(2)
   # if (input$sl) text(x =mean(c(xinf,xsup))+.1,y = .1,labels = round(Pd(it[2])-Pd(it[1]-1e-10),2),cex=2,col=4)
    if (input$md) {curve(f1(x)*alm,add=T,col=3,lwd=3);  curve(f2(x)*(1-alm),add=T,col=6,lwd=3)}
  }
}