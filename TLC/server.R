#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    if (input$dist=="norm") {
    n <- input$n
    m <- input$mu^input$dn*(input$mu*n)^(1-input$dn)
    s <- input$ss/n^input$dn*(input$ss*n)^(1-input$dn)
      curve(dnorm(x,m,s),m-4*s,m+4*s,1001)
      title("Normale")

    } 
    else  if (input$dist=="bin") {
      n <- input$n
      pg <- input$pp
      xn <- 0:n
      xg <- c(-.5,xn+.5)
      db <- function(x) dbinom(round(x),n,pg)
      dbn<- function(x) n*dbinom(round(x*n),n,pg)      
      if (!input$dn) {
          curve(db,-1,n+1,type="s",axes=F,1001)
          points(xg,db(xg),type="h",lty=2)
          axis(1,cex.axis=2); axis(2)
          if (input$tcl) curve(dnorm(x,n*pg,sqrt(n*pg*(1-pg))),add=T,n=1001,col=2,lwd=2)
      }
      if (input$dn) {
        curve(dbn,-.6/n,1+.6/n,type="s",axes=F,n=1001,ylim=c(0,max(dbn(xn/n))))
        points(xg/n,dbn(xg/n),type="h",lty=2)
        axis(1,cex.axis=2); axis(2)
        if (input$tcl) {
          curve(dnorm(x,pg,sqrt(pg*(1-pg)/n)),add=T,n=1001,col=2,lwd=2)
          text(mean(xn)/n,.1,labels = max(abs(pnorm(xn/n,pg,sqrt(pg*(1-pg)/n))-cumsum(dbn(xn/n))/sum(dbn(xn/n)))))
          }
        }
      title(paste("x~Bin( pi=",pg,"n=",n,")"))
      } 
    else  if (input$dist=="pois") {
        n <- input$n
        ll <- input$ll
        xg<-0:(n*ll+10*sqrt(ll*n))
        dp <- function(x) dpois(round(x),n*ll)
        dpn<- function(x) n*dpois(round(x*n),n*ll)      
        if (!input$dn) {
          curve(dp,-.5,n*ll+10*sqrt(ll*n),type="s",axes=F,1001)
          points(xg-.5,dp(xg),type="h",lty=2)
          axis(1,cex.axis=1,xg); axis(2)
          if (input$tcl) curve(dnorm(x,n*ll,sqrt(n*ll)),add=T,n=1001,col=2,lwd=2)
        } 
        if (input$dn) {
          curve(dpn,-.5/n,ll+10*sqrt(ll/n),type="s",axes=F,1001)
          points((xg-.5)/n,dpn(xg/n),type="h",lty=2)
          axis(1,cex.axis=1,round(xg/n,1)); axis(2)
          if (input$tcl) curve(dnorm(x,ll,sqrt(ll/n)),add=T,n=1001,col=2,lwd=2)
        } 
        
        title(paste("x~Pois( lambda=",ll,"), n=",n))
      } 
    else  if (input$dist=="unif") {
      n <- input$n
      a <- input$ab[1]
      b <- input$ab[2]      
      fcu <- function(x){
        n_ <- floor((x-n*a)/(b-a))
        i <- 0:n_
        (factorial(n-1)*(b-a)^n)^(-1)*sum((-1)^i*choose(n,i)*(x-n*a-i*(b-a))^(n-1))*(x>=n*a & x <=n*b)
      }
      m <- (a+b)/2
      v <- (b-a)^2/12
      fcv <- Vectorize(fcu,vectorize.args = "x")
      fcn <- function(x) n*fcv(n*x)
      if (!input$dn) {
        curve(fcv,a*n,b*n,type="l",axes=F,1001)
        axis(1,cex.axis=2); axis(2)
        if (input$tcl) curve(dnorm(x,m*n,sqrt(n*v)),add=T,n=1001,col=2,lwd=2)
      } 
      if (input$dn) {
        curve(fcn,a,b,type="l",axes=F,1001)
        axis(1,cex.axis=2); axis(2)
        if (input$tcl) curve(dnorm(x,m,sqrt(v/n)),add=T,n=1001,col=2,lwd=2)
      } 
      title(paste("x~Unif( a",a,"b=",b,")"))
    } 
    else  if (input$dist=="esempio-lezione") {
        n <- input$n
        xs <- c(-1,0,1)
        brk <- c(min(xs)-.5,xs+.5)
        p <- c(1/3,1/3,1/3)
        P2 <- outer(p,p)
        X2 <- outer(xs,xs,"+")
        p1 <- function(x) 1/3*(round(x)%in%xs)
        p2 <- function(x) sum(P2[X2==round(x)],na.rm = T)
        p2n<- Vectorize(p2)
        if (!input$dn) {
          if (n == 1) {
            curve(p1,-1.6,1.6,1001)
            points(brk,p1(brk),type='h',lty=2)
          } else if (n == 2) {
            curve(p2n,-2.6,2.6,1001,ylim=c(0,.5))
            points(brk,p2n(brk),type='h',lty=2)
          } else  {
            ppn <- p2n
            for (i in 3:n){
              xn <- -(i-1):(i-1)
              brk <- c(min(xn)-.5,xn+.5)
              Pn <- outer(ppn(xn),p)
              X <- outer(xn,xs,"+")
              pp <- function(x) sum(Pn[X==round(x)],na.rm = T)
              ppn<- Vectorize(pp)
            }
          curve(ppn,-n,n,1001)
          points(brk,ppn(brk),type='h',lty=2)
          }
          if (input$tcl) {
            curve(dnorm(x,0,sqrt(2/3*n)),add=T,n=1001,col=2,lwd=2)
          }
        }
        if (input$dn) {
          if (n == 1) {
            curve(p1,-1.6,1.6,1001)
            points(brk,p1(brk),type='h',lty=2)
          } else if (n == 2) {
            p2n2 <- function(x) p2n(x*2)
            curve(p2n2,-1.6,1.6,1001,axes=F)
            points(brk/2,p2n2(brk/2),type='h',lty=2)
            axis(1,at=brk,round(brk/n,2))
            axis(2)
          } else  {
            ppn <- p2n
            for (i in 3:n){
              xn <- -(i-1):(i-1)
              brk <- c(min(xn)-.5,xn+.5)
              Pn <- outer(ppn(xn),p)
              X <- outer(xn,xs,"+")
              pp <- function(x) sum(Pn[X==round(x)],na.rm = T)
              ppn<- Vectorize(pp)
            }
            curve(ppn,-n,n,1001,axes=F)
            points(brk,ppn(brk),type='h',lty=2)
            axis(1,at=brk,round(brk/n,2))
            axis(2)
          }
          if (input$tcl) {
            curve(dnorm(x,0,sqrt(2/3*n)),add=T,n=1001,col=2,lwd=2)
          }
        }
        
        }  
  }) 
})
