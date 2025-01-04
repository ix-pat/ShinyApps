source("main-functions.R")
library(colorspace)


genera_dati <- function(brk,nnn){
  k <- length(brk)
  brk1 <- brk[1:(k-1)]
  brk2 <- brk[2:(k)]
  brk_c <- (brk1+brk2)/2
  return(rep(brk_c,times=nnn))
}

iblue <- darken(rgb(0.024,0.282,0.478),amount = .4)
mblue <- rgb(0.024,0.282,0.478)
ablue <- rgb(0.729,0.824,0.878)
ared  <- rgb(0.671,0.161,0.18)

dens <- function(brk,nnn,x,per,pp){
  set.seed(1)
  n <- sum(nnn)
  samp <- genera_dati(brk = brk,nnn = nnn)
  source("stat-base.R",local = T)
  print(per)
  if (!per){
      x1 <- round(x[1],2)
      x2 <- round(x[2],2)
      histp(axes = T,xlim=c(min(brk)-1,max(brk)+1))
      h.int(x1,x2,density=20,col=ared)
      axis(1,x1,col.axis=2)
      axis(1,x2,col.axis=2)
    } else {
      x1 <- brk[1]
      x2 <- round(Q.int(pp),4)
      histp(axes = T,xlim=c(min(brk)-1,max(brk)+1))
      h.int(x1,x2,density=20,col=iblue)
      axis(1,x1,col.axis=2)
      axis(1,x2,col.axis=2)
    }
  }

rip <- function(brk,nnn,x,per,pp){
  set.seed(1)
  n <- sum(nnn)
  samp <- genera_dati(brk = brk,nnn = nnn)
  source("stat-base.R",local = T)
  if (!per){  
    x1 <- round(x[1],2)
    x2 <- round(x[2],2)
    plot(c(min(brk)-1,brk,max(brk)+1),c(0,0,dat2$Fj,1),type="l",axes=F,xlab="x",ylab="F(x)")
    axis(1,brk)
    axis(2,c(0,dat2$Fj),round(c(0,dat2$Fj),2),las=2,col.axis="grey")
    axis(1,x1,col.axis=2)
    axis(1,x2,col.axis=2)
    axis(2,F.int(c(x1,x2)),round(F.int(c(x1,x2)),2),col.axis=ared,las=2)
    segments(x0 = -10,x1 = dat2$xsup,y0 = dat2$Fj,y1 = dat2$Fj,lty=2,col="grey")
    segments(x0 = dat2$xsup,x1 = dat2$xsup,y0 = 0,y1 = dat2$Fj,lty=2,col="grey")
    segments(x0 = x2,x1 = x2,y0 = -1,y1 = F.int(x2),lty=2,col=ared)  
    segments(x0 = min(brk)-10,x1 = x2,y0 = F.int(x2),y1 = F.int(x2),lty=2,col=ared)  
    segments(x0 = x1,x1 = x1,y0 = -1,y1 = F.int(x1),lty=2,col=ared)  
    segments(x0 = min(brk)-10,x1 = x1,y0 = F.int(x1),y1 = F.int(x1),lty=2,col=ared)  
    arrows(x0 = min(brk)-1,y0 = F.int(x1),x1 = min(brk)-1,y1 = F.int(x2),code = 3)
    text(x = min(brk), y=(F.int(x1)+F.int(x2))*.5,labels=paste(round(100*(F.int(x2)-F.int(x1)),2),"%",sep=""))
  } else {
    x1 <- brk[1]
    x2 <- round(Q.int(pp),4)
    plot(c(min(brk)-1,brk,max(brk)+1),c(0,0,dat2$Fj,1),type="l",axes=F,xlab="x",ylab="F(x)")
    axis(1,brk)
    axis(2,c(0,dat2$Fj),round(c(0,dat2$Fj),2),las=2,col.axis="grey")
    axis(1,x1,col.axis=2)
    axis(1,x2,col.axis=2)
    axis(2,F.int(c(x1,x2)),round(F.int(c(x1,x2)),2),col.axis=ared,las=2)
    segments(x0 = -10,x1 = dat2$xsup,y0 = dat2$Fj,y1 = dat2$Fj,lty=2,col="grey")
    segments(x0 = dat2$xsup,x1 = dat2$xsup,y0 = 0,y1 = dat2$Fj,lty=2,col="grey")
    segments(x0 = x2,x1 = x2,y0 = -1,y1 = F.int(x2),lty=2,col=ared)  
    segments(x0 = min(brk)-10,x1 = x2,y0 = F.int(x2),y1 = F.int(x2),lty=2,col=ared)  
    segments(x0 = x1,x1 = x1,y0 = -1,y1 = F.int(x1),lty=2,col=ared)  
    segments(x0 = min(brk)-10,x1 = x1,y0 = F.int(x1),y1 = F.int(x1),lty=2,col=ared)  
    arrows(x0 = min(brk)-1,y0 = F.int(x1),x1 = min(brk)-1,y1 = F.int(x2),code = 3)
    text(x = min(brk), y=(F.int(x1)+F.int(x2))*.5,labels=paste(round(100*(F.int(x2)-F.int(x1)),2),"%",sep=""))
  }
}
