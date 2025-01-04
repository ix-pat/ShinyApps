##################
# Crea la Tabella
##################

k <- length(brk)-1
br1  <- brk[-(k+1)]
br2  <- brk[-1]

K <- length(brk)
dat2 <- data.frame(
  xinf = brk[1:(K-1)],
  xsup = brk[2:(K)],
  nj   = as.data.frame(table(cut(samp,brk)))$Freq
)


dat2$fj <- (dat2$nj/sum(dat2$nj))
dat2$bj <- (dat2$xsup-dat2$xinf)
dat2$hj <- (dat2$fj/dat2$bj*100)
dat2$Fj <- cumsum(dat2$fj)
dat2$x  <- apply(dat2[,1:2],1,mean)
dat2$x2 <- dat2$x^2
dat2$xn <- dat2$x *dat2$nj
dat2$x2n<- dat2$x2*dat2$nj
dat3 <- dat2
dat3 <- rbind(dat3,colSums(dat3))
dat3[K,c(1:2,6:9)] <- NA

names(dat3) <- c("$[\\text{x}_j,$","$\\text{x}_{j+1})$","$n_j$","$f_j$","$b_j$","$h_j$","$F_j$","$\\bar{\\text{x}}_j$","$\\bar{\\text{x}}_j^2$","$\\bar{\\text{x}}_jn_j$","$\\bar{\\text{x}}_j^2 n_j$")
dat3$`$f_{j\\%}$` <- dat3$`$f_j$`*100

perc <- dat2$xinf[-1]
sper <- ""
for (i in 1:(K-2)) sper <- c(sper,paste("$x_{",dat2$Fj[i],"}=",perc[i],"$"))
# dgl <- sapply(dat2,is.integer)
# dg <- numeric(dim(dat2)[2]+1) + 2
# dg[3]<- 0

Q.int <- approxfun(c(0,dat2$Fj),brk)
F.int <- approxfun(c(-1e10,brk,1e10),c(0,0,dat2$Fj,1))
H.int <- approxfun(c(min(-100,min(brk)-1),brk,max(100,max(brk)+1)),c(0,dat2$hj,0,0),method = "constant",yleft = 0,yright = 0)
h.int <- function(x1,x2,density=20,...){
  brtemp <- c(x1,brk[brk>x1 & brk<x2],x2)
  kk <- length(brtemp)
  brs <- sort(c(min(brtemp),rep(brtemp,each=2),max(brtemp)))
  
  hrs <- c(0,0,rep(H.int(brtemp[-(kk)]),each=2),0,0)
  
  kk <- length(brs)
  polygon(brs,hrs,density=density,...)
  lines(brs,hrs,...)
}

F_print <- function(x,verso="<",x2=0){
  datp <- round(dat2,4)
  if (verso == "<"){
  j <- max(which(brk <= x))
      if(j==1) {
      cat("\\begin{eqnarray*}
      \\%(X<",x,") &=&",x,"\\times h_1 \\\\
                   &=&",x,"\\times ",datp$hj[1],"\\\\
                   &=& ",F.int(x),"\\times(100) \\\\
      \\#(X<",x,") &=&",F.int(x)*n,"
            \\end{eqnarray*}")
        } else {
    cat("\\begin{eqnarray*}
      \\%(X<",x,") &=& ",paste("f_{",1:(j-1),"}\\times 100",collapse="+"),"+(",x,"-",brk[j],")\\times h_{",j,"} \\\\
                   &=& ",paste("(",datp$fj[1:(j-1)],")\\times 100",collapse="+"),"+(",x-brk[j],")\\times ",datp$hj[j]," \\\\
                   &=& ",F.int(x),"\\times(100) \\\\
      \\#(X<",x,") &=&",F.int(x)*n,"
            \\end{eqnarray*}")
        }
  } else if (verso == ">") {
    j <- min(which(brk >= x))
    if(j==k+1) {
      cat("\\begin{eqnarray*}
      \\%(X>",x,") &=&(",brk[j],"-",x,")\\times h_1 \\\\
                   &=&",brk[j]-x,"\\times ",datp$hj[k],"\\\\
                   &=& ",1-F.int(x),"\\times(100)\\\\
      \\#(X>",x,") &=&",(1-F.int(x))*n,"
            \\end{eqnarray*}")
    } else {
      cat("\\begin{eqnarray*}
      \\%(X>",x,") &=& (",brk[j],"-",x,")\\times h_{",j-1,"}+",paste("f_{",(j):(k),"}\\times 100",collapse="+"),"\\\\
                   &=& (",brk[j]-x,")\\times",datp$hj[j-1],"+",paste("(",datp$fj[(j):(k)],")\\times 100",collapse="+"), "\\\\
                   &=& ",1-F.int(x),"\\times(100)\\\\
      \\#(X>",x,") &=&",(1-F.int(x))*n,"
            \\end{eqnarray*}")
    }
  } else  {
    j1 <- max(which(br1 <= x))
    j2 <- min(which(br2 >= x2))
    cat("\\begin{eqnarray*}
      \\%(",x,"<X<",x2,") &=& (",brk[j1+1],"-",x,")\\times h_{",j1,"}+",paste("f_{",(j1+1):(j2-1),"}\\times 100",collapse="+"),"+(",x2,"-",brk[j2],")\\times h_{",j2-1,"} \\\\
                   &=& (",brk[j1+1]-x,")\\times ",datp$hj[j1],"+",paste("(",datp$fj[(j1+1):(j2-1)],")\\times 100",collapse="+"),"+(",x2-brk[j2],")\\times ",datp$hj[j2]," \\\\
                   &=& ",F.int(x2)-F.int(x),"\\times(100)\\\\
      \\#(",x,"< X <",x2,") &=&",(F.int(x2)-F.int(x))*n,"
            \\end{eqnarray*}")
  }
}


histp <- function(axes=F,...){ 
  if (!exists("nomex")) nomex <- ""
  plot(range(brk),range(c(0,dat2$hj),na.rm = T),type="n",axes=F,xlab = nomex,ylab = "Denistà percentuale")
  rect(xleft = br1,ybottom = 0,xright = br2,ytop = dat2$hj,...)
  if (axes){
  axis(1,brk)
  axis(2,c(0,dat2$hj),c(0,round(dat2$hj,2)),las=2)}
  }
