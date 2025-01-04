sss <- function(input,graf){
it <- input$it
gc <- input$gc
mu <- 0
ss <- 1
it2 <- it

if (round(it2[1])==-4)it2[1] <- -Inf
if (round(it[2])==4)it2[2] <- Inf
ff <- round(pnorm(it,mu,ss),4)

if (graf=="dist"){
  curve(pnorm(x,mean = mu,sd = ss),mu-4*ss,mu+4*ss,n=501,axes=F,xlab="z",ylab="Φ(z), Ripartizione di Z")
  abline(1,0,lty=2)
  curve(pnorm(x,mean = mu,sd = ss),it[1],it[2],n=501,add=T,lwd=2,col=2)
  segments(it,c(0,0),x1 = it,pnorm(it,mean = mu,sd = ss),lty=2)
  segments(it,pnorm(it,mean = mu,sd = ss),-4,pnorm(it,mean = mu,sd = ss),lty=2)
   arrows(mu-4*ss+.2,pnorm(it,mean = mu,sd = ss)[1],mu-4*ss+.2,pnorm(it,mean = mu,sd = ss)[2],code = 3)
   text(mu-4*ss+1,mean(pnorm(it,mean = mu,sd = ss)),round(pnorm(it2,mean = mu,sd = ss)[2]-pnorm(it2,mean = mu,sd = ss)[1],5),cex=2,col=4)
   if (it2[1]==-Inf){  
     arrows(mu-4*ss+.2,pnorm(it,mean = mu,sd = ss)[2],mu-4*ss+.2,1,code = 3)
     text(mu-4*ss+1,(1+pnorm(it2[2]))/2,round(1-pnorm(it2,mean = mu,sd = ss)[2]-pnorm(it2,mean = mu,sd = ss)[1],5),cex=2,col=4)
     }
   it[1] <- ifelse(it[1] %in% -4:4,as.integer(it[1]),it[1])
   it[2] <-ifelse(it[2] %in% -4:4, as.integer(it[2]),it[2])
   axis(1,c(-4,0,4))
   axis(2,c(0,ff,1),las=2,pos = -4) 
   axis(1,it[1],col.axis=2); axis(1,it[2],col.axis=2)
   
   title(paste0("P(",it2[1],"< Z <",it2[2],")=",round(pnorm(it2[2])-pnorm(it2[1]),5)))
   
} else if(graf=="dens"){
  curve(dnorm(x,mean = mu,sd = ss),mu-4*ss,mu+4*ss,n=501,axes=F,xlab="z",ylab="φ(z), densità di Z")
  curve(dnorm(x,mean = mu,sd = ss),it[1],it[2],n=501,add=T,type="h",col="grey")
  it[1] <- ifelse(it[1] %in% -4:4,as.integer(it[1]),it[1])
  it[2] <-ifelse(it[2] %in% -4:4, as.integer(it[2]),it[2])
  axis(1,c(-4,0,4));axis(2); axis(1,it[1],col.axis=2); axis(1,it[2],col.axis=2)
  if (it[1]==-4){
    text((-4+it[2])/2,.05,round(pnorm(it[2]),digits = 5),col=4,cex=3)
    text((+4+it[2])/2,.05,round(1-pnorm(it[2]),digits = 5),col=4,cex=3)
  } else if (it[2]==+4){
    text((-4+it[1])/2,.05,round(pnorm(it[1]),digits = 5),col=4,cex=3)
    text((+4+it[1])/2,.05,round(1-pnorm(it[1]),digits = 5),col=4,cex=3)
  } else {
    text((it[1]+it[2])/2,.05,round(pnorm(it[2])-pnorm(it[1]),digits = 5),col=4,cex=3)
  }
  if (it[2] >=0 & it2[1]==-Inf & it2[2] != Inf) {
    title(paste0("Uso delle tavole statistiche, Φ(",it2[2],")=",format(pnorm(abs(it2[2])),digits=5,nsmall=5)  ))
  } else if (it[2] <0 & it2[1]==-Inf & it2[2] != Inf) {
    title(paste0("Uso delle tavole statistiche, Φ(",it[2],")=1-Φ(",-it2[2],")=1-",format(pnorm(-it[2]),digits=5,nsmall=5),"=",format(pnorm(it[2]),digits=5,nsmall=5)  ))
  } else if (it[2] >=0 & it[1]>=0 & it2[1]!=-Inf & it2[2] != Inf) {
    title(
      paste0(
        "P(",it[1],"< Z <",it[2],")=Φ(",it[2],")-Φ(",it2[1],")=",format(pnorm(it[2]),digits=5,nsmall=5),"-",format(pnorm(it2[1]),digits=5,nsmall=5),"=",round(pnorm(it[2]),5)-round(pnorm(it[1]),5)
      )
    )
  } else if (it[1] <0 & it[2]<0 & it2[1]!=-Inf & it2[2] != Inf) {
    title(
      paste0(
        "P(",it[1],"< Z <",it[2],")=(1-Φ(",-it[2],"))-(1-Φ(",-it[1],"))=(1-",format(pnorm(-it[2]),digits=5,nsmall=5),")-(1-",format(pnorm(-it[1]),digits=5,nsmall=5),")=",round(pnorm(it[2]),5)-round(pnorm(it[1]),5)
      )
    )
  } else if (it[1] <0 & it[2]>=0  & it2[1]!=-Inf & it2[2] != Inf) {
    title(
      paste0(
        "P(",it[1],"< Z <",it[2],")=Φ(",it[2],")=Φ(",it[2],")-(1-Φ(",-it[1],"))=",format(pnorm(it[2]),digits=5,nsmall=5),"-(1-",format(pnorm(-it[1]),digits=5,nsmall=5),")=",round(pnorm(it[2]),5)-round(pnorm(it[1]),5)
      )
    )
  } else if (it[1] >0 & it2[1]!=-Inf & it2[2] == Inf) {
    title(
      paste0(
        "P(Z >",it[1],")=1-Φ(",it[1],")=1-",format(pnorm(it[1]),digits=5,nsmall=5),"=1-",format(pnorm(it[1]),digits=5,nsmall=5),")=",1-round(pnorm(it[1]),5)
      )
    )
  }  else if (it[1] <0 & it2[1]!=-Inf & it2[2] == Inf) {
    title(
      paste0(
        "P(Z >",it[1],")=1-(1-Φ(",-it[1],"))=1-(1-(",format(pnorm(it[1]),digits=5,nsmall=5),"))=",format(pnorm(-it[1]),digits=5,nsmall=5)
      )
    )
  }
  
  } else if(graf=="tab"){
  colore1 <- ifelse(it[1]<0,3,5)
  colore2 <- ifelse(it[2]<0,2,4)
  big <- round(seq(0, 3.9, by = 0.1),1)
  little <- round(seq(0, 0.09, by = 0.01),2)
  norm_table <- outer(big, little, function(x,y) pnorm(x+y))
  row.names(norm_table) <- format(big, digits=1)      # format() forces the digits we want
  colnames(norm_table) <- format(little, digits=2)
  int1 <- floor(abs(it[1])*10)/10
  dec1 <- abs(it[1])-int1
  int2 <- floor(abs(it[2])*10)/10
  dec2 <- abs(it[2])-int2
  inr1 <- which(big==round(int1,1))
  inc1 <- which(little==round(dec1,2))
  inr2 <- which(big==round(int2,1))
  inc2 <- which(little==round(dec2,2))
  
  ex <- expand.grid(little,big)
  length(big)
  i <- 40-(1:40)
  j <- (1:10)
  
  ij <- expand.grid(j,i)
  alt <- .5
  
  plot(c(0,11),c(0,42),type="n",axes=F,xlab="",ylab="")
  rect(0,(1:20)*2-alt,11,(1:20)*2+alt,col="grey90",border = NA)
  
  text(ij[,1],ij[,2]+1,(format(pnorm(ex[,1]+ex[,2]),digits = 5)),cex = gc,adj = 0)
  
  # Intestazioni
  text(0,40:1,format((big),digits = 2),cex = gc,adj = c(0,.5))
  text(1:10,42.5,format(little,digits = 2),cex = gc,adj=0)
  abline(v=.5)
  abline(h=41)
  
  if (it2[2] != Inf){
  text(inc2,40:1,format(pnorm(little[inc2]+big),digits = 5),col=colore2,cex = gc,adj = 0)
  text(1:10,40-inr2+1,format(pnorm(little+big[inr2]),digits = 5),col=colore2,cex = gc,adj = 0)
  }
  if (it2[1] != -Inf){
  text(inc1,40:1,format(pnorm(little[inc1]+big),digits = 5),col=colore1,cex = gc,adj = 0)
  text(1:10,40-inr1+1,format(pnorm(little+big[inr1]),digits = 5),col=colore1,cex = gc,adj = 0)
  }
  
  }
}

n_int <- function(x1,x2,mm,ss){
  ### Calcola la probabilità di una Normale qualunque su un intervallo
  ##  Input
  ##  x1 limite inferiore
  ##  x2 limite superiore
  ##  mm media
  ##  ss varianza
  ##  vnam nome X
  ##  mu nome media
  ##  sigma noem varianza
  vnam <- ifelse(mm == 0 & ss == 1,"Z","X")
  mu <- "\\mu"
  sigma <- "\\sigma"
  z1 <- round((x1 - mm)/sqrt(ss),2)
  z2 <- round((x2 - mm)/sqrt(ss),2)
  f1 <- round(pnorm(abs(z1)),4)
  f2 <- round(pnorm(abs(z2)),4)
  res <- round(pnorm(z2),4)-round(pnorm(z1),4)
  
  if (z1 <=-4 & z2 >= 4) {paste("\\Large P(-\\infty<",vnam,"\\le+\\infty)=1") } else if (z1> -4 & z2 < 4){  
  
  if (x1<=mm & x2>=mm) {
    p1 <- paste("\\Phi(", z2,")-(1-\\Phi(",-z1,")) \\\\", "&=& ",f2,"-(1-",f1,") \\\\")}
  if (x1>=mm & x2>=mm) {
    p1 <- paste(f2,"-",f1,"\\\\")
  }
  if (x1<=mm & x2<mm) {
    p1 <- paste("(1-\\Phi(", -z2,"))-(1-\\Phi(",-z1,")) \\\\","&=& (1-",f2,")-(1-",f1,") \\\\")
  }
  
  mm <- ifelse(mm>=0,mm,paste("(",mm,")"))
  
  paste("\\Large\\begin{eqnarray*}
P(", x1 , "<", vnam , "\\leq ", x2 , ") &=& P\\left( \\frac {", x1 , " - ", mm , "}{\\sqrt{", ss , "}} < \\frac {", vnam , " - ", mu , "}{", sigma , "} \\leq \\frac {", x2 , " - ", mm , "}{\\sqrt{", ss , "}}\\right)  \\\\
              &=& P\\left( ", z1 , " < Z \\leq ", z2 , "\\right) \\\\
              &=& \\Phi(", z2 , ")-\\Phi(", z1 , ")\\\\
              &=& ", p1 , "
              &=& ", res , "
\\end{eqnarray*}"  )} else {
  verso <- ifelse(z1 <= -4,"<",">")
  x1 <- ifelse(verso == "<",x2,x1)
  z1 <- round((x1 - mm)/sqrt(ss),2)
  f1 <- round(pnorm(z1,lower.tail = (verso == "<")),4)
  
  if (verso == "<") {
    p1 <- ifelse(x1 >=mm, 
                 paste("\\Phi(", z1,") \\\\","&=& ",f1),
                 paste("1-\\Phi(", -z1,") \\\\","&=& ",f1))
    p0 <- ""
  }
  if (verso == ">") {
    p1 <- ifelse(x1 >=mm, 
                 paste("1-\\Phi(", z1,") \\\\", "&=& ",f1),
                 paste("1-(1-\\Phi(", -z1,")) \\\\", "&=& ",f1))
    p0 <- paste(" &=& 1-P(Z<",z1,")\\\\")
  }
  
  mm <- ifelse(mm>=0,mm,paste("(",mm,")"))
  res <- f1
paste("
      \\Large\\begin{eqnarray*}
P(", vnam , " ", verso , " ", x1 , ") 
  &=& P\\left(  \\frac {", vnam , " - ", mu , "}{", sigma , "} ", verso , " \\frac {", x1 , " - ", mm , "}{\\sqrt{", ss , "}} \\right)  \\\\
              &=& P\\left(  Z  ", verso , " ", z1 , "\\right) \\\\  ", p0 , "
              &=& ", p1 , "
\\end{eqnarray*}
")  
}
}

b_int <- function(x1,n,pp,verso="\\leq",comp=FALSE,sing=FALSE,x0=0,vnam="X",size=""){
  ### Calcola la probabilità di una Binomiale P(X<=x) 
  ##  Input
  ##  x0 limite inf, se diverso da zero usare comp=F
  ##  x1 limite sup
  ##  verso ("\\leq" o "\\geq")
  ##  n numero prove
  ##  pp probabilità vittoria
  ##  vnam nome X
  
  ver_c <- ifelse(verso=="\\leq",">","<")
  sing <- verso == "="
  pp <- round(pp,4)
  
  if (sing){
    p0 <- paste("P(",vnam,"=",x1,") &=&","\\binom{",n,"}{",x1,"}",pp,"^{",x1,"}(1-",pp,")^{",n,"-",x1,"}","\\\\")
    p1 <- paste("                     &=&",choose(n,x1),"\\times",pp,"^{",x1,"}(1-",pp,")^{",n-x1,"}","\\\\")
    p2 <- paste("                     &=&",round(dbinom(x1,n,pp),4))
    res <- paste(p0,p1,p2)

    return(paste("\\Large\\begin{eqnarray*}",res,"\\end{eqnarray*}"))
    
  }
  if (!sing){
    if (!comp){
      if (verso == "\\leq")  xx <- x0:x1 else xx <- x1:n
      c0 <- paste("P(",vnam,verso,x1,") &=&",paste("\\binom{",n,"}{",xx,"}",pp,"^{",xx,"}(1-",pp,")^{",n,"-",xx,"}",collapse = "+"),"\\\\")
      c1 <- paste("                     &=&",paste(round(dbinom(xx,n,pp),4),collapse = "+"),"\\\\")
      c2 <- paste("                     &=&",sum(round(dbinom(xx,n,pp),4)))
      res <- paste(c0,c1,c2)
      return(paste("\\Large\\begin{eqnarray*}
                   ",res,"
                   \\end{eqnarray*}"))
    } 
    if (comp) {
      if (verso == "\\leq")  xx <- (x1+1):n else xx <- 0:(x1-1)
      c00<- paste("P(",vnam,verso,x1,") &=&","1-P(",vnam,ver_c,x1,")","\\\\")
      c0 <- paste("                     &=& 1-\\left(",paste("\\binom{",n,"}{",xx,"}",pp,"^{",xx,"}(1-",pp,")^{",n,"-",xx,"}",collapse = "+"),"\\right)\\\\")
      c1 <- paste("                     &=& 1-(",paste(round(dbinom(xx,n,pp),4),collapse = "+"),")\\\\")
      c2 <- paste("                     &=& 1-",sum(round(dbinom(xx,n,pp),4)),"\\\\")
      c3 <- paste("                     &=&   ",1-sum(round(dbinom(xx,n,pp),4)))
      res <- paste(c00,c0,c1,c2,c3)  
      return(paste("\\Large\\begin{eqnarray*}
                   ",res,"\\end{eqnarray*}
                   "))
    } 
  }
  
}

binom <- function(n,p){
  x <- 0:n
  xg <- c(-.5,x+.5)
  m <- n+2
  y  <- dbinom(x,n,p)
  plot(x,y,axes = F,type="n",ylim = c(0,max(y)),xlim = c(-.5,n+.5),ylab = "P(X=x)")
  rect(xg[1:(m-1)],0,xg[2:m],y,density = 20,col = "blue4")
  axis(2)
  axis(1,0:n)
}

pois <- function(l){
  x <- 0:qpois(.9999,l)
  n <- length(x)
  m <- n + 1
  xg <- c(-.5,x+.5)
  y  <- dpois(x,l)
  plot(x,y,axes = F,type="n",ylim = c(0,max(y)),xlim = c(-.5,n+.5),ylab = "P(X=x)")
  rect(xg[1:(m-1)],0,xg[2:m],y,density = 20,col = "blue4")
  axis(2)
  axis(1,0:(n-1))
}

p_int <- function(x1,ll,verso="\\leq",sing=FALSE,vnam="X"){
  ### Calcola la probabilità di una Binomiale P(X<=x) 
  ##  Input
  ##  x0 limite inf, se diverso da zero usare comp=F
  ##  x1 limite sup
  ##  verso ("\\leq" o "\\geq")
  ##  ll lambda
  ##  vnam nome X
  
  ver_c <- ifelse(verso=="\\leq",">","<")
  sing <- verso == "="
  if (sing){
    p0 <- paste("P(",vnam,"=",x1,")   &=&","\\frac{",ll,"^{",x1,"}}{",x1,"!}e^{-",ll,"}\\\\")
    p1 <- paste("                     &=&",round(ll^x1/factorial(x1),4),"\\times",round(exp(-ll),4),"\\\\")
    p2 <- paste("                     &=&",round(dpois(x1,ll),4))
    res <- paste(p0,p1,p2)
    return(paste("\\Large\\begin{eqnarray*} ",res,"\\end{eqnarray*} "))
    
  }
  if (!sing){
    if (verso == "\\leq"){
      xx <- 0:x1 
      c0 <- paste("P(",vnam,verso,x1,") &=&",paste("\\frac{",ll,"^{",xx,"}}{",xx,"!}e^{-",ll,"}",collapse = "+"),"\\\\")
      c1 <- paste("                     &=&",paste(round(dpois(xx,ll),4),collapse = "+"),"\\\\")
      c2 <- paste("                     &=&",sum(round(dpois(xx,ll),4)))
      res <- paste(c0,c1,c2)
      return(paste("\\Large\\begin{eqnarray*} ",res,"\\end{eqnarray*} "))
    } else {
      xx <- 0:(x1-1)
      c00<- paste("P(",vnam,verso,x1,") &=&","1-P(",vnam,ver_c,x1,")","\\\\")
      c0 <- paste("                     &=& 1-\\left(",paste("\\frac{",ll,"^{",xx,"}}{",xx,"!}e^{-",ll,"}",collapse = "+"),"\\right)\\\\")
      c1 <- paste("                     &=& 1-(",paste(round(dpois(xx,ll),4),collapse = "+"),")\\\\")
      c2 <- paste("                     &=& 1-",sum(round(dpois(xx,ll),4)),"\\\\")
      c3 <- paste("                     &=&   ",1-sum(round(dpois(xx,ll),4)))
      res <- paste(c00,c0,c1,c2,c3)  
      return(paste("\\Large\\begin{eqnarray*} ",res,"\\end{eqnarray*} "))
    } 
  }
}


two_way <- function(S_1,S_2,num1,num2,op=`+`,EV=T,vnam="X",size=""){
  den1 <- rep(sum(num1),times=length(S_1))
  den2 <- rep(sum(num2),times=length(S_2))
  k1 <- length(S_1)
  k2 <- length(S_2)
  P_1 <- num1/den1
  P_2 <- num2/den2
  html <- T
  
  SS <- round(outer(S_2,S_1,op),4)
  p1 <- P_1
  NN <- outer(num2,num1)
  DD <- outer(den2,den1)
  
  frc  <- ifelse(html,"\\frac","\\sfrac")  
  hed  <- ifelse(html,"\\[\n","\\[\\arraycolsep=10pt\\def\\arraystretch{1}")  
  frac <- function(i,j){
    paste(round(op(S_2[i],S_1[j]),4),";&","\\color{red}{",frc,"{",num2[i]*num1[j],"}{",den2[i]*den1[j],"}}",sep="")
  }
  
  mat1 <- outer(1:length(S_2),1:length(S_1),frac)
  mat1 <- cbind(paste(S_2,";\\color{blue}{",num2,"/",den2,"}"),mat1)
  cols <- paste("Y \\backslash X ",
    paste(
      paste("&",
            paste(
              paste(S_1,";"),
              paste("{",num1,"}"),sep=
                paste("&\\color{blue}{", frc))),
                paste("{",den1,"}}"),collapse = " "),"\\\\ \n")
  
  allign <- paste("r|",paste(rep("r",times=2*k1),collapse = ""),sep="",collapse = "")
  
  c0 <-  paste(size,"\n\n")
  c1 <- paste(hed,"\\begin{array}",paste("{",allign,"}\n"),
      cols,"\\hline \n",
      paste(apply(mat1,1,function(x)c(paste(x,collapse = "& "),"\\\\ \n")),collapse = ""),
      "\\end{array}\n \\]\n\n",sep ="")
  
  c2 <- paste("E ricaviamo la distribuzione di,",vnam,"\n\n")
  
  S_3 <- round(sort(unique(as.numeric(outer(S_2,S_1,op)))),4)
  num3 <- 1:length(S_3)
  den3 <- 1:length(S_3)
  for (i in 1:length(S_3)){
    num3[i] <- sum(NN[SS==S_3[i]])
    den3[i] <- DD[SS==S_3[i]][1]
  }
  
  k3 <- length(S_3)
  allign <- paste("r|",paste(rep("r",times=k3),collapse = ""),sep="",collapse = "")
  rig1 <- paste(vnam," &",paste(S_3,collapse = "& "),"\\\\ \n")
  rig2 <- paste("P(",vnam,") &",paste(frc,"{",num3,"}","{",den3,"}",collapse = "& ",sep=""))
  c3 <- paste(size,"\n\n")
  c4 <- paste("\\[
      \\begin{array}{",allign,"}\n",
      rig1,"\\hline \n",
      rig2,
      "\\\\ \n \\end{array}\n \\]\n")
  urn <- rep(S_3,times=num3)
  if (EV){    
    c5 <- paste("Calcoliamo valore atteso e varianza\n\n")
    c6 <- paste(size,"\n\n")
    c7 <- paste(stat_(urn,semp = T),"\n\n")
  }
  return(list(c0 = c0,c1=c1,c2=c2,c3=c3,c4=c4,c5=c5,c6=c6,c7=c7))
}

stat_ <- function(x,p=NULL,mnam="\\mu",vnam="\\sigma^2",semp=F){
  n <- length(x)
  m <- round(ifelse(test = is.null(p),mean(x),sum(x*p)),4)
  p1 <- character(n)
  p2 <- character(n)
  p1[x<0]<-"("
  p2[x<0]<-")"
  xp <- paste(p1,x,p2)
  if (!semp){
    if(is.null(p)){
      paste("\\begin{eqnarray*}\n",
          mnam,"&=& \\frac 1{",n,"}(",paste(xp,collapse = "+"),")=",m,"\\\\ \n",
          vnam,"&=& \\frac 1{",n,"}(",paste(paste(xp,2,sep = "^"),collapse = "+"),")-(",m,")^2=",round(s2c(x),4),
          "\n\\end{eqnarray*}\n")
    }
    if(!is.null(p)){
      p <- p/sum(p)
      pp <- round(p,4)
      paste("\\begin{eqnarray*}\n",
          mnam,"&=&",paste(paste(xp,pp,sep = " \\cdot "),collapse = "+"),"=",sum(x*p),"\\\\",
          vnam,"&=&(",paste(paste(paste(xp,2,sep = "^"),pp,sep = " \\cdot "),collapse = "+"),")-(",m,")^2=",vvv(x = x,p = p),
          "\\end{eqnarray*}")
    }} else {
      freq <- table(x)
      xx <- dimnames(freq)$x
      p1 <- character(length(xx))
      p2 <- character(length(xx))
      p1[xx<0]<-"("
      p2[xx<0]<-")"
      xx <- paste(p1,xx,p2)
      paste("\\begin{eqnarray*}",
          mnam,"&=& E(W) = \\sum_{w\\in S_W}x P(W=w)\\\\ \n",
          "&=&",paste(paste(xx,"\\frac {",freq,"}{",n,"}"),collapse = "+"),"\\\\ 
                &=&",round(mean(x),4),"\\\\ \n",
          vnam,"&=& V(W) = \\sum_{w\\in S_W}w^2 P(X=w)-\\mu^2\\\\ \n",
          "&=&\\left(",paste(paste(xx,"^2\\frac {",freq,"}{",n,"}"),collapse = "+"),"\\right)-(",m,")^2\\\\ 
                &=&",round(s2c(x),4),
          "\n\\end{eqnarray*}\n")
    }
}


s2c <- function(x) {(mean(x^2)-mean(x)^2)}  # varianza di pop
sc  <- function(x) {sqrt(s2c(x))}           # sd di pop


vvv <- function(x,p=NULL) {               # varianza per distr tabella e prob
  if (is.null(p)) v <- mean(x^2)-mean(x)^2
  else v <- sum(p*x^2)-(sum(p*x))^2
  return(v)
}
