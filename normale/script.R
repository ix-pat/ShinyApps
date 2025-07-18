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
  segments(it,pnorm(it,mean = mu,sd = ss),mu-5*ss,pnorm(it,mean = mu,sd = ss),lty=2)
   arrows(mu-4*ss+.2,pnorm(it,mean = mu,sd = ss)[1],mu-4*ss+.2,pnorm(it,mean = mu,sd = ss)[2],code = 3)
   text(mu-4*ss+1,mean(pnorm(it,mean = mu,sd = ss)),round(pnorm(it2,mean = mu,sd = ss)[2]-pnorm(it2,mean = mu,sd = ss)[1],5),cex=2,col=4)
   if (it2[1]==-Inf){  
     arrows(mu-4*ss+.2,pnorm(it,mean = mu,sd = ss)[2],mu-4*ss+.2,1,code = 3)
     text(mu-4*ss+1,(1+pnorm(it2[2]))/2,round(1-pnorm(it2,mean = mu,sd = ss)[2]-pnorm(it2,mean = mu,sd = ss)[1],5),cex=2,col=4)
     }
   it[1] <- ifelse(it[1] %in% -4:4,as.integer(it[1]),it[1])
   it[2] <-ifelse(it[2] %in% -4:4, as.integer(it[2]),it[2])
   axis(1,c(-4,0,4))
   axis(2,c(0,ff,1),las=2) 
   axis(1,it[1],col.axis=2); axis(1,it[2],col.axis=2)
   
   title(paste0("P(",it2[1],"< Z <",it2[2],")=",round(pnorm(it2[2])-pnorm(it2[1]),5)))
   
} else if(graf=="dens"){
  curve(dnorm(x,mean = mu,sd = ss),mu-4*ss,mu+4*ss,n=501,axes=F,xlab="z",ylab="φ(z), densità di Z")
  curve(dnorm(x,mean = mu,sd = ss),it[1],it[2],n=501,add=T,type="h",col="grey")
  it[1] <- ifelse(it[1] %in% -4:4,as.integer(it[1]),it[1])
  it[2] <-ifelse(it[2] %in% -4:4, as.integer(it[2]),it[2])
  axis(1,c(-4,0,4));axis(2); axis(1,it[1],col.axis=2); axis(1,it[2],col.axis=2)
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
  
  plot(c(0,11),c(0,42),type="n",axes=F,xlab="",ylab="")
  rect(0,(1:20)*2-.5,11,(1:20)*2+.5,col="grey90",border = NA)
  
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
