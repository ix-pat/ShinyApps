# Funzioni Usate nel libro ####
# Patrizio Frederic ####

#_______________________ ####

## funzioni speciali ####

srt <- "/home/pat/OneDrive/Stat/src/"
src_ <- function(x) {paste(srt,x,sep = "")}

tabl <- function(x,...){
  kable(x,...,booktabs=T,escape = F,linesep="") %>%
    kable_styling(full_width = F, latex_options = "HOLD_position")
}

### Punto elenco personalizzato numero.lettera 

item <- function(){
  it <- (paste(i1,".",letters[i2],sep = ""))
  return(it)}

### Funzione parentesi 

p <- function(x,ax=4){
  p1 <- ifelse(x < 0,"(","")
  p2 <- ifelse(x < 0,")","")
  paste(p1,round(x,ax),p2,sep="")
}


## Statistica Descrittiva ####

s2c <- function(x) {(mean(x^2)-mean(x)^2)}  # varianza di pop
sc  <- function(x) {sqrt(s2c(x))}           # sd di pop


vvv <- function(x,p=NULL) {               # varianza per distr tabella e prob
  if (is.null(p)) v <- mean(x^2)-mean(x)^2
  else v <- sum(p*x^2)-(sum(p*x))^2
  return(v)
}

vunif <- function(nnn, brk){              # genera i dati da una mistura di uniformi
  k <- length(brk)-1
  br1  <- brk[-(k+1)]
  br2  <- brk[-1]
  xi   <- runif(nnn[1],br1[1],br2[1])
  for (i in 2:k)
    xi <- c(xi,runif(nnn[i],br1[i],br2[i]))
  return(xi)
}

genera_dati <- function(brk,hhh=NULL,n,nnn=NULL){       # genera i dati fornendo brk, hhh, e n
  if (is.null(nnn))  nnn  <- round((hhh*diff(brk))/sum(hhh*diff(brk))*n) # riporto ad n
  if (!is.null(nnn)) nnn  <- round(nnn/sum(nnn)*n) # riporto ad n
  k <- length(brk)-1
  br1  <- brk[-(k+1)]
  br2  <- brk[-1]
  
  samp <- round(vunif(nnn,brk),2)
  
#  names(samp) <- nomex
  return(samp)
}

media_ <- function(x,p=NULL,mnam="\\mu",semp = F){
  n <- length(x)
  if (!semp){
  if(is.null(p)){
    cat("\\[",mnam,"=\\frac 1{",n,"}(",paste(x,collapse = "+"),")=",mean(x),"\\]")
  }
  if(!is.null(p)){
    
    cat("\\[",mnam,"=",paste(paste(x,p,sep = " \\cdot "),collapse = "+"),"=",sum(x*p),"\\]")
  }} else {
    freq <- table(x)
    xx <- dimnames(freq)$x
    cat("\\[",mnam,"=\\left(",paste(paste(xx,"\\frac {",freq,"}{",n,"}"),collapse = "+"),"\\right)=",mean(x),"\\]")
  }
}

var_ <- function(x,p=NULL,mnam="\\sigma^2",semp=F){
  n <- length(x)
  m <- ifelse(test = is.null(p),mean(x),sum(x*p))
  if (!semp){
    if(is.null(p)){
    cat("\\[",mnam,"=\\frac 1{",n,"}(",paste(paste(x,2,sep = "^"),collapse = "+"),")-(",m,")^2=",s2c(x),"\\]")
  }
  if(!is.null(p)){
    p <- sum(p)
    cat("\\[",mnam,"=(",paste(paste(paste(x,2,sep = "^"),p,sep = " \\cdot "),collapse = "+"),")-(",m,")^2=",vvv(x = x,p = p),"\\]")
  }} else {
    freq <- table(x)
    xx <- dimnames(freq)$x
    cat("\\[",mnam,"=\\left(",paste(paste(xx,"^2\\frac {",freq,"}{",n,"}"),collapse = "+"),"\\right)-(",m,")^2=",s2c(x),"\\]")
  }
}

stat_ <- function(x,p=NULL,mnam="\\mu",vnam="\\sigma^2",semp=F){
  n <- length(x)
  m <- ifelse(test = is.null(p),mean(x),sum(x*p))
  p1 <- character(n)
  p2 <- character(n)
  p1[x<0]<-"("
  p2[x<0]<-")"
  xp <- paste(p1,x,p2)
  if (!semp){
    if(is.null(p)){
      cat("\\begin{eqnarray*}\n",
          mnam,"&=& \\frac 1{",n,"}(",paste(xp,collapse = "+"),")=",mean(x),"\\\\ \n",
          vnam,"&=& \\frac 1{",n,"}(",paste(paste(xp,2,sep = "^"),collapse = "+"),")-(",m,")^2=",s2c(x),
          "\n\\end{eqnarray*}\n")
    }
    if(!is.null(p)){
      p <- p/sum(p)
      pp <- round(p,4)
      cat("\\begin{eqnarray*}\n",
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
      cat("\\begin{eqnarray*}",
          mnam,"&=& E(X_i) = \\sum_{x\\in S_X}x P(X=x)\\\\ \n",
               "&=&",paste(paste(xx,"\\frac {",freq,"}{",n,"}"),collapse = "+"),"\\\\ 
                &=&",mean(x),"\\\\ \n",
          vnam,"&=& V(X_i) = \\sum_{x\\in S_X}x^2 P(X=x)-\\mu^2\\\\ \n",
                "&=&\\left(",paste(paste(xx,"^2\\frac {",freq,"}{",n,"}"),collapse = "+"),"\\right)-(",m,")^2\\\\ 
                &=&",s2c(x),
          "\n\\end{eqnarray*}\n")
    }
  }

percentile <- function(p=0.5){
  cat(knit_child(src_("calcolo-percentile.Rmd"),envir = environment(),quiet=T))
}

## Probabilità ####

two_way <- function(S_1,S_2,num1,num2,op=`+`,EV=T,vnam="$X$",size="\\normalsize "){
  html <- ifelse(exists("html"),html,T)
  den1 <- rep(sum(num1),times=length(S_1))
  den2 <- rep(sum(num2),times=length(S_2))
  k1 <- length(S_1)
  k2 <- length(S_2)
  P_1 <- num1/den1
  P_2 <- num2/den2
  
  SS <- outer(S_2,S_1,op)
  p1 <- P_1
  NN <- outer(num2,num1)
  DD <- outer(den2,den1)

  frc  <- ifelse(html,"\\frac","\\sfrac")  
  hed  <- ifelse(html,"\\[\n","\\[\\arraycolsep=10pt\\def\\arraystretch{1}")  
  frac <- function(i,j){
    paste(op(S_2[i],S_1[j]),";&","\\color{red}{",frc,"{",num2[i]*num1[j],"}{",den2[i]*den1[j],"}}",sep="")
  }
  
  mat1 <- outer(1:length(S_2),1:length(S_1),frac)
  mat1 <- cbind(paste(S_2,";\\color{blue}{",num2,"/",den2,"}"),mat1)
  cols <- paste(paste(paste("&",paste(paste(S_1,";"),paste("{",num1,"}"),sep=paste("&\\color{blue}{", frc))),paste("{",den1,"}}"),collapse = " "),"\\\\ \n")
  
  allign <- paste("r|",paste(rep("r",times=2*k1),collapse = ""),sep="",collapse = "")

  cat(size,"\n\n")
  cat(hed,"\\begin{array}",paste("{",allign,"}\n"),
        cols,"\\hline \n",
        apply(mat1,1,function(x)c(paste(x,collapse = "& "),"\\\\ \n")),
      "\\end{array}\n \\]\n\n",sep ="")
  
  cat("\\normalsize E ricaviamo la distribuzione di,",vnam,"\n\n")
  
  S_3 <- sort(unique(as.numeric(outer(S_1,S_2,op))))
  num3 <- 1:length(S_3)
  den3 <- 1:length(S_3)
  for (i in 1:length(S_3)){
    num3[i] <- sum(NN[SS==S_3[i]])
    den3[i] <- DD[SS==S_3[i]][1]
  }
 
  k3 <- length(S_3)
  allign <- paste("r|",paste(rep("r",times=k3),collapse = ""),sep="",collapse = "")
  rig1 <- paste("X &",paste(S_3,collapse = "& "),"\\\\ \n")
  rig2 <- paste("P(X) &",paste(frc,"{",num3,"}","{",den3,"}",collapse = "& ",sep=""))
  cat(size,"\n\n")
    cat("\\[
      \\begin{array}{",allign,"}\n",
      rig1,"\\hline \n",
      rig2,
      "\\\\ \n \\end{array}\n \\]\n")
    urn <- rep(S_3,times=num3)
if (EV){    
    cat("\\normalsize Calcoliamo valore atteso e varianza\n\n")
    cat(size,"\n\n")
    cat(stat_(urn,semp = T),"\\normalsize\n\n")
    }
  return(list(S_3=S_3,num3=num3,den3=den3,urn=urn))
}

bin_dis <- function(x1,n,pp,verso="\\leq",comp=FALSE,sing=FALSE,x0=0,vnam="X",size="\\normalsize"){
  cat(knit_child(src_("binom-dis.Rmd"),envir = environment(),quiet=T))
}

pois_dis <- function(x1,ll,verso="\\leq",sing=FALSE,vnam="X"){
  cat(knit_child(src_("pois-dis.Rmd"),envir = environment(),quiet=T))
}

norm_int <-  function(x1,x2=NULL,verso="<",mm,ss,vnam="X",mu="\\mu",sigma="\\sigma"){
  if (is.null(verso)) cat(knit_child(src_("norm-int.Rmd"),envir = environment(),quiet=T)) else
         cat(knit_child(src_("norm-dis.Rmd"),envir = environment(),quiet=T))
}

norm_semp <- function(x1,mm,ss){
  z1 <- round((x1-mm)/sqrt(ss),2)
  pnorm(z1)
}
tlc <- function(tipo, x1, x2=NULL,verso, mu = F, s2 = NULL, n){
  if (tipo == "somma") {  
    if(!is.null(s2)){
    cat(
      "\\begin{eqnarray*}
  S_n & \\mathop{\\sim}\\limits_{a}& N(n\\mu,n\\sigma^2) \\\\
       &\\sim & N(",n*mu,",",n*s2,") 
  \\end{eqnarray*}")
    cat(norm_int(x1 = x1,x2 = x2,verso = verso,mm = n*mu,ss = s2*n,vnam = "S_n",mu = "n\\mu",sigma = "\\sqrt{n\\sigma^2}"))
    } else {
      cat(
        "\\begin{eqnarray*}
  S_n & \\mathop{\\sim}\\limits_{a}& N(n\\pi,n\\pi(1-\\pi)) \\\\
       &\\sim & N(",n*mu,",",n*mu*(1-mu),")
  \\end{eqnarray*}")
      s2 <- mu*(1-mu)
      cat(norm_int(x1 = x1,x2 = x2,verso = verso,mm = n*mu,ss = s2*n,vnam = "S_n",mu = "n\\pi",sigma = "\\sqrt{n\\pi(1-\\pi)}"))
      
    }
  }
  if (tipo == "media") {  
    cat(
      "\\begin{eqnarray*}
  \\bar X & \\mathop{\\sim}\\limits_{a}& N(\\mu,\\sigma^2/n) \\\\
       &\\sim & N(",mu,",",s2/n,")
  \\end{eqnarray*}")
    cat(norm_int(x1 = x1,x2 = x2,verso = verso,mm = mu,ss = s2/n,vnam = "\\bar X",mu = "\\mu",sigma = "\\sqrt{\\sigma^2/n}"))
  }
  if (tipo == "prop") {  
    cat(
      "\\begin{eqnarray*}
  \\hat\\pi & \\mathop{\\sim}\\limits_{a}& N(\\pi,\\pi(1-\\pi)/n) \\\\
       &\\sim & N(",mu,",",mu*(1-mu)/n,") 
  \\end{eqnarray*}")
    cat(norm_int(x1 = x1,x2 = x2,verso = verso,mm = mu,ss = mu*(1-mu)/n,vnam = "\\hat\\pi",mu = "\\pi",sigma = "\\sqrt{\\pi(1-\\pi)/n}"))
  }
  
}
  

draw_dist <- function(dist,z1,z2,...){    # aggiunge una distribuzione tratteggiata
  xx <- c(z1,seq(z1,z2,length=100),z2)
  yy <- c(0 ,dist(seq(z1,z2,length=100)),0)
  polygon(xx,yy,...,border = NA)
  curve(dist,z1,z2,add=T)
}

## Inferenza ####

idc <- function(xm,sd=NULL,alpha,n,dist_,mus=NULL,ss=NULL){
  if (!is.null(mus)){
    sd <- ifelse(dist_=="t",sd*sqrt(n/(n-1)),sd)
    SEs <- paste("\\frac{",ss,"}{\\sqrt{n}}")
    SEn <- paste("\\frac{",p(sd),"}{\\sqrt{",n,"}}")
  }
  if (is.null(sd)){
    cat("\\[
        \\hat\\pi = \\frac{S_n}n = \\frac{",xm,"}{",n,"}=",xm/n,"
        \\]\n\n")
    mus <- "\\hat\\pi"
    xm <- xm/n
    sd <- sqrt(xm*(1-xm))
    SEs  <- "\\sqrt{\\frac{\\hat\\pi(1-\\hat\\pi)}{n}}"
    SEn  <- paste("\\sqrt{\\frac{",p(xm),"(1-",p(xm),")}{",n,"}}")
  }
  if (!is.null(sd)&is.null(mus)&dist_=="z"){
    mus <- "\\hat\\mu"
    SEs  <- "\\frac{\\sigma}{\\sqrt{n}}"
    SEn <- paste("\\frac{",sd,"}{\\sqrt{",n,"}}")
  }
  if (!is.null(sd)&is.null(mus)&dist_=="t"){
    mus <- "\\hat\\mu"
    SEs  <- "\\frac{S}{\\sqrt{n}}"
    SEn <- paste("\\frac{",p(sd*sqrt(n/(n-1))),"}{\\sqrt{",n,"}}")
  }
  
  tstat <- ifelse(dist_=="z",qnorm(1-alpha/2),qt(1-alpha/2,n-1))
  tsimb <- ifelse(dist_=="z","z_{\\alpha/2}","t_{n-1;\\alpha/2}")
  if (dist_=="z"){
  sc <- sd
  idcn <- xm+c(-1,1)*tstat*sd/sqrt(n)
  }
  if (dist_=="t"){
    sc <- sqrt(n/(n-1))*sd
    cat(
      "\\[
      S =\\sqrt{\\frac {n}{n-1}}\\cdot\\hat\\sigma =
      \\sqrt{\\frac {",n,"}{",n-1,"}}\\cdot", sd,"=",p(sc),
        "\\]\n")
      idcn <- xm+c(-1,1)*tstat*sc/sqrt(n)
      sd <- sc
    }
  cat(
  "\\begin{eqnarray*}
  Idc: & & ",mus,"\\pm ",tsimb,"\\times",SEs,"\\\\
       & & ",xm, "\\pm ",tstat, "\\times",SEn,"\\\\
       & & ",xm, "\\pm ",tstat,"\\times ",sd/sqrt(n),"\\\\
       & & [",idcn[1],", ",idcn[2],"]
  \\end{eqnarray*}\n")
}

ztest_pi <- function(sn,n,p0,h1,alpha){
  cat(knit_child(src_("ztestpi.Rmd"),envir = environment(),quiet=T))
}

ztest_mu <- function(muh,s,n,mu0,h1,alpha,um="",pvalue=T){
  cat(knit_child(src_("ztest-mu.Rmd"),envir = environment(),quiet=T))
}

ttest_mu <-  function(muh,sh,n,mu0,h1,alpha,um=""){
  cat(knit_child(src_("ttest-mu.Rmd"),envir = environment(),quiet=T))
}

test_2c <-  function(mu1,mu2,s1h=F,s2h=F,n1,n2,h1,alpha,et=F,a="1",b="2",um=""){
  if (!s1h) {
    s1 <- mu1
    s2 <- mu2
    cat(knit_child(src_("ttest-2c-pi.Rmd"),envir = environment(),quiet=T))
    } else {
    if (et) cat(knit_child(src_("ttest-2c-et.Rmd"),envir = environment(),quiet=T)) else cat(knit_child(src_("ttest-2c-om.Rmd"),envir = environment(),quiet=T))
      }
}

ttest_2c_et <-  function(mu1,mu2,s1h,s2h,n1,n2,h1,alpha,a="1",b="2",um="",et=T){
  cat(knit_child(src_("ttest-2c-et.Rmd"),envir = environment(),quiet=T))
}

ttest_2c_om <-  function(mu1,mu2,s1h,s2h,n1,n2,h1,alpha,a="1",b="2",um="",et=F){
  cat(knit_child(src_("ttest-2c-om.Rmd"),envir = environment(),quiet=T))
}

ztest_2c_pi <-  function(s1,s2,n1,n2,h1,alpha,a="1",b="2",um="",et=F){
  cat(knit_child(src_("ttest-2c-pi.Rmd"),envir = environment(),quiet=T))
}

chi_print <- function(dat,nome_x,nome_y,print=T){
  dat_print <- cbind(dat,rowSums(dat))
  dat_print <- rbind(dat_print,colSums(dat_print))
  dimnames(dat)[[1]] <- c(nome_y)
  dimnames(dat)[[2]] <- c(nome_x)
  dimnames(dat_print)[[1]] <- c(nome_y,"Tot")
  dimnames(dat_print)[[2]] <- c(nome_x,"Tot")
  if (print) {
    kable(dat,digits = 4,row.names = T) %>%
      kable_styling(full_width = F) %>%
      column_spec(column = 1,bold = T)
    }
  return(list(dat,dat_print))
}

chi_print_conf <- function(Freq_c,Freq_0,X,Y){
  S <- c(paste("$",Freq_c,"$",sep = ""), paste("$",sum(Freq_c),"$",sep = ""))
  N <- c(paste("$",Freq_0,"\\%$",sep=""),"$100\\%$")
  D <- data.frame(rbind(S, N), row.names = Y)
  names(D) <- c(X,"Totale")
  tabl(D)
}

chi_test <- function(dat,alpha){
  cat(knit_child(src_("chi-test-indip.Rmd"),envir = environment(),quiet=T))
}

chi_conf <- function(Freq_c,Freq_0,X,Y,alpha=0.05){
  cat(knit_child(src_("chi-test-conf.Rmd"),envir = environment(),quiet=T))
}

## Regressione ####


regr <- function(x=NULL,y=NULL,stat1=NULL,stat2=NULL,semp=F,ax=2){
  if (!semp){
    if (!is.null(x)){
      n <- length(x)
      mx <- mean(x)
      vx <- mean(x^2)-mean(x)^2
      my <- mean(y)
      vy <- mean(y^2)-mean(y)^2
      co <- mean(x*y) - mx*my
      sumx <- sum(x)
      sumx2<- sum(x^2)
      sumy <- sum(y)
      sumy2<- sum(y^2)
      sumxy <- sum(x*y)
    } else if (!is.null(stat1)) {
      n <- stat1$n
      mx <- stat1$sumx/n
      vx <- stat1$sumx2/n - mx^2
      my <- stat1$sumy/n
      vy <- stat1$sumy2/n - my^2
      co <- stat1$sumxy/n - mx*my
      sumx <- stat1$sumx
      sumx2<- stat1$sumx2
      sumy <- stat1$sumy
      sumy2<- stat1$sumy2
      sumxy <- stat1$sumxy
    } else if (!is.null(stat2)){
      n <- stat2$n
      mx <- stat2$mx
      vx <- stat2$vx
      my <- stat2$my
      vy <- stat2$vy
      co <- stat2$co
      sumx <- n*mx
      sumx2<- n*(vx+mx^2)
      sumy <- n*my
      sumy2<- n*(vy+my^2)
      sumxy <- n*(co+mx*my)
    }} else {
      n <- length(x)
      sumx  <- round(sum(x),ax)
      sumx2 <- round(sum(x^2),ax)
      sumy  <- round(sum(y),ax)
      sumy2 <- round(sum(y^2),ax)
      sumxy <- round(sum(x*y),ax)
      mx <- sumx/n
      vx <- sumx2/n-mx^2
      my <- sumy/n
      vy <- sumy2/n-my^2
      co <- mean(x*y) - mx*my
    }
  sx <- sqrt(vx)
  sy <- sqrt(vy)
  r  <- co/sqrt(vy*vx)
  b1 <- co/vx
  b0 <- my - b1*mx
  ys <- b0 + b1 * x
  es <- y - ys
  rg <- ys - my
  sh2 <- vy*(1-r^2)
  se2 <- (n/(n-2))*sh2 
  vb0 <- se2 * (1/n+mx^2/(n*vx))
  vb1 <- se2 / (n*vx)
  seh <- sqrt((1-r^2)*vy)
  se <- sqrt(n/(n-2))*seh
  h <- 1/n + (x-mean(x))^2/(n*s2c(x))
  ss <- sd(y)*sqrt(1-r^2)*sqrt(1-h)
  
  # For old compatibility prin
  if (!is.null(x)){
  Dato <- c(1:n,"Totale","Totale/n")
  prn <- round(data.frame(x=c(x,0,0),y=c(y,0,0),x2 = c(x^2,0,0),y2 = c(y^2,0,0),xy=c(x*y,0,0)),ax)
  prn <- data.frame(Dato,prn)
  prn[n+1,2:6] <- colSums(prn[1:n,2:6])
  prn[n+2,2:6] <- colMeans(prn[1:n,2:6])
  names(prn)<-c("$i$","$x_i$","$y_i$","$x_i^2$","$y_i^2$","$x_i\\cdot y_i$") 
  
  ml <- lsfit(x,y)
  bb <- ml$coefficients
  } else {
    prn <- ml <- bb <- NULL
  }
  return(
    list(
      mx = mx,
      vx = vx,
      sx = sx,
      my = my,
      vy = vy,
      sy = sy,
      co = co,
      cv = co,
      r  = r,
      b1 = b1,
      b0 = b0,
      ys = ys,
      es = es,
      rxg = rg,
      sh2 = sh2,
      se2 = se2,
      vb0 = vb0,
      vb1 = vb1,
      seh = seh,
      se  = se,
      n   = n,
      sumx = sumx,
      sumy = sumy,
      sumx2 =sumx2,
      sumy2 = sumy2,
      sumxy = sumxy,
      prn = prn,
      ml = ml,
      bb = bb,
      h = h,
      ss=ss
    )
  )
}  

calcolo_beta <- function(semplice=F,inv = F){
  if (!inv){
  if (semplice) {cat(knit_child(src_("calcolo-beta-semp.Rmd"),envir = environment(),quiet=T))
  } else {cat(knit_child(src_("calcolo-beta.Rmd"),envir = environment(),quiet=T))}
  } else {cat(knit_child(src_("calcolo-beta2.Rmd"),envir = environment(),quiet=T))}
}

previsione <- function(x){
  if (b1 < 0) {
  }
  cat("\\[\\hat y_{X=",x,"}=\\hat\\beta_0+\\hat\\beta_1 x=",b0,"+",p(b1),"\\times",p(x),"=",b0+b1*x,"\\]")
}

residuo <- function(x,y){
  cat("\\begin{eqnarray*}\n")
  cat("\\hat y_i &=&\\hat\\beta_0+\\hat\\beta_1 x_i=\\\\ \n")
  cat(          "&=&",b0,"+",p(b1),"\\times",p(x),"=",b0+b1*x,"\\\\ \n")
  cat("\\hat \\varepsilon_i &=& y_i-\\hat y_i\\\\ \n")
  cat(                     "&=&",y,"-",b0+b1*x,"=",y - (b0+b1*x)," \n")
  cat("\\end{eqnarray*}\n")  
}

R2 <- function(){
  sgn <- ifelse(r^2>.75,">","<")
  cat("\\begin{eqnarray*}\n")
  cat("r&=&\\frac{\\text{cov}(X,Y)}{\\sigma_X\\sigma_Y}=\\frac{",co,"}{",sx,"\\times",sy,"}=",r,"\\\\")
  cat("r^2&=&",r^2,sgn,"0.75\n")
  cat("\\end{eqnarray*}\n")  
  ifelse(r^2>.75,"Il modello si adatta bene ai dati.","Il modello **non** si adatta bene ai dati.")
}

TSS <- function(){
  cat("\\begin{eqnarray*}
    TSS &=& n\\hat\\sigma^2_Y\\\\
        &=&",n,"\\times",vy,"\\\\
        &=& ",n*vy,"\\\\
    ESS &=& R^2\\cdot TSS\\\\
        &=& ",r^2,"\\cdot",n*vy,"\\\\
        &=&", r^2*n*vy,"\\\\
    RSS &=& (1-R^2)\\cdot TSS\\\\
        &=& (1-",r^2,")\\cdot",n*vy,"\\\\
        &=& ",(1-r^2)*n*vy, "\\\\
    TSS &=& RSS+TSS \\\\", 
    n*vy," &=& ", r^2*n*vy, "+", (1-r^2)*n*vy, "
  \\end{eqnarray*}")
}
  

se_beta1 <- function(x=NULL,y=NULL,stat1=NULL,stat2=NULL)
{
  cat(knit_child(src_("SE-beta1.Rmd"),envir = environment(),quiet=T))
}

se_beta0 <- function(x=NULL,y=NULL,stat1=NULL,stat2=NULL)
{
  cat(knit_child(src_("SE-beta0.Rmd"),envir = environment(),quiet=T))
}

ttest_beta <-  function(cof,bj0,h1,alpha){
  cat(knit_child(src_("ttest-beta.Rmd"),envir = environment(),quiet=T))
}

## Varie ####

ellisse <- function (x, y, a = 1, b = 1, agl = 0, segment = NULL, arc.only = TRUE, 
                     deg = TRUE, nv = 100, border = NULL, col = NA, lty = 1, lwd = 1, 
                     ...) {
  if (is.null(segment)) {
    if (deg) 
      segment <- c(0, 360)
    else segment <- c(0, 2 * pi)
  }
  draw1ellipse <- function(x, y, a = 1, b = 1, agl = 0, segment = NULL, 
                           arc.only = TRUE, nv = 100, deg = TRUE, border = NULL, 
                           col = NA, lty = 1, lwd = 1, ...) {
    if (deg) {
      agl <- agl * pi/180
      segment <- segment * pi/180
    }
    z <- seq(segment[1], segment[2], length = nv + 1)
    xx <- a * cos(z)
    yy <- b * sin(z)
    alpha <- xyagl(xx, yy, directed = TRUE, deg = FALSE)
    rad <- sqrt(xx^2 + yy^2)
    xp <- rad * cos(alpha + agl) + x
    yp <- rad * sin(alpha + agl) + y
    if (!arc.only) {
      xp <- c(x, xp, x)
      yp <- c(y, yp, y)
    }
    polygon(xp, yp, border = border, col = col, lty = lty, 
            lwd = lwd, ...)
    invisible(NULL)
  }
  xyagl <- function(x, y, directed = FALSE, deg = TRUE) {
    if (missing(y)) {
      y <- x[, 2]
      x <- x[, 1]
    }
    out <- atan2(y, x)
    if (!directed) 
      out <- out%%pi
    if (deg) 
      out <- out * 180/pi
    out
  }
  if (missing(y)) {
    y <- x[, 2]
    x <- x[, 1]
  }
  n <- length(x)
  if (length(a) < n) 
    a <- rep(a, n)[1:n]
  if (length(b) < n) 
    b <- rep(b, n)[1:n]
  if (length(agl) < n) 
    agl <- rep(agl, n)[1:n]
  if (length(col) < n) 
    col <- rep(col, n)[1:n]
  if (length(border) < n) 
    border <- rep(border, n)[1:n]
  if (length(nv) < n) 
    nv <- rep(nv, n)[1:n]
  if (n == 1) 
    draw1ellipse(x, y, a, b, agl = agl, segment = segment, 
                 arc.only = arc.only, deg = deg, nv = nv, col = col, 
                 border = border, lty = lty, lwd = lwd, ...)
  else {
    if (length(segment) < 2 * n) 
      segment <- matrix(rep(segment, n), n, 2, byrow = TRUE)
    lapply(1:n, function(i) draw1ellipse(x[i], y[i], a[i], 
                                         b[i], agl = agl[i], segment = segment[i, ], arc.only = arc.only, 
                                         deg = deg, nv = nv[i], col = col[i], border = border[i], 
                                         lty = lty, lwd = lwd, ...))
  }
  invisible(NULL)
}








two_way2 <- function(S_1,S_2,p1,p2,op=`+`,EV=T,vnam="$X$",size="\\normalsize "){
  html <- ifelse(exists("html"),html,T)
  k1 <- length(S_1)
  k2 <- length(S_2)

  SS <- outer(S_2,S_1,op)

  hed  <- ifelse(html,"\\[\n","\\[\\arraycolsep=10pt\\def\\arraystretch{1}")  
  frac <- function(i,j){
    paste(op(S_2[i],S_1[j]),";& ","\\color{red}{",p(p2[i]*p1[j]),"}",sep="")
  }
  
  mat1 <- outer(1:length(S_2),1:length(S_1),frac)
  cols <- paste(S_1,"~~\\color{blue}{",p(p1),"}")
  hed2 <- paste("&",paste(S_2,"&\\color{blue}{",p(p2),"}",collapse = " & "))
  
  
  allign <- paste("r|",paste(rep("r",times=2*k1),collapse = ""),sep="",collapse = "")
  
  cat(size,"\n\n")
  cat(hed,"\\begin{array}",paste("{",allign,"}\n"),
      hed2,"\\\\ \\hline \n",
      apply(cbind(cols,mat1),1,function(x)c(paste(x,collapse = "& "),"\\\\ \n")),
      "\\end{array}\n \\]\n\n",sep ="")
  
  cat("\\normalsize E ricaviamo la distribuzione di,",vnam,"\n\n")
  
  ord <- sort((as.numeric(outer(S_1,S_2,op))))
  pos <- order((as.numeric(outer(S_1,S_2,op))))
  p3  <- tapply(as.numeric(outer(p1,p2))[pos],INDEX = ord,FUN = sum)
  S_3 <- unique(ord)
  k3 <- length(S_3)
  allign <- paste("r|",paste(rep("r",times=k3),collapse = ""),sep="",collapse = "")
  rig1 <- paste("X &",paste(S_3,collapse = "& "),"\\\\ \n")
  rig2 <- paste("P(X) &",paste(p(p3),collapse = "& ",sep=""))
  cat(size,"\n\n")
  cat("\\[
      \\begin{array}{",allign,"}\n",
      rig1,"\\hline \n",
      rig2,
      "\\\\ \n \\end{array}\n \\]\n")
  if (EV){    
    cat("\\normalsize Calcoliamo valore atteso e varianza\n\n")
    cat(size,"\n\n")
    cat(stat_(S_3,p = p3),"\\normalsize\n\n")
  }
}
