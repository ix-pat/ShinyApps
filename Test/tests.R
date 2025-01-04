ttest_2c <- function(mu1,mu2,s1h,s2h,n1,n2,h1="\\neq",alpha=0.05){
  a <- "A"
  b <- "B"
  s1 <- sqrt(n1/(n1-1))*s1h
  s2 <- sqrt(n2/(n2-1))*s2h
  s2p <- (n1*s1h^2+n2*s2h^2)/(n1+n2-2)
  se <- sqrt(s2p/n1+s2p/n2)
  tobs <- (mu1-mu2)/se
  if (h1==">") tc <- qt(1-alpha,n1+n2-2)
  if (h1=="<") tc <- -qt(1-alpha,n1+n2-2)
  if (h1=="\\neq") tc <- sign((mu1-mu2))*qt(1-alpha/2,n1+n2-2)
  if (h1=="\\neq") alpha <- alpha/2
  if (!exists("um")) um <- ""
  if (!exists("a")) a <- 1
  if (!exists("b")) b <- 2
  q1 <- "p"; q2 <- "p"
  s2f1<- s2p; s2f2 <- s2p
  nomi_variabili <- ls(envir = environment())
  
  # Itera sui nomi delle variabili per arrotondare quelle numeriche
  for (nome in nomi_variabili) {
    valore <- get(nome, envir = environment())
    if (is.numeric(valore)) {
      # Arrotonda il valore e aggiornalo nell'ambiente della funzione
      assign(nome, round(valore, digits = 4), envir = environment())
    }
  }
  
  
  c0 <- paste("\\(\\fbox{A}\\) FORMULAZIONE DELLE IPOTESI
  
  $$\\begin{cases}
  H_0:\\mu_\\text{", a,"} =      \\mu_\\text{ ", b,"}\\text{", um,"}\\\\
  H_1:\\mu_\\text{", a,"} ", h1," \\mu_\\text{ ", b,"}\\text{", um,"}
  \\end{cases}$$
    ", ifelse(h1=="\\neq","Siccome \\(H_1\\) è bilaterale, considereremo \\(\\alpha/2\\), anziché \\(\\alpha\\) $$ $$",""),"
  
  \\(\\fbox{B}\\) SCELTA E CALCOLO STATISTICA-TEST, \\(T\\)

  L'ipotesi è di omogeneità e quindi calcoliamo:
    
    $$
    S_p^2=\\frac{n_\\text{", a,"}\\hat\\sigma^2_\\text{", a,"}+n_\\text{", b,"}\\hat\\sigma^2_\\text{", b,"}}{n_\\text{", a,"}+n_\\text{", b,"}-2} =
    \\frac{", n1,"\\cdot", s1h,"^2+", n2,"\\cdot", s2h,"^2}{", n1,"+", n2,"-2}=", s2p,"
  $$
    
    \\begin{eqnarray*}
  \\frac{\\hat\\mu_\\text{", a,"} - \\hat\\mu_\\text{", b,"}} 
  {\\sqrt{\\frac {S^2_p}{n_\\text{", a,"}}+\\frac {S^2_p}{n_\\text{", b,"}}}}&\\sim&t_{n_\\text{", a,"}+n_\\text{", b,"}-2}\\\\
  t_{\\text{obs}}
  &=& \\frac{ (", mu1,"- ", mu2,")} {\\sqrt{\\frac{", s2f1,"}{", n1,"}+\\frac{", s2f2,"}{", n2,"}}}
  =  ", tobs,"\\, .
  \\end{eqnarray*}
  ")
  
  H1 <- h1
  if (h1=='\\neq' & tc < 0) h1 <- ifelse(tc<0,"<",">")
  
  
  c1 <- paste0("\\(\\fbox{C}\\) DECISIONE
  Dalle tavole si ha \\(t_{(", n1,"+", n2,"-2);\\, ", alpha,"} = ", tc,"\\).
  $$t_{\\text{obs}} = ", tobs," ", ifelse(tobs<tc,"<",">")," t_{", n1+n2-2,";\\, ", alpha,"} = ", tc,"$$
    
    CONCLUSIONE: i dati ", ifelse(tobs<tc & h1=="<" | tobs>tc & h1==">", " non sono", "sono")," coerenti con \\(H_{0}\\) al LdS del ", ifelse(H1=="\\neq",2*alpha*100,alpha*100),"%")
  
  
  if (H1 == ">") {R <- c(tc,4); A <- c(-4,tc)}
  if (H1 == "<") {R <- c(-4,tc); A <- c(tc,4)}
  if (H1 == "\\neq") {A <- c(-abs(tc),abs(tc)); R1 <- c(-4,-abs(tc)); R <- c(4,abs(tc))}
  curve(dt(x,n1+n2-2),-4,4,axes=F,xlab="T",ylab="")
  
  lines(A,c(0,0),lwd=2,col=4)
  if (H1 == "\\neq") {
    lines(R1,c(0,0),lwd=2,col=2) 
    axis(1,c(-4,-tc,0,tc,4),round(c(-4,-tc,0,tc,4),3))
  } else axis(1,round(c(-4,4,0,tc),3))
  lines(R,c(0,0),lwd=2,col=2)
  points(tobs,0,pch=4,cex=2)
  text(tobs,.05,expression(t[obs]))
  axis(2)
  
  if (H1 == ">") {pval <- paste("P(T_{n1+n2-2}>t_{\\text{obs}})=P(T_{n1+n2-2}>",round(tobs,3),")=",format(1-pt(tobs,n1+n2-2),digits = 4,scipen=8))}
  if (H1 == "<") {pval <- paste("P(T_{n1+n2-2}<t_{\\text{obs}})=P(T_{n1+n2-2}<",round(tobs,3),")=",format(pt(tobs,n1+n2-2),digits = 4,scipen=8))}
  if (H1 == "\\neq") {pval <- paste("P(|T_{n1+n2-2}|>|t_{\\text{obs}}|)=2P(T_{n1+n2-2}>|t_{\\text{obs}}|)=2P(T_{n1+n2-2}>|",round(tobs,4),"|)=",format(2*pt(-abs(tobs),n1+n2-2),digits = 4,scipen=8))}
  
  c2 <- paste("Il \\(p_{\text{value}}\\) è
  $$", pval,"$$")
  return(paste(c0,c1,c2,sep="\n\n"))
}

ttest_2e <- function(mu1,mu2,s1h,s2h,n1,n2,h1="\\neq",alpha=0.05){
  env <- new.env()
  um <- ""
  a <- "A"
  b <- "B"
  s1 <- sqrt(n1/(n1-1))*s1h
  s2 <- sqrt(n2/(n2-1))*s2h
  s2p <- (n1*s1h+n2*s2h)/(n1+n2-2)
  se <- sqrt(s1^2/n1+s2^2/n2)
  tobs <- (mu1-mu2)/se
  if (h1==">") tc <- qt(1-alpha,n1+n2-2)
  if (h1=="<") tc <- -qt(1-alpha,n1+n2-2)
  if (h1=="\\neq") tc <- sign((mu1-mu2))*qt(1-alpha/2,n1+n2-2)
  if (h1=="\\neq") alpha <- alpha/2
  q1 <- a; q2 <- b
  s2f1<- s1^2; s2f2 <- s2^2
  
  nomi_variabili <- ls(envir = environment())
  
  # Itera sui nomi delle variabili per arrotondare quelle numeriche
  for (nome in nomi_variabili) {
    valore <- get(nome, envir = environment())
    if (is.numeric(valore)) {
      # Arrotonda il valore e aggiornalo nell'ambiente della funzione
      assign(nome, round(valore, digits = 4), envir = environment())
    }
  }
  
  nomi_variabili <- ls(envir = environment())
  
  # Itera sui nomi delle variabili per arrotondare quelle numeriche
  for (nome in nomi_variabili) {
    valore <- get(nome, envir = environment())
    if (is.numeric(valore)) {
      # Arrotonda il valore e aggiornalo nell'ambiente della funzione
      assign(nome, round(valore, digits = 4), envir = environment())
    }
  }
  
  c0 <- paste("
    \\(\\fbox{A}\\) FORMULAZIONE DELLE IPOTESI
    
    $$\\begin{cases}
    H_0:\\mu_\\text{",a,"} =      \\mu_\\text{ ",b,"}\\text{",um,"}\\\\
    H_1:\\mu_\\text{",a,"} ",h1," \\mu_\\text{ ",b,"}\\text{",um,"}
    \\end{cases}$$
      ",ifelse(h1=="\\neq","Siccome \\(H_1\\) è bilaterale, considereremo \\(\\alpha/2\\), anziché \\(\\alpha\\) $$ $$"," "),"
    
    \\(\\fbox{B}\\) SCELTA E CALCOLO STATISTICA-TEST, \\(T\\)
      
      L'ipotesi è di eterogeneità e quindi calcoliamo:
    $$
      S^2_\\text{",a,"}=\\frac{n_\\text{",a,"}}{n_\\text{",a,"}-1}\\hat\\sigma^2_\\text{",a,"}=\\frac{",n1,"}{",n1,"-1}",s1h,"^2=",s2f1," \\qquad
      S^2_\\text{",b,"}=\\frac{n_\\text{",b,"}}{n_\\text{",b,"}-1}\\hat\\sigma^2_\\text{",b,"}=\\frac{",n2,"}{",n2,"-1}",s2h,"^2=",s2f2,"
    $$\n\n ")
  
  
  c1 <- paste("\\begin{eqnarray*}
    \\frac{\\hat\\mu_\\text{",a,"} - \\hat\\mu_\\text{",b,"}} 
    {\\sqrt{\\frac {S^2_\\text{",q1,"}}{n_\\text{",a,"}}+\\frac {S^2_\\text{",q2,"}}{n_\\text{",b,"}}}}&\\sim&t_{n_\\text{",a,"}+n_\\text{",b,"}-2}\\
    t_{\\text{obs}}
    &=& \\frac{ (",mu1,"- ",mu2,")} {\\sqrt{\\frac{",s2f1,"}{",n1,"}+\\frac{",s2f2,"}{",n2,"}}}
    =  ",tobs,"\\, .
    \\end{eqnarray*}
    ")
  
  H1 <- h1
  if (h1=='\\neq') h1 <- ifelse(tc<0,"<",">")
  
  
  c2 <- paste("\\(\\fbox{C}\\) DECISIONE
    
    Dalle tavole si ha \\(t_{(",n1,"+",n2,"-2);\\, ",alpha,"} = ",tc,"\\).
    $$t_{\\text{obs}} = ",tobs," ",ifelse(tobs<tc,"<",">")," t_{",n1+n2-2,";\\, ",alpha,"} = ",tc,"$$
      
      CONCLUSIONE: i dati ",ifelse(tobs<tc & h1=="<" | tobs>tc & h1==">", " non sono", "sono")," coerenti con \\(H_{0}\\) al LdS del ",ifelse(H1=="\\neq",2*alpha*100,alpha*100),"%
    ")
  
  if (H1 == ">") {R <- c(tc,4); A <- c(-4,tc)}
  if (H1 == "<") {R <- c(-4,tc); A <- c(tc,4)}
  if (H1 == "\\neq") {A <- c(-abs(tc),abs(tc)); R1 <- c(-4,-abs(tc)); R <- c(4,abs(tc))}
  curve(dt(x,n1+n2-2),-4,4,axes=F,xlab="T",ylab="")
  
  lines(A,c(0,0),lwd=2,col=4)
  if (H1 == "\\neq") {
    lines(R1,c(0,0),lwd=2,col=2) 
    axis(1,c(-4,-tc,0,tc,4),round(c(-4,-tc,0,tc,4),3))
  } else axis(1,round(c(-4,4,0,tc),3))
  lines(R,c(0,0),lwd=2,col=2)
  points(tobs,0,pch=4,cex=2)
  text(tobs,.05,expression(t[obs]))
  axis(2)
  
  if (H1 == ">") {pval <- paste("P(T_{n1+n2-2}>t_{\\text{obs}})=P(T_{n1+n2-2}>",round(tobs,3),")=",format(1-pt(tobs,n1+n2-2),digits = 4, scipen=8))}
  if (H1 == "<") {pval <- paste("P(T_{n1+n2-2}<t_{\\text{obs}})=P(T_{n1+n2-2}<",round(tobs,3),")=",format(pt(tobs,n1+n2-2),digits = 4, scipen=8))}
  if (H1 == "\\neq") {pval <- paste("P(|T_{n1+n2-2}|>|t_{\\text{obs}}|)=2P(T_{n1+n2-2}>|t_{\\text{obs}}|)=2P(T_{n1+n2-2}>|",round(tobs,4),"|)=",format(2*pt(-abs(tobs),n1+n2-2),digits = 4, scipen=8))}
  
  c3 <- paste("Il \\(p_{\\text{value}}\\) è
    $$",pval,"$$")
  
  return(paste(c0,c1,c2,c3,sep="\n\n"))
}

ttest <- function(muh,sh,n,mu0,h1,alpha=0.05){
  s <- sqrt(n/(n-1))*sh
  se <- s/sqrt(n)
  tobs <- (muh-mu0)/se
  if (h1==">") tc <- qt(1-alpha,n-1)
  if (h1=="<") tc <- -qt(1-alpha,n-1)
  if (h1=="\\neq") tc <- sign((muh-mu0))*qt(1-alpha/2,n-1)
  if (h1=="\\neq") alpha <- alpha/2
  if (!exists("um")) um <- ""
  
  nomi_variabili <- ls(envir = environment())
  
  # Itera sui nomi delle variabili per arrotondare quelle numeriche
  for (nome in nomi_variabili) {
    valore <- get(nome, envir = environment())
    if (is.numeric(valore)) {
      # Arrotonda il valore e aggiornalo nell'ambiente della funzione
      assign(nome, round(valore, digits = 4), envir = environment())
    }
  }
  
  c0 <- paste("\\(\\fbox{A}\\) FORMULAZIONE DELLE IPOTESI
    $$\\begin{cases}
    H_0:\\mu=\\mu_0=", mu0,"\\text{", um,"}\\\\
    H_1:\\mu", h1," \\mu_0=", mu0,"\\text{", um,"}
    \\end{cases}$$
      ", ifelse(h1=="\\neq","Siccome \\(H_1\\) è bilaterale, considereremo \\(\\alpha/2\\), anziché \\(\\alpha\\) $$ $$","$$ $$"),"
    
    \\(\\fbox{B}\\) SCELTA E CALCOLO STATISTICA-TEST, \\(T\\)
      \\(\\sigma^{2}\\) di \\(\\cal{P}\\) non è nota: \\(\\Rightarrow\\) t-Test.
    \\begin{eqnarray*}
    S
    &=& \\sqrt{\\frac{n} {n-1}}\\ \\widehat{\\sigma}
    =  \\sqrt{\\frac{", n,"} {", n,"-1}} \\times ", sh," = ", s," 
    \\end{eqnarray*}
    
    \\begin{eqnarray*}
    \\frac{\\hat\\mu - \\mu_{0}} {S/\\,\\sqrt{n}}&\\sim&t_{n-1}\\\\
    t_{\\text{obs}}
    &=& \\frac{ (", muh,"- ", mu0,")} {", s,"/\\sqrt{", n,"}}
    =  ", tobs,"\\, .
    \\end{eqnarray*}
    ")
  
  H1 <- h1
  if (h1=='\\neq') h1 <- ifelse(tc<0,"<",">")
  
  c1 <- paste("\\(\\fbox{C}\\) DECISIONE
    Dalle tavole si ha \\(t_{(", n,"-1);\\, ", alpha,"} = ", tc,"\\).
    $$t_{\\text{obs}} = ", tobs," ", ifelse(tobs<tc,"<",">")," t_{", n-1,";\\, ", alpha,"} = ", tc,"$$
      CONCLUSIONE: i dati ", ifelse(tobs<tc & h1=="<" | tobs>tc & h1==">", " non sono", "sono")," coerenti con \\(H_{0}\\) al LdS del ", ifelse(H1=="\\neq",2*alpha*100,alpha*100),"%")
  
  if (H1 == ">") {R <- c(tc,4); A <- c(-4,tc)}
  if (H1 == "<") {R <- c(-4,tc); A <- c(tc,4)}
  if (H1 == "\\neq") {A <- c(-abs(tc),abs(tc)); R1 <- c(-4,-abs(tc)); R <- c(4,abs(tc))}
  curve(dt(x,n-1),-4,4,axes=F,xlab="T",ylab="")
  
  lines(A,c(0,0),lwd=2,col=4)
  if (H1 == "\\neq") {
    lines(R1,c(0,0),lwd=2,col=2) 
    axis(1,c(-4,-tc,0,tc,4),round(c(-4,-tc,0,tc,4),3))
  } else axis(1,round(c(-4,4,0,tc),3))
  lines(R,c(0,0),lwd=2,col=2)
  points(tobs,0,pch=4,cex=2)
  text(tobs,.05,expression(t[obs]))
  axis(2)
  
  if (H1 == ">") {pval <- paste("P(T_{n-1}>t_{\\text{obs}})=P(T_{n-1}>",round(tobs,3),")=",format(1-pt(tobs,n-1),digits = 4, scipen=8))}
  if (H1 == "<") {pval <- paste("P(T_{n-1}<t_{\\text{obs}})=P(T_{n-1}<",round(tobs,3),")=",format(pt(tobs,n-1),digits = 4, scipen=8))}
  if (H1 == "\\neq") {pval <- paste("P(|T_{n-1}|>|t_{\\text{obs}}|)=2P(T_{n-1}>|t_{\\text{obs}}|)=2P(T_{n-1}>|",round(tobs,4),"|)=",format(2*pt(-abs(tobs),n-1),digits = 4, scipen=8))}
  
  c2 <- paste("Il \\(p_{\\text{value}}\\) è
    $$", pval,"$$")
  return(paste(c0,c1,c2,sep = "\n\n"))
}

ztest_mu <- function(muh,mu0,s,n,h1,alpha=0.05){
  se <- s/sqrt(n)
  tobs <- round((muh-mu0)/se,4)
  if (h1==">") tc <- qnorm(1-alpha)
  if (h1=="<") tc <- -qnorm(1-alpha)
  if (h1=="\\neq") tc <- sign((muh-mu0)^0)*qnorm(1-alpha/2)
  if (h1=="\\neq") alpha <- alpha/2
  if (!exists("um")) um <- ""
  
  nomi_variabili <- ls(envir = environment())
  
  # Itera sui nomi delle variabili per arrotondare quelle numeriche
  for (nome in nomi_variabili) {
    valore <- get(nome, envir = environment())
    if (is.numeric(valore)) {
      # Arrotonda il valore e aggiornalo nell'ambiente della funzione
      assign(nome, round(valore, digits = 4), envir = environment())
    }
  }
  
  c0 <- paste("
    \\(\\fbox{A}\\) FORMULAZIONE DELLE IPOTESI
    $$\\begin{cases}
    H_0:\\mu=\\mu_0=",mu0,"\\text{",um,"}\\\\
    H_1:\\mu",h1," \\mu_0=",mu0,"\\text{",um,"}
    \\end{cases}$$",
    ifelse(h1=="\\neq","Siccome \\(H_1\\) è bilaterale, considereremo \\(\\alpha/2\\), anziché \\(\\alpha\\) $$ $$",""),
    "\n\n \\(\\fbox{B}\\) SCELTA E CALCOLO STATISTICA-TEST, \\(Z\\)
      \\(\\sigma^{2}\\) di \\(\\cal{P}\\) è nota: \\(\\Rightarrow\\) z-Test.
    \\begin{eqnarray*}
    \\frac{\\hat\\mu - \\mu_{0}} {\\sigma/\\sqrt{n}}&\\sim&N(0,1)\\\\
    z_{\\text{obs}}
    &=& \\frac{ (",muh,"- ",mu0,")} {",s,"/\\sqrt{",n,"}}
    =  ",tobs,"\\, .
    \\end{eqnarray*}"
  )
  tc <- round(tc,4)
  H1 <- h1
  if (h1=='\\neq') h1 <- ifelse(tc<0,"<",">")
  
  c1 <- paste("\\(\\fbox{C}\\) DECISIONE
    Dalle tavole si ha \\(z_{",alpha,"} = ",tc,"\\).
    $$z_{\\text{obs}} = ",tobs,ifelse(tobs<tc,"<",">"), "z_{",alpha,"} = ",tc,"$$ ")
  
  c2 <- paste("CONCLUSIONE: i dati",ifelse(tobs<tc & h1=="<" | tobs>tc & h1==">", " non sono", "sono")," coerenti con \\(H_{0}\\) al LdS del ", ifelse(H1=="\\neq",2*alpha*100,alpha*100),"%")
  
  c3 <- "**Graficamente**"
  
  
  if (H1 == ">") {R <- c(tc,4); A <- c(-4,tc)}
  if (H1 == "<") {R <- c(-4,tc); A <- c(tc,4)}
  if (H1 == "\\neq") {A <- c(-abs(tc),abs(tc)); R1 <- c(-4,-abs(tc)); R <- c(4,abs(tc))}
  curve(dnorm,-4,4,axes=F,xlab="Z",ylab="")
  
  lines(A,c(0,0),lwd=2,col=4)
  if (H1 == "\\neq") {
    lines(R1,c(0,0),lwd=2,col=2) 
    axis(1,c(-4,-tc,0,tc,4),round(c(-4,-tc,0,tc,4),3))
  } else axis(1,round(c(-4,4,0,tc),3))
  lines(R,c(0,0),lwd=2,col=2)
  points(tobs,0,pch=4,cex=2)
  text(tobs,.05,expression(z[obs]))
  
  if (H1 == ">") {pval <- paste("P(Z>z_{\\text{obs}})=P(Z>",round(tobs,2),")=",format(1-pnorm(tobs),digits = 4,scipen=8))}
  if (H1 == "<") {pval <- paste("P(Z<z_{\\text{obs}})=P(Z<",round(tobs,2),")=",format(pnorm(tobs),digits = 4,scipen=8))}
  if (H1 == "\\neq") {pval <- paste("P(|Z|>|z_{\\text{obs}}|)=2P(Z>|z_{\\text{obs}}|)=2P(Z>|",round(tobs,2),"|)=",format(2*pnorm(-abs(round(tobs,2))),digits = 4,scipen=8))}
  c4 <- paste("Il \\(p_{\\text{value}}\\) è $$",pval,"$$")
  return(paste(c0,c1,c2,c4,sep = "\n\n"))
}  

ztest_pi <- function(sn,n,p0,h1,alpha = 0.05){
  ph <- sn/n
  se <- sqrt(p0*(1-p0)/n)
  tobs <- (ph-p0)/se
  if (h1==">") tc <- qnorm(1-alpha)
  if (h1=="<") tc <- -qnorm(1-alpha)
  if (h1=="\\neq") alpha <- alpha/2
  if (h1=="\\neq") tc <- sign((ph-p0))*qnorm(1-alpha)
  
  
  nomi_variabili <- ls(envir = environment())
  
  # Itera sui nomi delle variabili per arrotondare quelle numeriche
  for (nome in nomi_variabili) {
    valore <- get(nome, envir = environment())
    if (is.numeric(valore)) {
      # Arrotonda il valore e aggiornalo nell'ambiente della funzione
      assign(nome, round(valore, digits = 4), envir = environment())
    }
  }
  
  
  c0 <- paste("La stima
    $$\\hat\\pi=\\frac {", sn,"} {", n,"}=", ph," $$")
  
  c01 <- paste("
    \\(\\fbox{A}\\) FORMULAZIONE DELLE IPOTESI
    $$\\begin{cases}
    H_0:\\pi=\\pi_0=", p0,"\\\\
    H_1:\\pi", h1," \\pi_0=", p0,"
    \\end{cases}$$", 
    ifelse(h1=="\\neq","Siccome \\(H_1\\) è bilaterale, considereremo \\(\\alpha/2\\), anziché \\(\\alpha\\) $$ $$",""),
    "\n\n \\(\\fbox{B}\\) SCELTA E CALCOLO STATISTICA-TEST, \\(Z\\)
          Test Binomiale per \\(n\\) grande: \\(\\Rightarrow\\) z-Test.
    \\begin{eqnarray*}
    \\frac{\\hat\\pi - \\pi_{0}} {\\sqrt {\\pi_0(1-\\pi_0)/\\,n}}&\\sim&N(0,1)\\\\
    z_{\\text{obs}}
    &=& \\frac{ (", ph,"- ", p0,")} {\\sqrt{", p0,"(1-", p0,")/", n,"}}
    =  ", tobs,"\\, .
    \\end{eqnarray*}")
  
  H1 <- h1
  if (h1=='\\neq') h1 <- ifelse(tc<0,"<",">")
  
  c1 <- paste0("\\(\\fbox{C}\\) DECISIONE
    Dalle tavole si ha \\(z_{", alpha,"} = ", tc,"\\).
    $$z_{\\text{obs}} = ", tobs," ", ifelse(tobs<tc,"<",">")," z_{", alpha,"} = ", tc,"$$
      CONCLUSIONE: i dati ", ifelse(tobs<tc & h1=="<" | tobs>tc & h1==">", " non sono", "sono")," coerenti con \\(H_{0}\\) al LdS del ", ifelse(H1=="\\neq",2*alpha*100,alpha*100),"%")
  
  
  if (H1 == ">") {R <- c(tc,4); A <- c(-4,tc)}
  if (H1 == "<") {R <- c(-4,tc); A <- c(tc,4)}
  if (H1 == "\\neq") {A <- c(-abs(tc),abs(tc)); R1 <- c(-4,-abs(tc)); R <- c(4,abs(tc))}
  curve(dnorm,-4,4,axes=F,xlab="Z",ylab="")
  
  lines(A,c(0,0),lwd=2,col=4)
  if (H1 == "\\neq") {
    lines(R1,c(0,0),lwd=2,col=2) 
    axis(1,c(-4,-tc,0,tc,4),round(c(-4,-tc,0,tc,4),3))
  } else axis(1,round(c(-4,4,0,tc),3))
  lines(R,c(0,0),lwd=2,col=2)
  points(tobs,0,pch=4,cex=2)
  text(tobs,.05,expression(z[obs]))
  axis(2)
  
  if (H1 == ">") {pval <- paste("P(Z>z_{\\text{obs}})=P(Z>",round(tobs,2),")=",format(1-pnorm(tobs),digits=4,scipen=8))}
  if (H1 == "<") {pval <- paste("P(Z<z_{\\text{obs}})=P(Z<",round(tobs,2),")=",format(pnorm(tobs),digits=4,scipen=8))}
  if (H1 == "\\neq") {pval <- paste("P(|Z|>|z_{\\text{obs}}|)=2P(Z>|z_{\\text{obs}}|)=2P(Z>|",round(tobs,2),"|)=",format(2*pnorm(-abs(round(tobs,2))),digits=4,scipen=8))}
  
  c2 <- paste0("Il \\(p_{\\text{value}}\\) è $$", pval,"$$")
  return(paste(c0,c01,c1,c2,sep="\n\n"))
}

ztest2pi <- function(s1,n1,s2,n2,h1,alpha=0.05){
  p1 <- s1/n1
  p2 <- s2/n2
  n <- n1 + n2
  pc <- (s1+s2)/n
  se <- sqrt((pc*(1-pc))/n1+(pc*(1-pc))/n2)
  tobs <- (p1-p2)/se
  
  if (h1==">") tc <- qnorm(1-alpha)
  if (h1=="<") tc <- -qnorm(1-alpha)
  if (h1=="\\neq") tc <- sign((p1-p2))*qnorm(1-alpha/2)
  if (h1=="\\neq") alpha <- alpha/2
  um <- ""
  a <- "A"
  b <- "B"
  
  nomi_variabili <- ls(envir = environment())
  
  # Itera sui nomi delle variabili per arrotondare quelle numeriche
  for (nome in nomi_variabili) {
    valore <- get(nome, envir = environment())
    if (is.numeric(valore)) {
      # Arrotonda il valore e aggiornalo nell'ambiente della funzione
      assign(nome, round(valore, digits = 4), envir = environment())
    }
  }
  
  
  c0 <- paste("\\(\\fbox{A}\\) FORMULAZIONE DELLE IPOTESI
    $$\\begin{cases}
    H_0:\\pi_\\text{",a,"} =      \\pi_\\text{ ",b,"}\\text{",um,"}\\\\
    H_1:\\pi_\\text{",a,"} ",h1," \\pi_\\text{ ",b,"}\\text{",um,"}
    \\end{cases}$$
      ",ifelse(h1=="\\neq","Siccome \\(H_1\\) è bilaterale, considereremo \\(\\alpha/2\\), anziché \\(\\alpha\\) $$ $$",""),"
    
    \\(\\fbox{B}\\) SCELTA E CALCOLO STATISTICA-TEST, \\(Z\\)
      
      $$\\hat\\pi_\\text{",a,"}=\\frac{s_\\text{",a,"}}{n_\\text{",a,"}}=\\frac{",s1,"}{",n1,"}=",p1,"\\qquad
    \\hat\\pi_\\text{",b,"}=\\frac{s_\\text{",b,"}}{n_\\text{",b,"}}=\\frac{",s2,"}{",n2,"}=",p2,"$$
      
      Calcoliamo la proporzione comune sotto \\(H_0\\)
      $$
      \\pi_C=\\frac{s_\\text{",a,"}+s_\\text{",b,"}}{n_\\text{",a,"}+n_\\text{",b,"}}=
      \\frac{",s1+s2,"}{",n1+n2,"}=",pc,"
    $$
      
      \\begin{eqnarray*}
    \\frac{\\hat\\pi_\\text{",a,"} - \\hat\\pi_\\text{",b,"}} 
    {\\sqrt{\\frac {\\pi_C(1-\\pi_C)}{n_\\text{",a,"}}+\\frac {\\pi_C(1-\\pi_C)}{n_\\text{",b,"}}}}&\\sim&N(0,1)\\\\
    z_{\\text{obs}}
    &=& \\frac{ (",p1,"- ",p2,")} {\\sqrt{\\frac{",pc,"(1-",pc,")}{",n1,"}+\\frac{",pc,"(1-",pc,")}{",n2,"}}}
    =  ",tobs,"\\, .
    \\end{eqnarray*}
    ")
  
  H1 <- h1
  if (h1=='\\neq') h1 <- ifelse(tc<0,"<",">")
  
  c1 <- paste("\\(\\fbox{C}\\) DECISIONE Dalle tavole si ha \\(z_{",alpha,"} = ",tc,"\\).
    $$z_{\\text{obs}} = ",tobs," ",ifelse(tobs<tc,"<",">")," z_{",alpha,"} = ",tc,"$$
      
      CONCLUSIONE: i dati ",ifelse(tobs<tc & h1=="<" | tobs>tc & h1==">", " non sono", "sono")," coerenti con \\(H_{0}\\) al LdS del ",ifelse(H1=="\\neq",2*alpha*100,alpha*100),"%
    ")
  
  if (H1 == ">") {R <- c(tc,4); A <- c(-4,tc)}
  if (H1 == "<") {R <- c(-4,tc); A <- c(tc,4)}
  if (H1 == "\\neq") {A <- c(-abs(tc),abs(tc)); R1 <- c(-4,-abs(tc)); R <- c(4,abs(tc))}
  curve(dt(x,n1+n2-2),-4,4,axes=F,xlab="Z",ylab="")
  
  lines(A,c(0,0),lwd=2,col=4)
  if (H1 == "\\neq") {
    lines(R1,c(0,0),lwd=2,col=2) 
    axis(1,c(-4,-tc,0,tc,4),round(c(-4,-tc,0,tc,4),3))
  } else axis(1,round(c(-4,4,0,tc),3))
  lines(R,c(0,0),lwd=2,col=2)
  points(tobs,0,pch=4,cex=2)
  text(tobs,.05,expression(z[obs]))
  axis(2)
  
  if (H1 == ">") {pval <- paste("P(Z>z_{\\text{obs}})=P(Z>",round(tobs,),")=",format(1-pnorm(tobs),digits=4, scipen=8))}
  if (H1 == "<") {pval <- paste("P(Z<z_{\\text{obs}})=P(Z<",round(tobs,2),")=",format(pnorm(tobs),digits=4, scipen=8))}
  if (H1 == "\\neq") {pval <- paste("P(|Z|>|z_{\\text{obs}}|)=2P(Z>|z_{\\text{obs}}|)=2P(Z>|",round(tobs,2),"|)=",format(2*pnorm(-abs(tobs)),digits=4, scipen=8))}
  
  c2 <- paste("Il \\(p_{\\text{value}}\\) è
    $$",pval,"$$")
  
  return(paste(c0,c1,c2,sep="\n\n"))
}

