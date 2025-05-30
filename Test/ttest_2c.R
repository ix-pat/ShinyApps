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
    ", ifelse(h1=="\\neq","Siccome \\(H_1\\) è bilaterale, considereremo \\(\\alpha/2\\), anziché \\(\\alpha\\) $$ $$","$$ $$"),"
  
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
  
  
  c1 <- paste("\\(\\fbox{C}\\) DECISIONE
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
  
  c2 <- paste("\n\n Il \\(p_{\text{value}}\\) è
  $$", pval,"$$")
  return(paste(c0,c1,c2,sep="\n\n"))
}
