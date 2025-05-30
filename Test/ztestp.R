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
