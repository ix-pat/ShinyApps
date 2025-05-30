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
      ", ifelse(h1=="\\neq","Siccome \\(H_1\\) è bilaterale, considereremo \\(\\alpha/2\\), anziché \\(\\alpha\\)",""),"
    
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
