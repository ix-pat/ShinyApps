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
