---
output: html_document
---

```{r, echo=FALSE}
### Calcola la probabilità di una Normale qualunque su un intervallo
##  Input
##  x1 limite inferiore
##  x2 limite superiore
##  mm media
##  ss varianza
##  vnam nome X
##  mu nome media
##  sigma noem varianza

# if (!exists(vnam)) vnam <- "X"
# if (!exists(mu)) mu <- "\\mu"
# if (!exists(sigma)) sigma <- "\\sigma"

z1 <- round((x1 - mm)/sqrt(ss),2)
z2 <- round((x2 - mm)/sqrt(ss),2)
f1 <- round(pnorm(abs(z1)),4)
f2 <- round(pnorm(abs(z2)),4)
res <- round(pnorm(z2),4)-round(pnorm(z1),4)

if (x1<=mm & x2>=mm) {
  p1 <- paste("\\Phi(", z2,")-(1-\\Phi(",-z1,")) \\\\", "&=& ",f2,"-(1-",f1,") \\\\")}
if (x1>=mm & x2>=mm) {
  p1 <- paste(f2,"-",f1,"\\\\")
  }
if (x1<=mm & x2<mm) {
  p1 <- paste("(1-\\Phi(", -z2,"))-(1-\\Phi(",-z1,")) \\\\","&=& (1-",f2,")-(1-",f1,") \\\\")
  }

mm <- ifelse(mm>=0,mm,paste("(",mm,")"))

```
\begin{eqnarray*}
P(`r x1`<`r vnam`\leq `r x2`) &=& P\left( \frac {`r x1` - `r mm`}{\sqrt{`r ss`}} < \frac {`r vnam` - `r mu`}{`r sigma`} \leq \frac {`r x2` - `r mm`}{\sqrt{`r ss`}}\right)  \\
              &=& P\left( `r z1` < Z \leq `r z2`\right) \\
              &=& \Phi(`r z2`)-\Phi(`r z1`)\\
              &=& `r p1`
              &=& `r res`
\end{eqnarray*}
