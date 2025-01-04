n <- 350      # ampiezza campionaria
a <- 5
b <- 1

brk  <- c(-15,-5,0,8, 10)
nnn  <- c( 5,10,20, 10)
nnn  <- round(nnn/sum(nnn)*n)
k <- length(brk)-1
br1  <- brk[-(k+1)]
br2  <- brk[-1]
vunif <- function(nnn, brk){
  k <- length(brk)-1
  br1  <- brk[-(k+1)]
  br2  <- brk[-1]
  xi   <- runif(nnn[1],br1[1],br2[1])
  for (i in 2:k)
    xi <- c(xi,runif(nnn[i],br1[i],br2[i]))
  return(xi)
}

samp <- round(vunif(nnn,brk),2)
nomex <- "Utile"

names(samp) <- nomex

# 
source(path("stat-base.R"))
dat3$`$f_{j\\%}$` <- dat3$`$f_j$`*100
# kable((dat3[1:(k+1),c(1:2,7)])) %>%
#    kable_styling(full_width = F)
n <- length(samp)

