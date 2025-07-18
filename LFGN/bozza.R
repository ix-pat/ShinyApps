pi_val <- 0.6
n <- 100
n_seq <- seq(1,   n, length.out = 500)
x_seq <- seq(0,   1,   length.out = 500)

Z <- outer(x_seq, n_seq, Vectorize(function(x, n) {
  mu <- pi_val
  sigma <- sqrt(pi_val * (1 - pi_val) / n)
  d <- dnorm(x, mean = mu, sd = sigma)
  Znorm <- pnorm(1, mean = mu, sd = sigma) - pnorm(0, mean = mu, sd = sigma)
  d / Znorm * (x >= 0 & x <= 1)
}))

# Colori da bianco a blu
mycols <- colorRampPalette(c("white", "blue","yellow"))(100)

# Plot

plot(c(0,n),c(0,1),axes=F,xlab = "n", ylab = expression(S[n]/n),main = expression(paste("Convergenza di ", frac(S[n], n), " → ", pi)),type="n")

if (immagine){
image(
  x = n_seq, y = x_seq, z = t(Z),
  col = mycols,
  add=T
  )
)
abline(h = pi_val, lty = 2)
axis(1)
axis(2,(0:10)/10,las=2)
}

if (prossimo){
x <- rbinom(n,1,pi_val)
points(1:n,cumsum(x)/1:n,col=1,type = "l",pch=16,cex=.5)
points(1:n,cumsum(x)/1:n,col=1,type = "p",pch=16,cex=.5)
}