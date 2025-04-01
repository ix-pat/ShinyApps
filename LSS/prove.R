x = c(0, 1, 2, 3)
y = c(2.0, 3.5, 2.5, 4.0)

model <- rq(y ~ x, tau = 0.5)
lsmod <- lsfit(x,y)
plot(x,y)
abline(lsmod)
abline(model$coefficients,col=2)

text(0,4,sum(lsmod$residuals^2))
text(0,3.9,sum(abs(lsmod$residuals)))

text(0.5,4,round(sum(model$residuals^2),2),col=2)
text(0.5,3.9,round(sum(abs(model$residuals)),2),col=2)

abline(h=mean(y),lty=2)
abline(v=mean(x),lty=2)


x <- rnorm(100)
y <- x + rnorm(100,0,1)
plot(x,y)

model <- rq(y ~ x, tau = 0.5)
lsmod <- lsfit(x,y)
plot(x,y)
abline(lsmod)
abline(model$coefficients,col=2)

text(0,4,round(sum(lsmod$residuals^2),2))
text(0,3.9,round(sum(abs(lsmod$residuals)),2))

text(0.5,4,round(sum(model$residuals^2),2),col=2)
text(0.5,3.9,round(sum(abs(model$residuals)),2),col=2)

abline(h=mean(y),lty=2)
abline(v=mean(x),lty=2)

x[1] <- -2
y[1] <- 5

model <- rq(y ~ x, tau = 0.5)
lsmod <- lsfit(x,y)
plot(x,y,ylim=c(-3,3))
abline(lsmod)
abline(model$coefficients,col=2)

text(0,4,round(sum(lsmod$residuals^2),2))
text(0,3.9,round(sum(abs(lsmod$residuals)),2))

text(0.5,4,round(sum(model$residuals^2),2),col=2)
text(0.5,3.9,round(sum(abs(model$residuals)),2),col=2)

abline(h=mean(y),lty=2)
abline(v=mean(x),lty=2)

x[1] <- -2
y[1] <- 150
