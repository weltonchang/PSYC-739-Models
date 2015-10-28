##### Code for HW #1 PSYC 739
##### Written by: Welton Chang
##### Spring 2015

libray(MASS)

setwd("/Users/weltonchang/Desktop")

x <- seq(-10,10,.01)
normdensity <- dnorm(x, mean=5, sd=1)
plot(x, normdensity)

rm(x)

par(mfrow=c(1,1))

curve(dnorm(x,5,1), 0, 10, col="blue")
curve(pnorm(x,5,1), 0, 10, col="red", add=TRUE)

Tx2 <- function(x) dnorm(exp(x+2), 5, 1)
Tx3 <- function(x) pnorm(exp(x+2), 5, 1)
curve(Tx2, xlim=c(-5,10), col="red")
curve(Tx3, xlim=c(-5,10), add= T, col="red")
curve(dnorm(x, 5, 1), add = T, col="blue")
curve(pnorm(x, 5, 1), add = T, col="blue")

Tx_pdf <- function(x) dnorm(exp(x+2), 5, 1)
Tx_cdf <- function(x) pnorm(exp(x+2), 5, 1)
curve(Tx_pdf, xlim=c(-5,10), ylim=c(0,1))
curve(Tx_cdf, xlim=c(-5,10), add = T, lty=3)
curve(dnorm(x, 5, 1), add = T, col="blue")
curve(pnorm(x, 5, 1), add = T, col="blue", lty=3)

x <- function(x) {exp(y+2)}

curve(dnorm(x, 5, 1), 0, 10, col="purple")
curve(pnorm(x, 5, 1), 0, 10, col="gray")

Tx2 <- function(x) dnorm(exp(x+2), 5, 1)
curve(Tx2, xlim=c(-5,5))
curve(dnorm(x,5,1), add=TRUE)

plot(curve(dnorm(x,5,1), 0, 10),
     main = "Probability distribution function",
     xlab = "Normal Distribution, mean=5, var=1",
     ylab = "Probability distribution")

plot(curve(pnorm(x,5,1), 0, 10),
     main = "Cumulative distribution function",
     xlab = "Normal Distribution, mean=5, var=1",
     ylab = "Probability distribution")

x<- function(x) {exp(y+2)}
curve(pnorm(x, 5, 1), col="yellow")
curve(dnorm(x, 5, 1), col="purple")
rm(x)
curve(dnorm(x,5,1), 0, 10, col="blue")
curve(pnorm(x,5,1), 0, 10, col="red")

f<- function(x) {exp(y+2)}

curve(dnorm((log(x)-2), 5, 1))
curve(pnorm((log(x)-2), 5, 1))

rnorm(10000, 5, 1)

x <- rnorm(100000, 5, 1)
hist(x)
d <- density(x)
plot(d)

plot(exp(y+2))

Tx <- function(x) log(pnorm(x, 5, 1))-2
curve(Tx, xlim=c(-10,10))

rm(x)
varx <- rnorm(1000, 15, ((0.5+0.2x)^2))

Jpdf <- function(x) pnorm(, 15, (0.5+0.2x)^2)

x1 <- 5
x2 <- 10
xvec <- c(5,10)

y<- [0:20:0.02]

y <- 0.02*(0:1000)
PDF <- function(x) pnorm(y, 10, (0.5+0.2*x)^2)

plot(pnorm(y, 10, (0.5+0.2*5)^2))

y <- 0.02*(0:1000)
par(mfrow=c(1,1))
plot(y, dnorm(y, 5, (0.5+0.2*5)^2), col="red")
par(new=TRUE)
plot(y, dnorm(y, 10, (0.5+0.2*10)^2), add=T, col="blue")

PDF7.5 <- function(x) dnorm(7.5, x, (0.5+0.2*x)^2)
x<-0.03*(0:1000)
plot(x, PDF7.5(x), col="red")
max(PDF7.5(x))
abline(v=6.03, col="blue")

PDF7.5(x[202])
match(max(PDF7.5(x)), PDF7.5(x))
x[202]

PDF7.5(x[239])
match(max(PDF7.5(x)), PDF7.5(x))
x[239]



PDFfinal <- function(x, y) ((dnorm(y, x, (0.5+0.2*x))) / (dnorm(x, 10, sqrt(8))))
PDFfinal <- function(x, y) ((dnorm(y, x, (0.5+0.2*x))) / (dnorm(x, 10, sqrt(8))))
plot(x, PDFfinal(x,7.5), col="purple")

PDFx <- (dnorm(x, 10, sqrt(8)))


PDFfinal2 <- function(x, y) (((dnorm(x, 10, sqrt(8)))*(dnorm(y, x, (0.5+0.2*x))))/sum(((dnorm(x, 10, sqrt(8)))*(dnorm(y, x, (0.5+0.2*x))))))
par(new=TRUE)
x[273]
curve(PDFfinal2(x,x))
plot(x, PDFfinal2(x,y), col="purple")
par(new=TRUE)
plot(x, PDF7.5(x), col="red")
par(new=TRUE)
plot(PDFx, col="blue")
abline(v=7.14, col="green")
abline(v=8.16, col="green")

plot(x, PDF7.5(x), col="green")
abline(v=30, col="yellow")
abline(v=6.03, col="blue") 

max(PDFfinal2(x, 7.5))
dnorm(PDFfinal(x,7.5))
match(max(PDFfinal2(x, 7.5)), PDFfinal2(x, 7.5))
x[273]

PDF7.5 <- function(x) dnorm(7.5, x, (0.5+0.2*x))
x<-0.03*(0:1000)
plot(x, PDF7.5(x), col="red")
max(PDF7.5(x))
abline(v=7.14, col="blue")

legend()


sum(PriorPDF*PDFyx[i,])
PriorPDF <- dnorm(x, 10, sqrt(8))
PDFyx <- dnorm(y, x, (0.5+0.2*x))
PDFBayes <- function(x, y) (((dnorm(x, 10, sqrt(8)))*(dnorm(y, x, (0.5+0.2*x))))/sum(((dnorm(x, 10, sqrt(8)))*(dnorm(y, x, (0.5+0.2*x))))))

max(PDF200y1(y))

plot(PDF200y1(y))
plot(PDF200x1(x))

plot(PDF200x1(x), PDF200y1(y), col="red")
mean(PDF200x1(x))
abline(v=0.09419192)

plot(y, PDF200y1(y), col="blue")
plot(y, PDF200y1(mode(y)))
mean(PDF200y1(y))
abline(h=0.1192278)

plot(y, PDF200y1(max(y)))
mode(PDF200y1(y))
abline(h=0.0289708)

PDFBayes <- function(x, y) (((dnorm(x, 10, sqrt(8)))*(dnorm(y, x, (0.5+0.2*x))))/sum(((dnorm(x, 10, sqrt(8)))*(dnorm(y, x, (0.5+0.2*x))))))

PDFBayesMAP <- function(x, y) (((dnorm(x, 10, sqrt(8)))*(dnorm(y, x, (0.5+0.2*x))))/sum(((dnorm(x, 10, sqrt(8)))*(dnorm(y, x, (0.5+0.2*x))))))

PDFBayesLS <- function(x-y)^2 (((dnorm(x, 10, sqrt(8)))*(dnorm(y, x, (0.5+0.2*x))))/sum(((dnorm(x, 10, sqrt(8)))*(dnorm(y, x, (0.5+0.2*x))))))

plot(PDFBayesMAP(y))

plot(y, PDFBayesLS(x,y))

plot(y, PDFBayesMAP(x,y))
max(PDFBayes(x,y))
mean(PDFBayes(x,y))
abline(v=max(PDFBayes(x,y)))
abline(v=mean(PDFBayes(x,y)))

plot(x, PDFBayes(x,y))
abline(h=max(PDFBayes(x,y)))
abline(h=mean(PDFBayes(x,y)))

max(PDFBayes(x,y))

fMAP <- function(x,y) max((((dnorm(x, 10, sqrt(8)))*(dnorm(y, x, (0.5+0.2*x))))/sum(((dnorm(x, 10, sqrt(8)))*(dnorm(y, x, (0.5+0.2*x)))))))





