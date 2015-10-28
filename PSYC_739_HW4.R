install.packages("dice")
install.packages("gtools")
library(dice)

x<- rbinom(100000,10,1/6)
table(x)
hist(x, main="Dice rolling exercise", ylab="Total # of times observed", xlab="# correct out of 10 in single trial")
y<- dbinom(5:10, 10, 1/6)
sum(y)

x1<- dbinom(1:10, 10, 1/2)
plot(x1, main="Probability of guessing top or bottom face", xlab="Total correct out of 10", ylab="Probability")
x2<- rbinom(100000, 10, 1/2)
table(x2)
hist(x2, main="Top or bottom face 100000 trial simulation", xlab="# correct out of 10", ylab= "Total # of times observed")

x3<- dbinom(5:10, 10, 1/2)
sum(x3)
-2*(log(0.015)) + 2*(log(0.24))

(((1/6)^5)*(1-(1/6))^5) / (((1/2)^5)*(1-(1/2))^5)
0.015/0.24 

(0.013)*(0.95) / (0.015)
(0.24)*(0.05) / (0.64)

(0.013)*(0.5) / (0.015)
(0.24)*(0.5) / (0.64)


#######

install.packages("R.matlab")
install.packages("R.methodsS3-package")
library(R.methodsS3)
library(R.matlab)
setwd("/Users/weltonchang/Desktop")
ex4Data <- readMat("../ex4Data.mat")

ex4Data$V4 <- NULL 
ex4Data
ex4Data$refval <- ex4Data$V1
ex4Data$larger <- ex4Data$V2
ex4Data$smaller <- ex4Data$V3
ex4Data$V1 <- NULL 
ex4Data$V2 <- NULL
ex4Data$V3 <- NULL

ex4Data$prob <- ex4Data$larger

#####

plot(ex4Data$problarger, ex4Data$refval)

plot(ex4Data$refval, ex4Data$problarger)

smooth <- smooth.spline(ex4Data$refval, ex4Data$problarger, spar=0.35)
plot(ex4Data$refval, ex4Data$problarger)
lines(smooth, col='red', lwd=2)

### 1

lo <- loess(ex4Data$problarger~ex4Data$refval)
xl <- seq(min(ex4Data$refval),max(ex4Data$refval), (max(ex4Data$refval) - min(ex4Data$refval))/1000)
plot(ex4Data$refval,ex4Data$problarger, main="ex4Data psychometric curve", xlab = "Reference Values", 
     ylab = "Probability subject says larger than test", col = "blue")
lines(xl, predict(lo,xl), col='red', lwd=2)

### 2

install.packages("stats4")
library(stats4)
library(MASS)

fit1 <- fitdistr(ex4Data$refval, "normal")
fit2 <- pnorm(ex4Data$refval, 70, 8.9443)

summary(fit1)

plot(ex4Data$refval, ex4Data$problarger, ylim=c(0,1), col = "red", main = "ML fit under normal distrib", ylab = "MLE fit", xlab = "Ref Values")
lines(ex4Data$refval, fit2, lwd=2, col="blue")

### 3

lo1 <- loess(ex4Data1$probother~ex4Data1$refval)
xl1 <- seq(min(ex4Data1$refval),max(ex4Data1$refval), (max(ex4Data1$refval) - min(ex4Data1$refval))/1000)
smooth1 <- smooth.spline(ex4Data1$refval, ex4Data1$problarger, spar=0.35)
fit2<- lm(ex4Data1$probother~ex4Data1$refval)

plot(ex4Data$refval, ex4Data$problarger, ylim=c(0,1), col = "red", main = "Piece-wise fit", ylab = "MLE fit", xlab = "Ref Values")
abline(fit2, lwd=2, col="blue")

#### Other tries

normal.lik1 <- function(theta,y){
  mu<-theta[1]
  sigma2<-theta[2]
  n<-nrow(y)
  logl<- -.5*n*log(2*pi) -.5*n*log(sigma2) -
    (1/(2*sigma2))*sum((y-mu)**2)
  return(-logl)
}

normal.lik2<-function(theta,y){
  mu<-theta[1]
  sigma<-theta[2]
  n<-nrow(y)
  z<-(y-mu)/sigma
  logl<- -n*log(sigma) - sum(log(dnorm(z)))
  return(-logl)
}

optim(c(,),normal.lik1,y=ex4Data$refval,method="BFGS")

#########

x <- ex4Data$refval
y <- ex4Data$larger
z <- ex4Data$smaller

LL <- function(mu, sigma) {
  R = suppressWarnings(dnorm(x, mu, sigma))
  likelihood <-0
  R[R>.9999] <- .9999
  R[R<.0001] <- .0001
  for (i in 1:length(x)) {
    K <- y[i]
    J <- z[i]
    
    for (J in 1:K) {
      likelihood <- likelihood + log(R[i])
    }
    for (K in 1:J) {
      likelihood <- likelihood + log(1- R[i])
    } 
  }
NLL <- -1*(likelihood)
return(NLL)
}

fit1 <- mle((LL), start = list(mu = 50, sigma=2), method = "L-BFGS-B", lower = c(50, 1),
    upper = c(200, 100))
summary(fit1)

plot(ex4Data$refval, ex4Data$problarger, col="red", main="MLE fit", xlab = "Reference Values", ylab = "Probability")
lines(x, (pnorm(x, 76, 6.6)), col = "blue", lwd="5")

#### 3

LL <- function(mu, sigma) {
  R <- suppressWarnings(pnorm(x, mu, sigma))
  likelihood <-0
  R[R>.9999] <- .99999
  R[R<.0001] <- .00001
  for (i in 1:length(x<61)) {
    K <- y[i] 
    J <- z[i] 
    
    for (J in 1:K) {
      likelihood <- likelihood + log(R[i])
    }
    for (L in 1:K) {
      likelihood <- likelihood + log(R[i])
    }
  NLL <- -1*(likelihood)
  return(NLL)
}
}

fit1 <- mle((LL), start = list(mu = 70, sigma=4), method = "L-BFGS-B", lower = c(-Inf, 0),
            upper = c(Inf, Inf))
summary(fit1)

plot(ex4Data$refval, ex4Data$problarger, col="red", main="MLE fit 2", xlab = "Reference Values", ylab = "Probability")
lines(x, ex4Data1$probother, col = "blue", lwd="5")

#### 3

LL <- function(mu, sigma) {
  R = suppressWarnings(pnorm(x, mu, sigma))
  likelihood <- 1
  R[R>.9999] <- .9999
  R[R<.0001] <- .0001
  for (i in 1:length(x)) {
    K <- y[i]
    J <- z[i]
    
    for (J in 1:K) {
      likelihood <- likelihood + log(R[i])
    }
    for (L in 1:K) {
      likelihood <- likelihood + log(1-R[i])
    } 
  }
  NLL <- -1*(likelihood)
  return(NLL)
}

install.packages("bbmle")
library(bbmle)

norm.fit<-function(mu,sigma) {
  -sum(suppressWarnings(dnorm(x,mu,sigma,log=T))) } 

mle.results<-mle2(norm.fit,start=list(mu=70,sigma=4),data=list(ex4Data$refval))
summary(mle.results)

plot(ex4Data$refval, ex4Data$problarger, col="red", main="MLE fit 2", xlab = "Reference Values", ylab = "Probability")
points(x, pnorm(x, 70, 8.9))

regress.ll<-function(int,slope,sigma) {
  predicted.value<-int+slope*predictor.value 
  -sum(dnorm(x,mean=predicted.value,sd=sigma,log=T)) } 

mle.results2 <- mle2(regress.ll,start=list(int=0,slope=0,sigma=0),data=list(ex4Data$refval) )

mle(LL, start = list(mu = 1, sigma=1), method = "L-BFGS-B", lower = c(-Inf, 0),
    upper = c(Inf, Inf))

AICctab(mle.result,mle.result2,weights=T)
