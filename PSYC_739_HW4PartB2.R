x <- ex4Data$refval
y <- ex4Data$larger
z <- ex4Data$smaller

### 2

LL <- function(mu, sigma) {
  R = suppressWarnings(pnorm(x, mu, sigma))
  loglikelihood <-0
  R[R>.9999] <- .9999
  R[R<.0001] <- .0001
  for (i in 1:length(x)) {
    K <- y[i]
    J <- z[i]
    
    loglikelihood <- loglikelihood + (y[i] * log(R[i])) + (z[i] * log(1-R[i])) 
    }
  NLL <- -1*(loglikelihood)
  return(NLL)
  }

fit1 <- mle((LL), start = list(mu = 50, sigma=4), method = "L-BFGS-B", lower = c(50, 4),
            upper = c(200, 100))

summary(fit1)

plot(ex4Data$refval, ex4Data$problarger, col="red", main="MLE fit", xlab = "Reference Values", ylab = "Probability")
lines(x, (pnorm(x, 69.89, 4.38)), col = "blue", lwd="5")

#### 3

LL2 <- function(mu, 
                sigma,
                lower.cutoff = 63,
                upper.cutoff = 78) {
  R = suppressWarnings(pnorm(x, mu, sigma))
  loglikelihood <-0
  R[R>.9999] <- .9999
  R[R<.0001] <- .0001
  for (i in 1:length(x)) {
    K <- y[i]
    J <- z[i]
    
    if (x[i] <= lower.cutoff){
      loglikelihood <- loglikelihood + 0
    } else if (x[i] >= upper.cutoff) {
      loglikelihood <- loglikelihood + 1
    } else {
      loglikelihood <- loglikelihood + (y[i] * log(R[i])) + (z[i] * log(1-R[i]))     
    }
  }
  NLL <- -1*(loglikelihood)
  return(NLL)
}

fit2 <- mle((LL2), start = list(mu = 50, sigma=4), method = "L-BFGS-B", lower = c(50, 4),
            upper = c(200, 100))

summary(fit2)

plot(ex4Data$refval, ex4Data$problarger, col="red", main="Piece-wise MLE fit", xlab = "Reference Values", ylab = "Probability")
lines(x, c(rep(0,9), pnorm(x[10:24], 69.8257, 4.7711), rep(1, 7)), col="blue", lwd=3)
lines(x, stepwise.linear(a,b), col="green")

AIC(fit1,fit2)

#### 3

a<-70
b<-80 

x <- ex4Data$refval

stepwise.linear <- function(a, b) {
  m <- 1/(b-a)
  int <- 0-(m*a)
  y <- rep(NA, length(x)) 
  if (m<0) {
    setl <- 1
    setr <- 0
  } else if (m>0) {
    setl <- 0
    setr <- 1
  }
  for (i in 1:length(x)) {
    if (x[i]<=a) {
      y[i] = setl
    } else if (x[i]>=b) {
      y[i] = setr
    } else {
      y[i] = m*x[i] + int
    }
  } 
  return(y)
}

LLstep <- function(a,b) {
  R = stepwise.linear(a,b)
  loglikelihood <-0
  R[R>.9999] <- .9999
  R[R<.0001] <- .0001
  for (i in 1:length(x)) {
    K <- y[i]
    J <- z[i]    
    loglikelihood <- loglikelihood + (y[i] * log(R[i])) + (z[i] * log(1-R[i])) 
  }
  NLL <- -1*(loglikelihood)
  return(NLL)
}

fit2 <- mle(LLstep, start = list(a = 50, b=75), method = "L-BFGS-B", lower = c(55,71),
            upper = c(70, 85))

plot(ex4Data$refval, ex4Data$problarger, col="red", main="MLE and piece-wise fit", xlab = "Reference Values", ylab = "Probability", lwd="3")
lines(x, (pnorm(x, 69.89, 4.38)), col = "blue", lwd="5")
lines(x, stepwise.linear(a,b), col="green", lwd="5")

summary(fit2)

AIC(fit1,fit2)


########

a1<-80
b1<-70 

a<-60
b<-80
par(mfrow=c(1,1))

stepwise.linear(a,b)
stepwise.linear(a1,b1)

