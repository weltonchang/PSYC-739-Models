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
    if (J>0){
      for (J in 1:K) {
        likelihood <- likelihood + log(R[i])
      }
    }
    if (K>0) {
      for (K in 1:J) {
        likelihood <- likelihood + log(1- R[i])
      } 
    }
  }
  
  NLL <- -1*(likelihood)
  return(NLL)
}

LL(40,2)

fit1 <- mle((LL), start = list(mu = 50, sigma=2), method = "L-BFGS-B", lower = c(50, 1),
            upper = c(200, 100))
summary(fit1)

plot(ex4Data$refval, ex4Data$problarger, col="red", main="MLE fit", xlab = "Reference Values", ylab = "Probability")
lines(x, (pnorm(x, 70, 8.9)), col = "blue", lwd="5")


#### 2 Working

LL <- function(mu, sigma) {
  R = suppressWarnings(dnorm(x, mu, sigma))
  likelihood <-0
  R[R>.9999] <- .9999
  R[R<.0001] <- .0001
  for (i in 1:length(x)) {
    K <- y[i]
    J <- z[i]
    for (J in K<J) {
      likelihood <- likelihood + log(R[i])
    }
    for (K in J<K) {
      likelihood <- likelihood + log(1- R[i])
    } 
  }
  NLL <- -1*(likelihood)
  return(NLL)
}

fit1 <- mle((LL2), start = list(mu = 50, sigma=2), method = "L-BFGS-B", lower = c(50, 1),
            upper = c(200, 100))
summary(fit1)

plot(ex4Data$refval, ex4Data$problarger, col="red", main="MLE fit", xlab = "Reference Values", ylab = "Probability")
lines(x, (pnorm(x, 70, 8.9)), col = "blue", lwd="5")

#### 3 Working

LL2 <- function(mu, sigma) {
  R = suppressWarnings(dnorm(x, mu, sigma))
  likelihood <-0
  R[R>.9999] <- .9999
  R[R<.0001] <- .0001
  for (i in 1:length(x)) {
    K <- y[i]
    J <- z[i]
    for (J in K<J) {
      likelihood <- likelihood + log(R[i])
    }
    for (K in J<K) {
      likelihood <- likelihood + log(1- R[i])
    } 
  }
  NLL <- -1*sum(likelihood)
  return(NLL)
}

fit2 <- mle((LL2), start = list(mu = 50, sigma=4), method = "L-BFGS-B", lower = c(50, 4),
            upper = c(200, 100))
summary(fit2)

plot(ex4Data$refval, ex4Data$problarger, col="red", main="MLE fit", xlab = "Reference Values", ylab = "Probability")
lines(x, (pnorm(x, 70, 8.9921)), col = "blue", lwd="5")


