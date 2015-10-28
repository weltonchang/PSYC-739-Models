#### PSYC 739 HW#2
#### Prof Alan Stocker, Fall 2014
#### Code by: Welton Chang

### Select 200 machine parts x and find a sample value y for each x
x1 <- rnorm(200, 10, sqrt(8))
y1 <- rnorm(200, x1, (0.5+0.2*x1))

### Check the two random samples
hist(x)
hist(y)
plot(x,y, col="blue")
abline(0,1)

par(mfrow=c(1,1))
par(new=TRUE)
mean(x)
plot(x, x)

abline(v=10.22116)

install.packages("flux")
library(flux)

### Set x and y variables
x <- 0.03*(1:1000)
y <- 0.02*(1:1000)

### Select 200 machine parts x and find a sample value y for each x
x1 <- rnorm(200, 10, sqrt(8))
y1 <- rnorm(200, x1, (0.5+0.2*x1))

### Set for loop variables
PriorPDF <- dnorm(x, 10, sqrt(8))
PDFyx <- matrix(0, nrow=1000, ncol=1000)
PDFy <- vector(mode='numeric', length(y))
Posterior <- matrix(0, nrow=1000, ncol=1000)
MAP <- vector(mode='numeric', length(y))
BLS <- vector(mode='numeric', length(y))

### For loop
for (i in 1:length(x)) { 
  PDFyx[i,] <- (dnorm(y[i], x, (0.5+0.2*x)))
  PDFy[i] <- auc(x, PDFyx[i,]*PriorPDF)
  Posterior[i,] <- (PDFyx[i,]*PriorPDF)/PDFy[i]
  MAP[i] <- which(diff(sign(diff(Posterior[i,])))==-2)+1
  BLS[i] <- auc(x, (x)*(Posterior[i,]))
}

plot(y1, x1, col="red", ylim=c(0,20), xlim=c(0,20), main="MAP/BLS")
lines(y, x[MAP], col="black")
lines(x1, x1, col="blue")
lines(y, BLS, col="green")

