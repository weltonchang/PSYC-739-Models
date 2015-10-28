### SDT HW#2 part B
### PSYC 739
### Author: Welton Chang

install.packages("pROC")
library(pROC)
install.packages("ROCR")
library(ROCR)
install.packages("flux")
library(flux)

x <- (0:1000)
source <- dpois(x, lambda=20)
detector <- dpois(x, lambda=5)
crit <- (0:40)
par(mfrow=c(2,2))

HR <- 1- ppois(crit, lambda=20)
FPR <- 1- ppois(crit, lambda=5)
HR1 <- 1- ppois(crit, lambda=5)
HR2 <- 1- ppois(crit, lambda=10)
HR3 <- 1- ppois(crit, lambda=100)

par(mfrow=c(2,2))
plot(FPR, HR, col="black")
plot(FPR, HR1, col="blue")
plot(FPR, HR2, col="red")
plot(FPR, HR3, col="green")

par(mfrow=c(1,1))
plot(FPR, HR, col="black", main="ROC Curves")
lines(FPR, HR1, col="blue")
lines(FPR, HR2, col="red")
lines(FPR, HR3, col="green")

# Part B

# Probability of noise or signal
pnoise <- 0.8
psignal <- 0.2

# 1000 observations from poisson distribution
x1 <- rpois(800, lambda=5)
x2 <- rpois(200, lambda=10)
x3 <- c(x1, x2)

# Probability distribution functions based on 1000 observations
noise <- dpois(x3, lambda=5)
signal <- dpois(x3, lambda=10)

plot(x3, noise)
points(x3, signal)

x3[1]

# Set loss functions

L1 <- 1 # CR
L2 <- 4*L1 # FA
L3 <- L2*2 # Miss
L4 <- 1 # Hit

EL1 <- c(noise*pnoise*L1 + signal*psignal*L3) # EL from saying no
EL2 <- c(noise*pnoise*L2 + signal*psignal*L4) # EL from saying yes

# Check EL vectors
length(EL1)
length(EL2)

# Plot to check
plot(x3, signal, ylim=c(0,1), main="psignal=0.2")
points(x3, noise, col="green")
points(x3, EL1, col="blue")
points(x3, EL2, col="red")

plot(x3, EL1, col="blue", ylim=c(0,1))
points(x3, EL2, col="red")

#### Finding HR / FPR
d1 <- which(EL1>EL2)
d1 <- sum(EL1>EL2)
d1

e1 <- rbinom(1000, 1, 0.2)
e2 <- which(e1>0)
205/263

### 4, prob = 0.5

pnoise1 <- 0.5
psignal1 <- 0.5

# 1000 observations from poisson distributions
x11 <- rpois(500, lambda=5)
x22 <- rpois(500, lambda=10)
x33 <- c(x11, x22)
length(x33)

# Prior distributions
noise1 <- dpois(x33, lambda=5)
signal1 <- dpois(x33, lambda=10)

EL3 <- c(noise1*pnoise1*L1 + signal1*psignal1*L3)
EL4 <- c(noise1*pnoise1*L2 + signal1*psignal1*L4)

length(EL3)
length(EL4)

#HR

sum(EL3>EL4)

d2 <- which(EL3>EL4)
d2

e11 <- rbinom(1000, 1, 0.5)
e22 <- sum(e11>0)
e22 <- which(e11>0)
e22

508/668

# Plot to check
plot(x33, signal1, ylim=c(0,1), main="psignal=0.5")
points(x33, noise1, col="green")
points(x33, EL3, col="blue")
points(x33, EL4, col="red")

### 5, FA = 2M, 4H and 4CR

L11 <- 1 # CR
L22 <- 2*L33 # FA
L33 <- 2 # Miss
L44 <- 1 # Hit

EL5 <- noise*pnoise*L11 + signal*psignal*L33
EL6 <- noise*pnoise*L22 + signal*psignal*L44

# HR

sum(EL5>EL6)

#HR

sum(EL5>EL6)
d3 <- which(EL5>EL6)
d3

e111 <- rbinom(1000, 1, 0.2)
e222 <- which(e111>0)
sum(e222>0)

99/231

# Plot to check
plot(x3, signal, ylim=c(0,1), main="FA=2*M, FA=4*H or 4*CR")
points(x3, noise, col="green")
points(x3, EL5, col="blue")
points(x3, EL6, col="red")

