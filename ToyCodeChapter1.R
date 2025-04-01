# partialling out
# and failing when p/n is large 
library(sandwich)
set.seed(1)
n <- 100
k <- 1.1 # number of observations per coefficient
1/k
p <- floor(n/k)
beta <- rep(0.5, p)
X <- replicate(p, rnorm(n)) # simplified setting: all predictors are orthogonal 
Y <- X%*%beta + rnorm(n)

m1 <- lm(Y ~ X)
coef(m1)
mY <- lm(Y ~ X[,-1])
mX1 <- lm(X[,1] ~ X[,-1])
Y.tilde <- mY$residuals
X1.tilde <- mX1$residuals
m2 <- lm(Y.tilde ~ X1.tilde -1 ) # no intercept 
coef(summary(m1))[2,]
coef(summary(m2)) # point estimates are the same, but SE's not. 

# Let's put this in a loop and look at the distribution of the estimators 

N.sim <- 1000
beta.LR <- beta.PO <- SE.LR <- SE.PO <- SE.PO2 <- SE.PO2Bis <- c()
for(i in 1:N.sim)
{
  beta <- rep(0.5, p)
  X <- replicate(p, rnorm(n)) 
  Y <- X%*%beta + rnorm(n)
  m1 <- lm(Y ~ X)
  mY <- lm(Y ~ X[,-1])
  mX1 <- lm(X[,1] ~ X[,-1])
  Y.tilde <- mY$residuals
  X1.tilde <- mX1$residuals
  m2 <- lm(Y.tilde ~ X1.tilde -1 ) 
  beta.LR[i] <- coef(summary(m1))[2,1]
  SE.LR[i] <- coef(summary(m1))[2,2]
  beta.PO[i] <- coef(summary(m2))[1] 
  SE.PO[i] <- coef(summary(m2))[2] 
  SE.PO2[i] <- sqrt(vcovHC(m2, type = "HC0"))# Huber-White SE
  SE.PO2Bis[i] <- sqrt(vcovHC(m2, type = "HC3"))# Jacknife 
}

mean(beta.LR)
mean(beta.PO)
sd(beta.LR)
mean(SE.LR) # works very well, even when p approx n. Will likely be less the case when predictors are correlated
mean(SE.PO) # bias, especially when p/n is not small, is fine when e.g. k = 10 (demonstrating that ignoring first step is no problem)
mean(SE.PO2)
mean(SE.PO2Bis)

qqnorm(beta.LR)
qqline(beta.LR)

sd(SE.LR)
sd(SE.PO)


Data <- data.frame(Y = rnorm(n), X1 = rnorm(n), X2 = rnorm(n), X3 = rnorm(n), X4 = rnorm(n))
lm(Y ~ (X1 + X2 + X3 + X1:X2)*X4, data = Data)




## Partialling out to remove linearity 
set.seed(1)
n <- 100
X1 <- rnorm(n)
X2 <- rnorm(n)
Y <- X1 + X2 + rnorm(n)
plot(X1, Y)
abline(coef(lm(Y~X1)))

plot(X2, Y)
abline(coef(lm(Y~X2)))

#Partial out X2
Y.tilde <- residuals(lm(Y ~ X2))
X1.tilde <- residuals(lm(X1 ~ X2))

plot(X1.tilde, Y.tilde)
abline(coef(lm(Y.tilde~X1.tilde))) # same slope 

plot(X2, Y.tilde)
abline(coef(lm(Y.tilde~X2))) # no pattern anymore. So points are shifted so that straight line becomes horizontal






