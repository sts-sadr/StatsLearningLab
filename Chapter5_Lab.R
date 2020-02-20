########################
#Chapter 5 Lab
########################

####Validation set approach
library(ISLR)
attach(Auto)
dim(Auto)
summary(Auto)
set.seed(1)
train = sample(392, 392/2) ##split data
lm.fit = lm(mpg~horsepower, data=Auto, subset = train)
lm.pred = predict(lm.fit, Auto[-train]) 
mse = mean((as.vector(Auto[-train, 1]) - as.vector(lm.pred))^2)
mse ##R has very confusing index 

##################
#Leave-one-out CV
##################
library(boot)
head(Auto)
glm.fit = glm(mpg~horsepower, data = Auto) #simple LR
cv.glm = cv.glm(Auto, glm.fit) #use loocv 
names(cv.glm)
cv.glm$delta ##error rate
#we can better use loocv in order to test different model
cv.err = rep(0:5)
for (i in 1:5) {
  glm.fit = glm(mpg~poly(horsepower, i), data = Auto)
  cv.fit = cv.glm(Auto, glm.fit)
  cv.err[i] = cv.fit$delta[1]
}
cv.err

########################
#K-fold cross-validation
########################
library(ISLR)
library(boot)
set.seed(18)
cv.error.10 = rep(0, 10)
for (i in 1:10) {
  glm.fit = glm(mpg~poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K = 10)$delta[1]
} #delta has two values, the first is the standard k-fold CV 
#the second is a bias-corrected version. 
cv.error.10


################
#The Bootrap 
################
#applied in any situation
#first, create a function that computes the statistic of interest
#second, perform it by repeatedly sampling obvs from data with replacement
alpha.fn = function(data, index) {
  x = data$X[index]
  y = data$Y[index]
  return((var(y)-cov(x, y)/(var(x)+var(y)-2*cov(x, y))))
}
alpha.fn(Portfolio, 1:100)

set.seed(1)
result = boot(Portfolio, alpha.fn, R=1000)
result
###estimate coefficient accuracy with Bootstrap
library(ISLR)
#1. function computes stats of interest, coefficients here
se.fn = function(data, index) {
  lm.fit = lm(mpg~horsepower, data = Auto, subset = index)
  return(lm.fit$coefficients)
}
#2, resampling
set.seed(1)
boot(Auto, se.fn, R = 1000)

