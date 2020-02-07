############10 
library(ISLR)
dim(Weekly)
summary(Weekly)
attach(Weekly)
####a
round(cor(Weekly[, -c(1, 9)]), 3)
pairs(Weekly[, -c(1, 9)])
#there is not any significant relationship between variables from
#the correlation matrix and correlation plot
####b
logis.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
                family = 'binomial')
summary(logis.fit)
###Lag2 appears to be significant. 
####c
logis.pred = predict(logis.fit, type='response')
log.pred = rep('Down', 1089)
log.pred[logis.pred > 0.5] = "Up"
table(log.pred, Direction)
