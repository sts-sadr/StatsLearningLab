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
(54+557)/1089
####d 
train = (Year >= 1999 & Year <= 2008)
x.train = Weekly[train, ]
x.test = Weekly[!train, ]
y.train = Direction[train]
y.test = Direction[!train]
log.fit.2 = glm(Direction~Lag2, data=Weekly, family = 'binomial',
                subset = train)
log.pred.2 = predict(log.fit.2, x.test, type = 'response')
log.pred.2.res = rep("Down", 568)
log.pred.2.res[log.pred.2>=0.5] = "Up"
table(log.pred.2.res, y.test)

####e
library(MASS)
lda.fit = lda(Direction~Lag2, data = Weekly, subset=train)
lda.pred = predict(lda.fit, x.test)
pred.class.lda = lda.pred$class
table(pred.class.lda, y.test)

####f
qda.fit = qda(Direction~Lag2, data=Weekly, subset = train)
qda.pred = predict(qda.fit, x.test)
pred.class.qda = qda.pred$class
table(pred.class.qda, y.test)

####g 
library(class)
x.train = as.data.frame(Lag2[train])
x.test = as.data.frame(Lag2[!train])
y.train = Direction[train]
y.test = Direction[!train]
knn.fit = knn(x.train, x.test, y.train, k=1)
table(knn.fit, y.test)

####h 
#lda has the best accuracy. 

####i


