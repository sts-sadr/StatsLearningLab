#########################
#Lab Chapter 4
#########################
#The stock Market Data
#EDA
library(ISLR)
names(Smarket)
head(Smarket)
pairs(Smarket)
round(cor(Smarket[, -9]), 3)
attach(Smarket)
plot(Volume) ##after attach(), we do not need specify the data

#####################
#Logistic Regression
#####################
#Build model
logis.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
                data=Smarket, family=binomial)
names(logis.fit)
#check the property/results
summary(logis.fit)
logis.pred = predict(logis.fit, type='response') #type here specifies prob for y=1
logis.pred[1:10] #if no new data to predict, the function return train results
contrasts(Direction) #show the class, the function creates dummy variables for class
#Now we need to specify the decision boundry
glm.pred = rep("Down", 1250)
glm.pred[logis.pred >.5] = 'Up'  
#show the confusion matrix in the train model
table(glm.pred, Direction) #stupid even consider the training error rate

###################
##Build model with train and test data
train = (Year<2005)
test.x = Smarket[!train, ]
dim(Smarket.2005)
test.y = Direction[!train]
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket,
              family=binomial, subset = train) #train model with train data
glm.probs = predict(glm.fit, test.x, type = "response") #test with new data
preds = rep("Down", 252) #create vector with all char 'Down' 
preds[glm.probs>.5] = "up" #change the corresponding y to "up"/ decision boundry  
table(preds, test.y)
###predict with new data observation
predict(glm.fit, newdata = data.frame(Lag1 = 1.2, Lag2=1.3, Lag3=2.5,
                                      Lag4=2.1, Lag5=4.1, Volume=2),
        type = "response")

##############################
#Linear Discriminant Analysis 
##############################
library(MASS)
#build model
lda.fit = lda(Direction~Lag1+Lag2, data = Smarket, subset = train)
names(lda.fit)
lda.fit$means #means of different classes, we need the mean vector  
lda.fit$scaling #coefficient 
plot(lda.fit)
#predict 
lda.pred = predict(lda.fit, test.x[, 2:3])
names(lda.pred)
lda.pred$class #predicted class
lda.pred$posterior #posterior values
lda.pred$x ###what is this? 

lda.class = lda.pred$class
table(lda.class, test.y)

#################################
#Quadratic Discriminant Analysis
#################################
qda.fit = qda(Direction~Lag1+Lag2, data = Smarket, subset = train)
names(qda.fit)
qda.pred = predict(qda.fit, test.x)
names(qda.pred)
qda.class = qda.pred$class #predicted class 
table(qda.class, test.y) #confusion matrix

##################################
#K-nearest Neighbors
##################################
library(class)
train = (Year<2005)
x.test = cbind(Lag1, Lag2)[!train, ]
x.train = cbind(Lag1, Lag2)[train,]
y.train = Direction[train]
y.test = Direction[!train]

#build KNN
set.seed(1)
knn.pred = knn(x.train, x.test, y.train, k=1)
table(knn.pred, y.test)
#change k to 3
knn.pred2 = knn(x.train, x.test, y.train, k=3)
table(knn.pred2, y.test)


########################################
#A Application to Caravan Insurance Data
########################################
dim(Caravan)
attach(Caravan)
summary(Purchase)
#we first standardize the data, becasue different scales
standard.x = scale(Caravan[, -86]) #scale function standardize the variables 
var(Caravan[, 1])
var(standard.x[, 1])
#split the dataset into train and testing
test = 1:1000
x.train = standard.x[-test, ] 
y.train = Purchase[-test]
x.test = standard.x[test, ]
y.test = Purchase[test]
set.seed(1)
knn.pred = knn(x.train, x.test, y.train, k=3)
knn.pred
table(knn.pred, y.test)

