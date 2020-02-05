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
table(glm.pred, Direction) #stupid even consider the error rate

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

