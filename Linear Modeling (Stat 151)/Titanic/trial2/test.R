library(mlbench)
library(ggplot2)
library(plyr)
library(reshape2)
library(grid)
library(lattice)
library(GGally)
library(rpart)

setwd("~/Desktop/Stat 151/Titanic")
test = read.csv('test.csv')
train = read.csv('train.csv')

allvars = c('Survived', 'Pclass', 'Sex', 'Age', 'SibSp', 'Parch', 'Fare', 'Embarked')

tit = rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,  method="class", data=train[!is.na(train$Age),])
tit2 = rpart(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,  method="class", data=train)

test$Survived.Rpart[!is.na(test$Age)] = as.numeric(predict(tit, test[!is.na(test$Age),])[,2] > .5)
test$Survived.Rpart[is.na(test$Age)] = as.numeric(predict(tit2, test[is.na(test$Age),])[,2] > .5)

ft = glm(Survived ~ Pclass + Sex + cut(Age, c(-1,9,36,100)) + SibSp, family = "binomial", train[!is.na(train$Age),])
# ft = glm(Survived ~ Pclass + Sex + cut(Age, c(-1,9,36,100)) + SibSp + cut(Age, c(-1,9,36,100))*SibSp, family = "binomial", train[!is.na(train$Age),])
ft2 = glm(Survived ~ Pclass + Sex + SibSp, family = "binomial", train)

test$Surv.prob[!is.na(test$Age)] = predict.glm(ft, test[!is.na(test$Age),], type = "response")
test$Surv.prob[is.na(test$Age)] = predict.glm(ft2, test[is.na(test$Age),], type = "response")

test$Survived.glm[!is.na(test$Age)] = as.numeric(test$Surv.prob[!is.na(test$Age)] > .52)
# test$Survived.glm[!is.na(test$Age)] = as.numeric(test$Surv.prob[!is.na(test$Age)] > .5455)
test$Survived.glm[is.na(test$Age)] = as.numeric(test$Surv.prob[is.na(test$Age)] > .62)

test$Survived[test$Survived.Rpart == test$Survived.glm] = test$Survived.Rpart[test$Survived.Rpart == test$Survived.glm]

sum(test$Survived.Rpart != test$Survived.glm)
#34

bad = test[test$Survived.Rpart != test$Survived.glm,-c(1,3,8)]
probs = test$Surv.prob[test$Survived.Rpart != test$Survived.glm]

# Whether or not Age is NA does not affect whether the predictions agree.
# This is good!
sum(is.na(test$Age)) / nrow(test)
#0.2057416
sum(is.na(bad$Age)) / nrow(bad)
#0.2058824

numbad = list()
for (thresh1 in (480:560)/1000) {
  for (thresh2 in (580:660)/1000) {
    
    test$Survived.glm[!is.na(test$Age)] = as.numeric(test$Surv.prob[!is.na(test$Age)] > thresh1)
    test$Survived.glm[is.na(test$Age)] = as.numeric(test$Surv.prob[is.na(test$Age)] > thresh2)
    
    test$Survived[test$Survived.Rpart == test$Survived.glm] = test$Survived.Rpart[test$Survived.Rpart == test$Survived.glm]
    
    numbad[[length(numbad) + 1]] = c(sum(test$Survived.Rpart != test$Survived.glm), thresh1, thresh2)
  }
}

numbad = matrix(unlist(numbad), byrow = TRUE, ncol = 3)
#34 is the minimum number of disagreements achievable by adjusting thresholds



train$Surv.prob[!is.na(train$Age)] = predict.glm(ft, train[!is.na(train$Age),], type = "response")
train$Surv.prob[is.na(train$Age)] = predict.glm(ft2, train[is.na(train$Age),], type = "response")
train$Survived.glm[!is.na(train$Age)] = as.numeric(train$Surv.prob[!is.na(train$Age)] > .52)
train$Survived.glm[is.na(train$Age)] = as.numeric(train$Surv.prob[is.na(train$Age)] > .62)

data = train[,c('Survived', 'Survived.glm' ,'Surv.prob')]
data[data$Survived != data$Survived.glm,]

test$Survived[as.numeric(rownames(bad[!is.na(bad$Age),]))] = as.numeric(test$Surv.prob[as.numeric(rownames(bad[!is.na(bad$Age),]))] > .6)
test$Survived[as.numeric(rownames(bad[is.na(bad$Age),]))] = 0

bad[is.na(bad$Age),]
test$Survived[128] = 0
ft3 = glm(Survived ~ Pclass + Sex + SibSp + Fare, family = "binomial", train)

thresholds = (0:1000)/1000
a = findthresh.ad(ft3, thresholds)
b = findthresh.bc(ft3, thresholds)
which(a == max(a)) - 1
which(b == min(b)) - 1
thresh = .59

predict.glm(ft3, bad[is.na(bad$Age),], type = "response")
test$Survived[as.numeric(names(predict.glm(ft3, bad[is.na(bad$Age),], type = "response")))] =
  predict.glm(ft3, bad[is.na(bad$Age),], type = "response") > thresh




test$Survived[is.na(test$Survived) & abs(test$Surv.prob - .52) > .07] =
  test$Survived.glm[is.na(test$Survived) & abs(test$Surv.prob - .52) > .07]


bad = test[is.na(test$Survived),]

test$Survived[as.numeric(rownames(bad))] = 1


frame = test[,c('PassengerId', 'Survived')]
write.table(frame, "results.csv", row.names=FALSE, sep=",")

# test$Survived[as.numeric(rownames(bad)[which(bad$Surv.prob < .65)])] = 0
# bad = test[is.na(test$Survived),]
# test$Survived[as.numeric(rownames(bad)[which(bad$Surv.prob > .6)])] = 1
# 
# frame = test[,c('PassengerId', 'Survived')]
# names(frame)[2] = 'Survived'
# write.table(frame, "results2.csv", row.names=FALSE, sep=",")
