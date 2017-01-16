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
data = train[,allvars]

# Create pairwise plots of explanatory variables
ggpairs(data, lower=list(continuous="smooth"))

# Age is not very significant
ft = glm(Survived ~ Age, family = "binomial", data = data)
summary(ft)
# Optimal cutoff for Age is 13
ft = glm(Survived ~ (Age<=13), family = "binomial", data = data)
summary(ft)

# Pclas is significant
ft = glm(Survived ~ Pclass, family = "binomial", data = data)
summary(ft)


# Parch alone is not very significant
ft = glm(Survived ~ Parch, family = "binomial", data = data)
summary(ft)
# Optimal cutoff for Parch is 0.
ft = glm(Survived ~ (Parch==0), family = "binomial", data = data)
summary(ft)

# Fare is significant
ft = glm(Survived ~ Fare, family = "binomial", data = data)
summary(ft)

# Embarked is not significant
ft = glm(Survived ~ Embarked, family = "binomial", data = data)
summary(ft)
# Embarked == 'S' is significant.  Most people embarked from this location.
ft = glm(Survived ~ (Embarked=='S'), family = "binomial", data = data)
summary(ft)

data$Age.13 = data$Age < 13
data$Parch.0 = data$Parch == 0
data$Embarked.S = data$Embarked == 'S'

ft = glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, family = "binomial", data)
summary(ft)

ft = glm(Survived ~ Pclass + Sex + Age.13 + SibSp + Parch.0 + Fare + Embarked.S, family = "binomial", data)
summary(ft)

ft = glm(Survived ~ Pclass + Sex + Age.13 + SibSp + Parch.0 + Fare, family = "binomial", data)
summary(ft)

ft = glm(Survived ~ Pclass + Sex + Age.13 + SibSp + Parch.0, family = "binomial", data)
summary(ft)

ft = glm(Survived ~ Pclass + Sex + Age.13 + SibSp, family = "binomial", data)
summary(ft)

ft = glm(Survived ~ Pclass + Sex + Age + SibSp, family = "binomial", data)
summary(ft)

ft = glm(Survived ~ Pclass + Sex + (Age<9) + SibSp, family = "binomial", data)
summary(ft)

ft = glm(Survived ~ Pclass + Sex + cut(Age, c(-1,9,36,100)) + SibSp, family = "binomial", data)
summary(ft)

data$Age.cat = cut(data$Age, c(-1,9,36,100))
data$Parch.0 = data$Parch == 0
data$Embarked.S = data$Embarked == 'S'

train$Age.cat = cut(train$Age, c(-1,9,36,100))
train$Parch.0 = train$Parch == 0
train$Embarked.S = train$Embarked == 'S'

test$Age.cat = cut(test$Age, c(-1,9,36,100))
test$Parch.0 = test$Parch == 0
test$Embarked.S = test$Embarked == 'S'



### Model not using Age

ft = glm(Survived ~ Pclass + Sex + SibSp + Parch.0 + Fare + Embarked.S, family = "binomial", data)
summary(ft)

ft = glm(Survived ~ Pclass + Sex + SibSp + Parch.0 + Embarked.S, family = "binomial", data)
summary(ft)

ft = glm(Survived ~ Pclass + Sex + SibSp + Embarked.S, family = "binomial", data)
summary(ft)

ft = glm(Survived ~ Pclass + Sex + SibSp, family = "binomial", data)
summary(ft)




