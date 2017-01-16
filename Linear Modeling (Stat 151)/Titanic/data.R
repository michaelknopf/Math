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

ft = glm(Survived ~ ., family = "binomial", data = data)
summary(ft)

ft = glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare, family = "binomial", data = data)
summary(ft)

ft = glm(Survived ~ Pclass + Sex + Age + SibSp + Fare, family = "binomial", data = data)
summary(ft)

ft = glm(Survived ~ Pclass + Sex + Age + SibSp, family = "binomial", data = data)
summary(ft)

qplot(x=jitter(ft$y), y=ft$fitted.values, geom = c("point", "smooth"), method = "lm")

summary((ft$fitted.values)[ft$y == 0])
summary((ft$fitted.values)[ft$y == 1])

plots.simple = list()
plots.simple[[1]] = qplot(x=jitter(Pclass), y=jitter(Survived), data = data, geom = c("point", "smooth"), method = "lm", xlab = 'Pclass', ylab = "")
plots.simple[[2]] = qplot(x=jitter(as.numeric(Sex)%%2), y=jitter(Survived), data = data, geom = c("point", "smooth"), method = "lm", xlab = 'Sex', ylab = "")
plots.simple[[3]] = qplot(x=na.omit(data$Age), y=jitter(data$Survived[!is.na(data$Age)]), geom = c("point", "smooth"), method = "lm", xlab = 'Age', ylab = "")
plots.simple[[4]] = qplot(x=jitter(SibSp), y=jitter(Survived), data = data, geom = c("point", "smooth"), method = "lm", xlab = 'SibSp', ylab = "")
plots.simple[[5]] = qplot(x=jitter(Parch), y=jitter(Survived), data = data, geom = c("point", "smooth"), method = "lm", xlab = 'Parch', ylab = "")
plots.simple[[6]] = qplot(x=Fare, y=jitter(Survived), data = data, geom = c("point", "smooth"), method = "lm", xlab = 'Fare', ylab = "")

multiplot(plotlist = plots.simple, cols = 3)

ft = glm(Survived ~ Embarked, family = "binomial", data = data)
summary(ft)
# Embarked appears highly insignificant


# Should SibSp be categorical?
# We will use age, so remove observations where age is NA
data = data[!is.na(data$Age),]
sum(data$SibSp > 1)
plot(data$SibSp[data$SibSp > 1])


# Check the difference in means of survival between SibSp groups
data$SibSp1 = data$SibSp == 1
data$SibSp2 = data$SibSp == 2
data$SibSp3 = data$SibSp == 3
data$SibSp4 = data$SibSp == 4

ft = lm(Survived ~ SibSp1 + SibSp2 + SibSp3 + SibSp4, data = data)
summary(ft)

mean(data$Survived)
mean(data$SibSp == 0)
mean(data$SibSp1)
mean(data$SibSp2)
mean(data$SibSp3)
mean(data$SibSp4)

data$SibSp.new = (data$SibSp == 1 | data$SibSp == 2)

ft1 = glm(Survived ~ Pclass + Sex + Age + SibSp.new, family = "binomial", data = data)
summary(ft)
summary(ft1)

data$SibSp.new = (data$SibSp == 0)
ft1 = glm(Survived ~ Pclass + Sex + Age + SibSp.new, family = "binomial", data = data)
summary(ft)
summary(ft1)

data$SibSp.new = data$SibSp
ft1 = glm(Survived ~ Pclass + Sex + Age + SibSp.new, family = "binomial", data = data)
summary(ft)
summary(ft1)
