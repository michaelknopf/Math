data = rbind(train[,-2],test)[!is.na(data$Age),]

ft.age = lm(Age ~ ., data[,allvars[-1]])
ft.age = lm(Age ~ Pclass + Sex + SibSp + Parch + Fare, data[,allvars[-1]])
ft.age = lm(Age ~ Pclass + Sex + SibSp + Parch, data[,allvars[-1]])
summary(ft.age)

data$Parch.cat = cut(data$Parch, c(-1,0,2,6))
data$Parch.0 = data$Parch == 0

ft1 = lm(Age ~ Parch.cat, data)
summary(ft1)
ft2 = lm(Age ~ Parch.0, data)
summary(ft2)

ft.age = lm(Age ~ Pclass + Sex + SibSp + Parch.0 + Fare, data)
summary(ft.age)

plot(data$Age, data$Fare)

ft.age = lm(Age ~ Pclass + Sex + SibSp + Parch.0, data)
summary(ft.age)

ft1 = lm(Age ~ (SibSp == 0), data)
summary(ft1)
ft2 = lm(Age ~ SibSp, data)
summary(ft2)






### Precit cut(Age, c(-1,9,36,100)) ###
ft.age9 = glm(cut(Age, c(-1,9,36,100)) ~ Pclass + Sex + SibSp + Parch.0, family = 'binomial', data)
summary(ft.age9)

ft.age9 = glm(cut(Age, c(-1,9,36,100)) ~ Pclass + SibSp + Parch.0, family = 'binomial', data)
summary(ft.age9)

ft.age9 = glm(cut(Age, c(-1,9,36,100)) ~ Pclass + (SibSp>1) + Parch.0, family = 'binomial', data)
summary(ft.age9)



# Predict age
train$Parch.0 = train$Parch == 0
test$Parch.0 = test$Parch == 0
train$SibSp.1 = train$SibSp > 1
test$SibSp.1 = test$SibSp > 1
train$Age.cat = cut(train$Age, c(-1,9,36,100))
test$Age.cat = cut(test$Age, c(-1,9,36,100))
ft.age = lm(Age ~ Pclass + Sex + SibSp + Parch.0, data)

ind = which(is.na(train$Age))
train$Age[ind] = predict(ft.age, train[ind,])

ind = which(is.na(test$Age))
test$Age[ind] = predict(ft.age, test[ind,])
