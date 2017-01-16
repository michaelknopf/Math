library(rpart)

data2 = train[!is.na(train$Age),]

tit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,  method="class", data=data2)
plot(tit)
text(tit)

preds = as.numeric(predict(tit,data2)[,2] > .5)
obs = data2[,'Survived']
table(obs,preds)

print(tit)

plot(prune.rpart(tit, cp = .02))
text(prune.rpart(tit, cp = .02))

tit2 <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,  method="class", data=train)
plot(tit2)
text(tit2)

### Predictions ###
test$Survived.Rpart[!is.na(test$Age)] = as.numeric(predict(tit, test[!is.na(test$Age),])[,2] > .5)
test$Survived.Rpart[is.na(test$Age)] = as.numeric(predict(tit2, test[is.na(test$Age),])[,2] > .5)








