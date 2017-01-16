models = list()
models[[1]] = lm(BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + HIP + THIGH + FOREARM + WRIST, data)
models[[2]] = lm(BODYFAT ~ WEIGHT + ABDOMEN + FOREARM + WRIST, data)

GCV = function(model) {
  deviance(model) + 2*(length(model$coefficients))*deviance(model)/252
}

variables = sapply(models, function(x) names(x$coefficients[-1]))
test = list(1:126,127:252)
errs = c(0,0)

prederror = function(trainSet, whichmodel) {
  testSet = (1:252)[-trainSet]
  vars = variables[[whichmodel]]
  model = lm(BODYFAT ~ ., data[trainSet, c("BODYFAT",vars)])
  beta = model$coefficients
  err = sapply(testSet, function(obs){
    x = c(1,as.numeric(data[obs,vars]))
    pred = sum(x*beta)
    actual = data[obs,"BODYFAT"]
    return(pred - actual)
    })
  return(sum(err^2))
}

errs[1] = prederror(test[[1]],1) + prederror(test[[2]],1)
errs[2] = prederror(test[[1]],2) + prederror(test[[2]],2)
#  GCV: 4092.841 4152.815
# errs: 4219.102 4324.321