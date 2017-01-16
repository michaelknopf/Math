setwd("~/Desktop/Stat 151")
body = read.csv("BodyFat.csv")
allvars = c('BODYFAT', 'AGE', 'WEIGHT', 'HEIGHT', 'ADIPOSITY', 'NECK', 'CHEST', 'ABDOMEN',
               'HIP', 'THIGH', 'KNEE', 'ANKLE', 'BICEPS', 'FOREARM', 'WRIST')
data = body[,allvars]

backward = function(vars = allvars, critical = .2) {
  model = lm(BODYFAT ~ ., data[,vars])
  pvalues = summary(model)$coefficients[-1,4]
  var = which(pvalues == max(pvalues))
  if(pvalues[var] > critical) {
    #print(var)
    vars = vars[-(var+1)]
    return(backward(vars, critical))
  } else {
    return(model)
  }
}

forward = function(vars = 1, critical = .2) {
  if (length(vars) == length(allvars))
  {
    return(lm(BODYFAT ~ ., data[,allvars]))
  }
  
  pvalues = sapply(1:length(allvars), function(var) {
    if(var %in% vars) {
      return(1)
    }
    model = lm(BODYFAT ~ ., data[,allvars[c(vars,var)]])
    p = rev(summary(model)$coefficients[,4])[1]
    return(p[length(p)])
  })
  var = which(pvalues == min(pvalues))
  if(pvalues[var] < critical) {
    #print(var)
    vars = c(vars,var)
    return(forward(vars = vars, critical = critical))
  } else {
    return(lm(BODYFAT ~ ., data[, allvars[vars]]))
  }
}

adjRsqr = function(vars = allvars) {
  ft = lm(BODYFAT ~., data[,vars])
  currentAdjR = summary(ft)$adj.r.squared
  adjR = sapply(vars[-1], function(var) {
    model = lm(BODYFAT ~., data[,vars[vars != var]])
    adjR = summary(model)$adj.r.squared
    return(adjR)
  })
  var = which(adjR == max(adjR))
  if(adjR[var] > currentAdjR)
  {
    #print(var)
    return(adjRsqr(vars[-(var+1)]))
  } else {
    return(ft)
  }
}

AIC = function() {
  model = lm(BODYFAT ~ ., data[,allvars])
  return(step(model, direction = "both", trace = 0))
}

BIC = function() {
  model = lm(BODYFAT ~ ., data[,allvars])
  return(step(model, direction="both", k = log(nrow(data)), trace = 0))
}

Mallow = function(vars = allvars) {
  n = nrow(data)
  p = length(vars) - 1
  ft = lm(BODYFAT ~ ., data[,vars])
  sigma = summary(ft)$sigma
  currentMallows = p+1
  
  p = p-1
  
  mallows = sapply(vars[-1], function(var) {
    model = lm(BODYFAT ~., data[,vars[vars != var]])
    RSS = deviance(model)
    mallow = (RSS / sigma^2) - (n-2-2*p)
    return(mallow)
  })
  var = which(mallows == min(mallows))
  if(mallows[var] < currentMallows)
  {
    #print(var)
    return(Mallow(vars[-(var+1)]))
  } else {
    return(ft)
  }
}






