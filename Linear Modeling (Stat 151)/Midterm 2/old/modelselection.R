allvars = c('medv', 'lon', 'lat', 'crim', 'zn', 'indus', 'chas', 'nox',
            'rm', 'age', 'dis', 'rad', 'tax', 'ptratio', 'b', 'lstat')

getMallows = function(model) {
  p = length(model$coefficients)
  (deviance(model) / sigma^2) - (n-2-2*p)
}

getAIC = function(model) {
  n*log(deviance(model) / n) + 2*(1+length(model$coefficients))
}

getBIC = function(model) {
  n*log(deviance(model) / n) + log(n)*(1+length(model$coefficients))
}

getCVscore = function(model) {
  sum(residuals(model)^2/ (1-hatvalues(model))^2)
}

backward = function(vars = allvars, critical = .15) {
  model = lm(medv ~ ., bost[,vars])
  pvalues = summary(model)$coefficients[-1,4]
  var = which(pvalues == max(pvalues))
  if(pvalues[var] > critical) {
    vars = vars[-(var+1)]
    return(backward(vars, critical))
  } else {
    return(model)
  }
}


forward = function(vars = 1, critical = .15) {
  if (length(vars) == length(allvars))
  {
    return(lm(medv ~ ., bost[,allvars]))
  }
  
  pvalues = sapply(1:length(allvars), function(var) {
    if(var %in% vars) {
      return(1)
    }
    model = lm(medv ~ ., bost[,allvars[c(vars,var)]])
    p = rev(summary(model)$coefficients[,4])[1]
    return(p[length(p)])
  })
  var = which(pvalues == min(pvalues))
  if(pvalues[var] < critical) {
    vars = c(vars,var)
    return(forward(vars = vars, critical = critical))
  } else {
    return(lm(medv ~ ., bost[, allvars[vars]]))
  }
}

adjRsqr = function(vars = allvars) {
  ft = lm(medv ~., bost[,vars])
  currentAdjR = summary(ft)$adj.r.squared
  adjR = sapply(vars[-1], function(var) {
    model = lm(medv ~., bost[,vars[vars != var]])
    adjR = summary(model)$adj.r.squared
    return(adjR)
  })
  var = which(adjR == max(adjR))
  if(adjR[var] > currentAdjR)
  {
    return(adjRsqr(vars[-(var+1)]))
  } else {
    return(ft)
  }
}

AIC = function() {
  model = lm(medv ~ ., bost[,allvars])
  return(step(model, direction = "both", trace = 0))
}

BIC = function() {
  model = lm(medv ~ ., bost[,allvars])
  return(step(model, direction="both", k = log(nrow(bost)), trace = 0))
}

Mallow = function(vars = allvars) {
  ft = lm(medv ~ ., bost[,vars])
  currentMallows = getMallows(ft)

  p = p-1
  
  mallows = sapply(vars[-1], function(var) {
    model = lm(medv ~., bost[,vars[vars != var]])
    return(getMallows(model))
  })
  var = which(mallows == min(mallows))
  if(mallows[var] < currentMallows)
  {
    return(Mallow(vars[-(var+1)]))
  } else {
    return(ft)
  }
}

models = list()
models[[1]] = backward()
models[[2]] = forward()
models[[3]] = adjRsqr()
models[[4]] = AIC()
models[[5]] = BIC()
models[[6]] = Mallow()

for (ft in models) print(summary(ft))

# Alphabetize coefficients to compare models
coefs = sapply(models, function(ft) ft$coefficients)
coefs = sapply(coefs, function(x) x = x[order(names(x))])
print(coefs)

# Only two unique models were produced
ft = list()
ft[[1]] = models[[1]]  # All but one of the methods produced this model
ft[[2]] = models[[3]]  # This model also includes lat and lon

# Garbage collection
rm(models)

# Check correlation of variables not in both models
cor(bost$lon, bost$medv)
cor(bost$lat, bost$medv)

# If lon and lat are modified to measure proximity from Boston's center,
# is a better model produced?
bost[,'londist'] = abs(bost[,'lon'] - (-71.0589))
bost[,'latdist'] = abs(bost[,'lat'] - (42.3601))

ft[[3]] = lm(medv ~ londist + latdist + crim + zn + chas + nox
            + rm + dis+ rad + tax + ptratio + b + lstat,
            data = bost)


# Create matrix of criteria
crit = sapply(ft, function(model) c(getAIC(model), getBIC(model), getMallows(model), getCVscore(model)))
criteria = data.frame(crit = c("AIC", "BIC", "Mallows CP", "CV"), diff = crit[,2] - crit[,1])

# Barplot comparison of criteria
crit.barplot = qplot(x = crit, y = diff, fill = 'red', main = "Difference in Criteria",
                     xlab = "Criteria", ylab = "Difference (ft2 - ft1)",
                     data = criteria, geom = 'bar', stat = 'identity',
                     position = 'dodge') + theme(legend.position="none")


