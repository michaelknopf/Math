allvars = c('medv', 'latdist', 'londist', 'crim', 'zn', 'indus', 'chas', 'nox',
            'rm', 'age', 'dis', 'rad', 'tax', 'ptratio', 'b', 'lstat')

# Initialize full model
fullmodel = lm(medv ~ latdist + londist + crim + zn + indus + chas + nox +
                 rm + age + dis + rad + tax + ptratio + b + lstat,
               data = bost)

# Compute parameters used in variable selection
sigma = summary(fullmodel)$sigma
n = nrow(bost)
p = length(fullmodel$coefficients) - 1
summary(fullmodel)

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
  sum(model$residuals^2/ (1-hatvalues(model))^2)
}

getGCVscore = function(model) {
  p = length(model$coefficients)
  deviance(model)*(1 + 2*(1+p)/n)
}

# Backward selection based on p-value
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

# Forward selection based on p-value
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

# Backward selection based on adjusted R squared
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

# AIC selection using step function
AIC = function() {
  model = lm(medv ~ ., bost[,allvars])
  return(step(model, direction = "both", trace = 0))
}

# BIC selection using step function
BIC = function() {
  model = lm(medv ~ ., bost[,allvars])
  return(step(model, direction="both", k = log(nrow(bost)), trace = 0))
}

# Backward selection using Mallow's Cp
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

# Get model selected by each method
models = list()
models[[1]] = backward()
models[[2]] = forward()
models[[3]] = adjRsqr()
models[[4]] = AIC()
models[[5]] = BIC()
models[[6]] = Mallow()

# Inspect summaries
for (ft in models) print(summary(ft))

# Alphabetize coefficients to compare models
coefs = lapply(models, function(ft) names(ft$coefficients)[-1])
coefs = sapply(coefs, function(x) x = x[order(x)])
print(coefs)

# Only two unique models were produced
ft = list()
ft[[1]] = models[[3]]  # This model also includes latdist
ft[[2]] = models[[1]]  # All but one of the methods produced this model; no latdist

# Create data frame of criteria
crit = sapply(ft, function(model) c(getAIC(model), getBIC(model),
                                    getMallows(model), getCVscore(model),
                                    summary(model)$adj.r.squared,
                                    getGCVscore(model)))
criteria = data.frame(model = c("latdist", "no latdist"),
                      AIC = crit[1,], BIC = crit[2,], Mallow = crit[3,],
                      CV = crit[4,], adj.r.squared = crit[5,],
                      GCV = crit[6,])

# Force R to respect order of factor levels (for plotting purposes)
criteria$model = factor(criteria$model, levels = c("latdist", "no latdist"))

# Barplot comparison of criteria
crit.barplot.AIC = qplot(x = model, y = AIC, fill = 'red',
                         main = "AIC", xlab = '', ylab = "",
                         data = criteria, geom = 'bar', stat = 'identity',
                         position = 'dodge') + theme(legend.position="none") +
  geom_bar(stat="identity", colour="white") + coord_cartesian(ylim=c(1560,1575))

crit.barplot.BIC = qplot(x = model, y = BIC, fill = 'blue',
                         main = "BIC", xlab = '', ylab = "",
                         data = criteria, geom = 'bar', stat = 'identity',
                         position = 'dodge') + theme(legend.position="none") +
  geom_bar(stat="identity", fill="#799AFF", colour="white") + coord_cartesian(ylim=c(1625,1640))

crit.barplot.Mallow = qplot(x = model, y = Mallow, fill = 'red',
                            main = "Mallow's CP", xlab = '', ylab = "",
                            data = criteria, geom = 'bar', stat = 'identity',
                            position = 'dodge') + theme(legend.position="none") +
  geom_bar(stat = "identity", fill="#FFB445", colour="white") + coord_cartesian(ylim=c(14,16))

crit.barplot.adjr = qplot(x = model, y = adj.r.squared, fill = 'red',
                          main = "Adjusted R-squared", xlab = '', ylab = "",
                          data = criteria, geom = 'bar', stat = 'identity',
                          position = 'dodge') + theme(legend.position="none") +
  geom_bar(stat="identity", fill = "#FF9494", colour="white") + coord_cartesian(ylim=c(.743,.744))

crit.barplot.CV = qplot(x = model, y = CV, fill = 'red',
                        main = "LOO Cross-Validation Score", xlab = '', ylab = "",
                        data = criteria, geom = 'bar', stat = 'identity',
                        position = 'dodge') + theme(legend.position="none") +
  geom_bar(stat = "identity", fill="#6ADA6A", colour="white") + coord_cartesian(ylim=c(11490,11510))

crit.barplot.GCV = qplot(x = model, y = GCV,
                         main = "General Cross-Validation Score", xlab = '', ylab = "",
                         data = criteria, geom = 'bar', stat = 'identity',
                         position = 'dodge') + theme(legend.position="none") +
  geom_bar(stat = "identity", fill="#5CBD7D", colour="white") + coord_cartesian(ylim=c(11285,11300))

# Make barplots
multiplot(crit.barplot.AIC, crit.barplot.BIC, crit.barplot.Mallow,
          crit.barplot.adjr, crit.barplot.CV, crit.barplot.GCV, cols = 3)
