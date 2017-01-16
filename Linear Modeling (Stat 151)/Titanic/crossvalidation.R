data = data[!is.na(data$Age),]  #Remove unused observations
n = nrow(data)
### n=714 is divisible by 7
k = 714/7

# Split data into test sets of size 102
testsets = list()
for (i in 0:6) {
  testsets[[i+1]] = 1:k + k*i
}

# Variables used in current model of choice
vars = c('Survived', 'Pclass', 'Sex', 'Age', 'SibSp')

# Cross validation function.  Returns the sum of squared errors for each test set.
cross = function(vars, binary = FALSE) {
  models = list()
  
  for (i in 1:7) {
    models[[i]] = glm(Survived ~ ., family = "binomial", data = data[-testsets[[i]],vars])
  }
  
  if (binary == FALSE) {
    dev = sapply(1:7, function(i) {
      sum((predict.glm(models[[i]], data[testsets[[i]],], type = "response") - data$Survived[testsets[[i]]])^2)
    })
    
  } else {
    
    dev = sapply(1:7, function(i) {
      pred = predict.glm(models[[i]], data[testsets[[i]],], type = "response")
      thresholds = (25:75)/100
      a = findthresh.ad(models[[i]], thresholds)
      thresh = (mean(which(a == max(a))) + 24) /100
      return(sum(((pred >= thresh) - data$Survived[testsets[[i]]])^2))
    })
    
  }
  
  return(dev)
}

# Check total sum of squared error for chosen model and full model
dev.ft = cross(vars)
sum(dev.ft)
var(dev.ft)

dev.full = cross(allvars)
sum(dev.full)
var(dev.full)

# Create list of cross-validation scores for every possible model
dev = list()
for (n in 1:7) {
  for (set in 1:choose(7,n)) {
    dev[[length(dev) + 1]] = cross(c('Survived', var.sets[[n]][,set]))
  }
}

# List of variables used in each model (to see which score corresponds to which model)
varlist = list()
for (n in 1:7) {
  for (set in 1:choose(7,n)) {
    varlist[[length(varlist) + 1]] = var.sets[[n]][,set]
  }
}

# Get total rss of each model
rss = sapply(dev,sum)
variances = sapply(dev,var)


## Identify chosen model's index
which(rss == sum(dev.ft))

# Plot scores against model size
plots = list()
plots[[1]] = qplot(jitter(sapply(varlist,length)), rss, xlab = "Model Size", ylab = "Score (RSS)") +
  geom_hline(y = sum(dev.ft), col = "red")
plots[[2]] = qplot(jitter(sapply(varlist,length)), variances, xlab = "Model Size", ylab = "Score (variance)") +
  geom_hline(y = var(dev.ft), col = "blue")




#############  BINARY CROSS-VALIDATION  #############

# Check total sum of squared error for chosen model and full model
dev.ft.bin = cross(vars, binary = TRUE)
sum(dev.ft.bin)
var(dev.ft.bin)

dev.full.bin = cross(allvars, binary = TRUE)
sum(dev.full.bin)
var(dev.full.bin)

# Create list of cross-validation scores for every possible model
dev.bin = list()
for (n in 1:7) {
  for (set in 1:choose(7,n)) {
    dev[[length(dev) + 1]] = cross(c('Survived', var.sets[[n]][,set]), binary = TRUE)
  }
}

# Get total rss of each model
rss.bin = sapply(dev.bin,sum)
variances.bin = sapply(dev.bin,var)


## Identify chosen model's index
which(rss.bin == sum(dev.ft.bin))

# Plot scores against model size
plots[[3]] = qplot(jitter(sapply(varlist,length)), rss.bin,
      xlab = "Model Size", ylab = "Score (RSS.bin)") +
  geom_hline(y = sum(dev.ft.bin), col = "red")
plots[[4]] = qplot(jitter(sapply(varlist,length)), variances.bin,
      xlab = "Model Size", ylab = "Score (variance.bin)") +
  geom_hline(y = var(dev.ft.bin), col = "blue")

multiplot(plotlist = plots, cols = 2)




