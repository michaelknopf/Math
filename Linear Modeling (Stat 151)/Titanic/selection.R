var.sets = sapply(1:7, function(n) combn(allvars[-1],n))
sum(sapply(var.sets, ncol))
# 127 possible models

mods = list()

for (n in 1:7) {
  for (set in 1:choose(7,n)) {
    mods[[length(mods) + 1]] = glm(Survived ~ ., family = "binomial", data = data[,c('Survived', var.sets[[n]][,set])])
  }
}

aic = sapply(mods, AIC)
fts = mods[which(aic < 655)]
dev = sapply(fts, deviance)

for (ft in fts) {
  print(summary(ft))
}
