library(mlbench)
require(ggplot2)
require(plyr)
require(reshape2)
require(grid)
require(lattice)
data(BostonHousing2)
help(BostonHousing2)

# Initialize dataframe and matrix
bost = BostonHousing2
rm(BostonHousing2)
for(i in 2:ncol(bost)) {
  bost[,i] = as.numeric(bost[,i])
}
bost$chas = as.numeric(bost$chas)
bostmat = as.matrix(bost[,-(1:2)])


# How many tracts differ in these categories?  Just 8.
sum(bost$medv - bost$cmedv != 0)
bost$town[bost$medv - bost$cmedv != 0]

# Check for highly correlated variables
cormat = cor(bostmat)
which(abs(cormat) > .75 & cormat < 1, arr.ind = TRUE)

# If lon and lat are modified to measure proximity from Boston's center,
# is a better model produced?
bost$lon = bost$lon - (-71.0589)
bost$lat = bost$lat - 42.3601
bost$londist = abs(bost$lon)
bost$latdist = abs(bost$lat)


# Look for skewed data and non-linear relationships
plots.simple = list()
for (i in 1:length(names(bost)[-(1:6)])) {
  var = (names(bost)[-(1:6)])[i]
  plots.simple[[i]] = qplot(x=bost[,var], y=medv, data = bost, geom = c("point", "smooth"), method = "lm", xlab = var)
}
plots.simple = list()
plots.simple[[1]] = qplot(x=crim, y=medv, data = bost, geom = c("point", "smooth"), method = "lm", xlab = 'crim')
plots.simple[[2]] = qplot(x=zn, y=medv, data = bost, geom = c("point", "smooth"), method = "lm", xlab = 'zn')
plots.simple[[3]] = qplot(x=indus, y=medv, data = bost, geom = c("point", "smooth"), method = "lm", xlab = 'indus')
plots.simple[[4]] = qplot(x=chas, y=medv, data = bost, geom = c("point", "smooth"), method = "lm", xlab = 'chas')
plots.simple[[5]] = qplot(x=nox, y=medv, data = bost, geom = c("point", "smooth"), method = "lm", xlab = 'nox')
plots.simple[[6]] = qplot(x=rm, y=medv, data = bost, geom = c("point", "smooth"), method = "lm", xlab = 'rm')
plots.simple[[7]] = qplot(x=age, y=medv, data = bost, geom = c("point", "smooth"), method = "lm", xlab = 'age')
plots.simple[[8]] = qplot(x=dis, y=medv, data = bost, geom = c("point", "smooth"), method = "lm", xlab = 'dis')
plots.simple[[9]] = qplot(x=rad, y=medv, data = bost, geom = c("point", "smooth"), method = "lm", xlab = 'rad')
plots.simple[[10]] = qplot(x=tax, y=medv, data = bost, geom = c("point", "smooth"), method = "lm", xlab = 'tax')
plots.simple[[11]] = qplot(x=ptratio, y=medv, data = bost, geom = c("point", "smooth"), method = "lm", xlab = 'ptratio')
plots.simple[[12]] = qplot(x=b, y=medv, data = bost, geom = c("point", "smooth"), method = "lm", xlab = 'b')
plots.simple[[13]] = qplot(x=lstat, y=medv, data = bost, geom = c("point", "smooth"), method = "lm", xlab = 'lstat')
plots.simple[[14]] = qplot(x=londist, y=medv, data = bost, geom = c("point", "smooth"), method = "lm", xlab = 'londist')
plots.simple[[15]] = qplot(x=latdist, y=medv, data = bost, geom = c("point", "smooth"), method = "lm", xlab = 'latdist')

multiplot(plotlist = plots.simple, cols = 5)


# tax and rad categorical?  Perform simple linear regression on subsets where
# tax is above/below cutoff line.
bost.taxlow = bost[which(bost$tax < 500),c('medv','tax')]
taxlow = qplot(x=tax, y=medv, data = bost.taxlow, geom = c("point", "smooth"), method = "lm", xlab = 'tax < 500')
bost.taxhigh = bost[which(bost$tax >= 500),c('medv','tax')]
taxhigh = qplot(x=tax, y=medv, data = bost.taxhigh, geom = c("point", "smooth"), method = "lm", xlab = 'tax > 500')

bost.radlow = bost[which(bost$rad < 15),c('medv','rad')]
radlow = qplot(x=rad, y=medv, data = bost.radlow, geom = c("point", "smooth"), method = "lm", xlab = 'rad < 15')
bost.radhigh = bost[which(bost$rad >= 15),c('medv','rad')]
qplot(x=rad, y=medv, data = bost.radhigh, geom = c("point", "smooth"), method = "lm", xlab = 'rad > 15')
mean(bost.radhigh$medv)

multiplot(plotlist = list(taxlow, taxhigh, radlow), cols = 3)

bost$rad.high = bost$rad > 15

ft1 = lm(medv ~ latdist + londist + crim + zn + indus + chas + nox +
           rm + age+ dis+ rad + tax + ptratio + b + lstat,
         data = bost)

ft2 = lm(medv ~ latdist + londist + crim + zn + indus + chas + nox +
           rm + age+ dis+ rad.high + tax + ptratio + b + lstat,
         data = bost)
# 
# xyplot(medv ~ crim | (crim > 1), data = bost, scales = list(relation = 'free', xlim = list(c(0,1),c(0,100))))
# 
# 
# 
# 
# 
# 
# 
# # Initialize model
# fullmodel = lm(medv ~ londist + latdist + crim + zn + indus + chas + nox +
#                  rm + age+ dis+ rad + tax + ptratio + b + lstat,
#                data = bost)
# sigma = summary(fullmodel)$sigma
# n = nrow(bost)
# p = length(fullmodel$coefficients) - 1
# summary(fullmodel)
# 
# 
# 
