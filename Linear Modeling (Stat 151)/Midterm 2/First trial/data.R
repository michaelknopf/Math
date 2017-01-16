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
bost$londist = abs(bost$lon - (-71.0589))
bost$latdist = abs(bost$lat - 42.3601)
bost$lon = bost$lon - (-71.0589)
bost$lat = bost$lat - 42.3601
bost$dist = sqrt(bost$londist^2 + bost$latdist^2)

# Initialize model
fullmodel = lm(medv ~ londist + latdist + crim + zn + indus + chas + nox +
                 rm + age+ dis+ rad + tax + ptratio + b + lstat,
               data = bost)
sigma = summary(fullmodel)$sigma
n = nrow(bost)
p = length(fullmodel$coefficients) - 1
summary(fullmodel)


# Look for skewed data and non-linear relationships
for (var in names(bost)[-(1:6)]) {
  print(qplot(x=bost[,var], y=medv, data = bost, geom = c("point", "smooth"), method = "lm", xlab = var))
}

fullmodel2 = lm(medv ~ londist + latdist + crim + zn + indus + chas + nox +
                 rm + age+ dis+ rad + tax + ptratio + b + lstat + (crim<1),
               data = bost)

xyplot(medv ~ crim | (crim > 1), data = bost, scales = list(relation = 'free', xlim = list(c(0,1),c(0,100))))

mod = lm(medv ~ (crim<1), data = bost)

crim.splits = sapply(1:200, function(i) {
  split = i/100
  model = lm(medv ~ (crim < split), data = bost)
  return(model$coefficients[1] - model$coefficients[2])
})
qplot(x = (1:200)/100, y =  crim.splits, geom = "point", xlab = 'split value', 'medv') +
  geom_vline(xintercept = .55, col = 'red') + annotate("text", x = .65, y = 16, label = "x = .55")





mod1 = lm(medv ~ (rad > 20), data = bost)
mod2 = lm(medv ~ rad, data = bost)

mod1 = lm(medv ~ (tax > 600) + ptratio + (tax > 600)*ptratio, data = bost)

mod1 = lm(medv ~ (zn == 0) + ptratio + (zn == 0)*ptratio, bost)
mod2 = lm(medv ~ (tax>500) + ptratio + (tax>500)*ptratio, bost)






