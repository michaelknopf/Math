library(mlbench)
library(ggplot2)
library(plyr)
library(reshape2)
library(grid)
library(lattice)
library(GGally)
data(BostonHousing2)

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


# Look for skewed data and non-linear relationships by fitting medv to each individual explanatory variable
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


# Are tax and rad categorical?  Perform simple linear regression on subsets where
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



# Is dis categorical?
qplot(x=dis, y=medv, data = bost, geom = c("point", "smooth"), method = "lm", xlab = 'dis')

# Test different split values.  Obtain total RSS and difference of slopes for each split.
diff = sapply(1:250, function(i) {
  split = 2.5 + i/100
  ft.dislow = lm(medv ~ dis, data = bost[which(bost$dis < split),])
  ft.dishigh = lm(medv ~ dis, data = bost[which(bost$dis > split),])
  return(ft.dislow$coefficients[2] - ft.dishigh$coefficients[2])
})

rss = sapply(1:250, function(i) {
  split = 2.5 + i/100
  ft.dislow = lm(medv ~ dis, data = bost[which(bost$dis < split),])
  ft.dishigh = lm(medv ~ dis, data = bost[which(bost$dis > split),])
  return(deviance(ft.dislow) + deviance(ft.dishigh))
})

# Set split value and plot resulting fits
splitvalue = min(2.5 + which(rss == min(rss))/100)
plots = list()
plots[[1]] = qplot(2.5 + (1:250)/100, diff, xlab = "dis", ylab = "Difference in slope") + geom_vline(x = 3.5, col = 'blue')
plots[[2]] = qplot(2.5 + (1:250)/100, rss, xlab = 'split', ylab = "Total RSS") + geom_vline(x = 3.5, col = 'blue')
plots[[3]] = qplot(x=dis, y=medv, data = bost[which(bost$dis < splitvalue),], geom = c("point", "smooth"), method = "lm", xlab = 'dis')
plots[[4]] = qplot(x=dis, y=medv, data = bost[which(bost$dis >= splitvalue),], geom = c("point", "smooth"), method = "lm", xlab = 'dis')

multiplot(plotlist = plots, cols = 4)

# Check summaries of split models
ft.dislow = lm(medv ~ dis, data = bost[which(bost$dis < splitvalue),])
ft.dishigh = lm(medv ~ dis, data = bost[which(bost$dis >= splitvalue),])

# Create interaction term and check summary in new full model
bost$dislow = bost$dis * (bost$dis < splitvalue)

summary(lm(medv ~ latdist + londist + crim + zn + indus + chas + nox +
          rm + age + dis + dislow + rad + tax + ptratio + b + lstat,
        data = bost))
summary(lm(medv ~ dis + dislow, data = bost))

# Create pairwise plots of explanatory variables
ggpairs(bost[,vars], lower=list(continuous="smooth"))

