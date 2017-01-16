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

allvars = c('medv', 'londist', 'crim', 'zn', 'indus', 'chas', 'nox',
            'rm', 'age', 'dis', 'rad', 'tax', 'ptratio', 'b', 'lstat')

pvalues = sort(sapply(allvars[-1], function(var) {
  coefficients(summary(lm(medv ~ bost[,var], data = bost)))[2,4]
}))

dat = as.matrix(bost[,allvars[-1]])
cormat = cor(dat)
diag(cormat) = 0

which(cormat > .85, arr.ind = TRUE)
i = which(rownames(cormat) == 'rad')
cormat = cormat[-i,-i]
which(cormat > .8, arr.ind = TRUE)
i = which(rownames(cormat) == 'dis')
cormat = cormat[-i,-i]
which(cormat > .75, arr.ind = TRUE)
i = which(rownames(cormat) == 'nox')
cormat = cormat[-i,-i]
which(cormat > .7, arr.ind = TRUE)
i = which(rownames(cormat) == 'tax')
cormat = cormat[-i,-i]
which(cormat > .6, arr.ind = TRUE)
i = which(rownames(cormat) == 'age')
cormat = cormat[-i,-i]
i = which(rownames(cormat) == 'indus')
cormat = cormat[-i,-i]
which(cormat > .5, arr.ind = TRUE)
i = which(rownames(cormat) == 'londist')
cormat = cormat[-i,-i]
which(cormat > .35, arr.ind = TRUE)
i = which(rownames(cormat) == 'ptratio')
cormat = cormat[-i,-i]
i = which(rownames(cormat) == 'crim')
cormat = cormat[-i,-i]
which(cormat > .3, arr.ind = TRUE)


allvars = c('londist', 'crim', 'zn', 'indus', 'chas', 'nox',
            'rm', 'age', 'dis', 'rad', 'tax', 'ptratio')
allvars = c('crim', 'indus', 'chas', 'nox',
            'rm', 'dis', 'rad', 'tax', 'ptratio')

pvalues = sort(sapply(allvars, function(var) {
  coefficients(summary(lm(medv ~ bost[,var], data = bost)))[2,4]
}))

dat = as.matrix(bost[,allvars])
cormat = cor(dat)
diag(cormat) = 0

a = apply(cormat,2,function(x) sum(x^2))
rev(sort(a))
i = which(rownames(cormat) == 'indus')
cormat = cormat[-i,-i]

model = lm(medv ~ rm + ptratio + crim + londist, data = bost)
model = lm(medv ~ rm + ptratio + crim + zn, data = bost)
model = lm(medv ~ rm + ptratio + crim + chas + indus, data = bost)
model = lm(medv ~ rm + ptratio + crim + chas, data = bost)

cormat2 = cormat[c('rm','ptratio','crim','zn'),c('rm','ptratio','crim','zn')]
cormat1 = cormat[c('rm','ptratio','crim','londist'),c('rm','ptratio','crim','londist')]
