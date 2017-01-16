library(mlbench)
require(ggplot2)
require(plyr)
require(reshape2)
require(grid)
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

# Identify highly correlated variables
cormat = cor(bostmat)
which(abs(cormat) > .75 & cormat < 1, arr.ind = TRUE)

# If lon and lat are modified to measure proximity from Boston's center,
# is a better model produced?
bost$londist = abs(bost$lon - (-71.0589))
bost$latdist = abs(bost$lat - 42.3601)

# Initialize model
fullmodel = lm(medv ~ londist + latdist + crim + zn + indus + chas + nox +
               rm + age+ dis+ rad + tax + ptratio + b + lstat,
               data = bost)
sigma = summary(fullmodel)$sigma
n = nrow(bost)
p = length(fullmodel$coefficients) - 1
summary(fullmodel)

# Look for skewed data and non-linear relationships
for (var in names(bost)[-c(1,2)]) {
  plot(bost[,var], bost[,'medv'], pch = 19, cex = .5, xlab = var)
  abline(lm(bost$medv ~ bost[,var]))
}



# Repair nonlinear relationship?
par(mfrow = c(1,2))
qplot(x=crim, y=medv, data = bost, geom = c("point", "smooth"), method = "lm", formula = y ~ x, ylim = c(0,50))
qplot(x=log(crim), y=medv, data = bost, geom = c("point", "smooth"), method = "lm", formula = y ~ x)
par(mfrow = c(1,1))

# Transform crim
bost$logcrim = log(bost$crim)

# Check new model
fullmodel2 = lm(medv ~ londist + latdist + logcrim + zn + indus + chas + nox
               + rm + age+ dis+ rad + tax + ptratio + b + lstat,
               data = bost)
summary(fullmodel2)

#Remove logcrim, not going to use
rm(fullmodel2)
bost = bost[,-ncol(bost)]





# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

