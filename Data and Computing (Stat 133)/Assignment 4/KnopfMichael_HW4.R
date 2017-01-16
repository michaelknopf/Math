################################################################################
# YOUR NAME: Michael Knopf

################################################################################
# PROBLEM 4E
# Your explanation goes here. Keep it as a comment!
# The plot looks roughly like a straight line, which indicates that the times of
# change do are approximately uniform on the interval (0,720).
#
################################################################################
# PROBLEM 5C
# Your explanation goes here. Keep it as a comment!
# This plot seems to follow a line even better than the plot from question 4.
# This was expected, since the lumpedChanges vector contains a larger sample
# than those from a single site, which should cause its quantiles to fall
# even closer to those of a uniform scatter.
#
################################################################################
# PROBLEM 6D
# Your explanation goes here. Keep it as a comment!
# These plots reveal that the quantiles likely follow a uniform distribution.
# The left plot is roughly linear, showing that the uniform simulation of the
# site changes looks the same, when its quantiles are compared to the uniform
# distribution's, as did the actual site changes.  The right plot shows that
# the uniform quantiles are not linearly related to the normal quantiles.
################################################################################
# PROBLEM 1
# load data
load(url("http://www.stat.berkeley.edu/users/nolan/data/Cache500.rda"))
# check Cache500 is a list
class(Cache500) == "list"
# check Cache500 has 500 elements
length(Cache500) == 500
# define Cache500_class
Cache500_class = sapply(Cache500, class)

# check all integers
all(Cache500_class == "integer")
################################################################################
# PROBLEM 2
# create vector with number of changes
numChanges = sapply(Cache500, length)

# create vector with first change
firstChange = sapply(Cache500, function(x) x[1])

# create vector with last change
lastChange = as.numeric(sapply(Cache500, function(x) x[length(x)]))

################################################################################
# PROBLEM 3
# sites with between 200 and 250 changes, and last change is after 700th visit
whichOK = which(200 < numChanges & numChanges < 250 & lastChange > 700)

################################################################################
# PROBLEM 4
# which site in the whichOK set are you focusing on? give the index in Cache500
siteIndex = 41

# 0.01 quantiles of this site
siteQuantiles = quantile(Cache500[[siteIndex]], seq(.01,.99,by=.01))

# 0.01 quantiles of a uniform
unifQuantiles = seq(.01,.99,by=.01)

# write a plot to a png file
png("KnopfMichael_Prob4.png", width=500, height=500)
# plot code goes here, like
plot(siteQuantiles ~ unifQuantiles, pch = 19, cex = .6,
     main = "Site Quantiles vs. Uniform Quantiles",
     xlab = "Uniform Quantiles",
     ylab = "Site Quantiles")
abline(a=0, b=719, col = "red", lwd = 4)
dev.off() # this tidbit finishes plotting the png
################################################################################
# PROBLEM 5
# vector of all of the time changes for the sites in whichOK
lumpedChanges = unlist(Cache500[whichOK])

# 0.01 quantiles of the lumped changes
lumpedQuantiles = quantile(lumpedChanges, seq(.01,.99,by=.01))

# write a plot to a png file
png("KnopfMichael_Prob5.png", width=500, height=500)
# plot code goes here, like
plot(lumpedQuantiles ~ unifQuantiles, pch = 19, cex = .6,
     main = "Lumped Quantiles vs. Uniform Quantiles",
     xlab = "Uniform Quantiles",
     ylab = "Lumped Quantiles")
dev.off() # this tidbit finishes plotting the png

################################################################################
# PROBLEM 6
# generate as many random unif[0, 720] variables as changes for your chosen site
simSiteChanges = sample.int(719, numChanges[siteIndex], replace = FALSE)

# 0.01 quantiles of the simulated time changes
simSiteQuantiles = quantile(simSiteChanges, unifQuantiles)

# generate as many random normal(0, 1) variables as changes for your chosen site
normChanges = rnorm(numChanges[siteIndex])

# 0.01 quantiles of the normal variables
normQuantiles = quantile(normChanges, unifQuantiles)

# write a plot to a png file
png("KnopfMichael_Prob6.png", width=900, height=500)
par(mfrow=c(1,2)) # fill this in to make two side-by-side plots
plot(simSiteQuantiles ~ unifQuantiles, pch = 19, cex = .6,
     main = "Sim Site Quantiles vs. Uniform Quantiles",
     xlab = "Uniform Quantiles",
     ylab = "Sim Site Quantiles") # the first plot, with the simulated uniform data
plot(normQuantiles ~ unifQuantiles, pch = 19, cex = .6,
     main = "Normal Quantiles vs. Uniform Quantiles",
     xlab = "Uniform Quantiles",
     ylab = "Normal Quantiles") # the second plot, with the simulated normal data
dev.off() # this tidbit finishes plotting the png
  




