# Michael Knopf
# 24457981

library(MASS)

dat = read.csv("gamma-arrivals.csv", header = FALSE) $ V1
hist(dat, main = "Interarrival Times", xlab = "Time",
     freq = FALSE, xlim = c(0,700), ylim = c(0,.012))

### a) It does appear that the gamma distribution looks plausible, since
###    the histogram rises quickly then decays.

n = length(dat)
mu = mean(dat)
sigmasqr = (n-1)/n*var(dat)

alpha.mom = mu^2/sigmasqr
lambda.mom = mu/sigmasqr

mle = fitdistr(dat,dgamma,list(shape=alpha.mom,rate=lambda.mom))
alpha.mle = mle$estimate[1]
lambda.mle = mle$estimate[2]

### b) The parameter estimations obtained using the method of moments
###    were almost exactly equal to those obtained using the method of
###    maximum likelihood.

curve(dgamma(x,shape=alpha.mom,rate=lambda.mom),lwd=3,add=TRUE)
curve(dgamma(x,shape=alpha.mle,rate=lambda.mle),lwd=2,col="red",lty=2,add=TRUE)
legend("topright", legend = c("Method of Moments", "Maximum Likelihood"),
       cex = 1, col = c("Black", "Red"), lty = c(1,2))

### c) The fits look very reasonable in comparison with the histogram.

