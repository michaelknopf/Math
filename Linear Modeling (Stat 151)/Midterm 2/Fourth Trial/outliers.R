# Initialize selected model
ft = lm(formula = medv ~ rm + ptratio + crim + chas, data = bost)
ft = lm(formula = medv ~ rm + ptratio + crim, data = bost)

# Compute statistics used in outlier detection
coefs = names(ft$coefficients)[-1]
n = length(ft$fitted.values)
p = length(coefs)
x = matrix(c(rep(1,n), as.matrix(bost[,coefs])), nrow = n)
h = x %*% solve(t(x)%*% x) %*% t(x)

fit = unname(ft$fitted.values)
res = unname(ft$residuals)
sigma = summary(ft)$sigma
stdRes = unname(res / (sigma*(1-diag(h))))
predRes = unname(res / (1 - diag(h)))
stdPredRes = unname(stdRes*sqrt((n-p-1) / (n-p-stdRes^2)))
cook = stdRes^2 * diag(h) / ((1-diag(h)) * (p+1))

# Make regression diagnostic plots
plots = list()
plots[[1]] = qplot(x = res, y = diag(h), xlab = "Leverage", ylab = "Residual",
                   geom = 'point') + theme(legend.position="none")
plots[[2]] = qplot(x = 1:n, y = cook, xlab = "Cook's Distance", ylab = "Observation #",
                   geom = 'point') + theme(legend.position="none")
plots[[3]] = qplot(x = qnorm(p = (1:n)/(n+1)), y = sort(res), geom = 'point',
                    xlab = "Normal Quantile", ylab = "Residual") + theme(legend.position="none")
plots[[4]] = qplot(x = fit, y = res, xlab = "Fitted Value", ylab = "Residual",
                   geom = 'point') + theme(legend.position="none")
plots[[5]] = qplot(x = bost$londist, y = res, xlab = "londist", ylab = "Residual",
                   geom = 'point') + theme(legend.position="none")
plots[[6]] = qplot(x = bost$crim, y = res, xlab = "crim", ylab = "Residual",
                   geom = 'point') + theme(legend.position="none")
plots[[7]] = qplot(x = bost$zn, y = res, xlab = "zn", ylab = "Residual",
                   geom = 'point') + theme(legend.position="none")
plots[[8]] = qplot(x = bost$chas, y = res, xlab = "chas", ylab = "Residual",
                   geom = 'point') + theme(legend.position="none")
plots[[9]] = qplot(x = bost$nox, y = res, xlab = "nox", ylab = "Residual",
                   geom = 'point') + theme(legend.position="none")
plots[[10]] = qplot(x = bost$rm, y = res, xlab = "rm", ylab = "Residual",
                   geom = 'point') + theme(legend.position="none")
plots[[11]] = qplot(x = bost$dis, y = res, xlab = "dis", ylab = "Residual",
                   geom = 'point') + theme(legend.position="none")
plots[[12]] = qplot(x = bost$rad, y = res, xlab = "rad", ylab = "Residual",
                   geom = 'point') + theme(legend.position="none")
plots[[13]] = qplot(x = bost$tax, y = res, xlab = "tax", ylab = "Residual",
                   geom = 'point') + theme(legend.position="none")
plots[[14]] = qplot(x = bost$ptratio, y = res, xlab = "ptratio", ylab = "Residual",
                   geom = 'point') + theme(legend.position="none")
plots[[15]] = qplot(x = bost$b, y = res, xlab = "b", ylab = "Residual",
                   geom = 'point') + theme(legend.position="none")
plots[[16]] = qplot(x = bost$lstat, y = res, xlab = "lstat", ylab = "Residual",
                   geom = 'point') + theme(legend.position="none")

multiplot(plotlist = plots, layout = matrix(1:16, nrow = 4, byrow = TRUE))

# Identify significant observations (outliers)
bonferroni = .05 / n
pValues = sapply(stdPredRes, function(t) {
  2*(1-pt(abs(t), n-p-2))
})

# Plot pValues vs. observation number and compare to Bonferroni line
plot.pValues = qplot(x = 1:n, y = pValues, xlab = "Observation #", ylab = "pValue",
      geom = 'point', size = I(4)) + theme(legend.position="none")

# Plot influential observations with Bonferroni line
plot.bon = qplot(x = 1:n, y = pValues, xlab = "Observation #", ylab = "pValue",
      geom = 'point', size = I(5)) + theme(legend.position="none") + geom_hline(y=bonferroni, col = "red") +
      coord_cartesian(xlim=c(368,374), ylim = c(-.00001, bonferroni*1.5))

multiplot(plot.pValues, plot.bon, cols = 2)

# Create model with outliers dropped
ftDropOutliers = lm(formula = medv ~ rm + ptratio + crim, data = bost[-which(pValues < bonferroni),])
