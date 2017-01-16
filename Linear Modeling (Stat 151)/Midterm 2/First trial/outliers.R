# Initialize selected model
ft = lm(formula = medv ~ londist + crim + zn + chas + nox + rm + dis + 
     rad + tax + ptratio + b + lstat, data = bost)

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

# Plots
cex = .3
par(mfrow = c(3,3))
# Residuals against fitted values
plot(fit, res, pch = 19, cex = cex, xlab = "Fitted Values", ylab = "Residuals")
# Standardized residuals against fitted values
plot(fit, stdRes, pch = 19, cex = cex, xlab = "Fitted Values", ylab = "Standardized Residuals")
# Residuals against Standardized Residuals
plot(res, stdRes, pch = 19, cex = cex, xlab = "Residuals", ylab = "Standardized Residuals")
# Predicted residuals against fitted values
plot(fit, predRes, pch = 19, cex = cex, xlab = "Fitted Values", ylab = "Predicted Residuals")
# Residuals against predicted residuals
plot(predRes, res, pch = 19, cex = cex, xlab = "Predicted Residuals", ylab = "Residuals")
# Residuals against leverage
plot(diag(h), res, pch = 19, cex = cex, xlab = "Leverage", ylab = "Residuals")
# Predicted residuals against standardized predicted residuals
plot(stdPredRes, predRes, pch = 19, cex = cex, xlab = "Standardized Predicted Residuals", ylab = "Predicted Residuals")
# Standardized residuals against standardized predicted residuals
plot(stdPredRes, stdRes, pch = 19, cex = cex, xlab = "Standardized Predicted Residuals", ylab = "Standardized Residuals")
# Cook's distance against observation #
plot(1:n, cook, pch = 19, cex = cex, xlab = "Observation #", ylab = "Cook's Distance")

# Identify significant observations (outliers)
bonferroni = .05 / n
pValues = sapply(stdPredRes, function(t) {
  2*(1-pt(abs(t), n-p-2))
})

# Plot pValues vs. observation number and compare to Bonferroni line
par(mfrow = c(1,1))
plot(1:n, pValues, pch = 19, cex = cex, xlab = "Observation #", ylab = "p-values")
abline(h = .05, col = 'red')
abline(h = bonferroni, col = 'blue')

# Plot influential observations with Bonferroni line
plot(1:n, pValues, pch = 19, cex = .5, main = "Influential Observations", xlab = "Observation #",
     ylab = "p-values", xlim = c(368,374), ylim = c(0,bonferroni*1.5))
abline(h = bonferroni, col = 'blue')

# Create model with outliers dropped
ftDropOutliers = lm(formula = medv ~ londist + crim + zn + chas + nox + rm + dis +
                      rad + tax + ptratio + b + lstat, data = bost[-which(pValues < bonferroni),])


setwd("~/Desktop/Stat 151/Midterm 2")
sink(file = "summary.R")
summary(ft)
sink()

