ft = lm(BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + HIP + THIGH + FOREARM + WRIST, data)
par(mfrow = c(1,2))
scatter.smooth(ft$fitted.values, ft$residuals, span = 1000000, lpars = list(col = 'red', lwd = 1.5),
               pch = 19, cex = .5, ylab = "Residuals", xlab = "Fitted Values")
qqnorm(ft$residuals, pch = 19, cex = .3, main = "", ylab = "Normal Quantiles", xlab = "Residuals")
par(mfrow = c(3,5))
for(i in allvars[-1])
{
  scatter.smooth(data[,i], ft$residuals, span = 1000000, lpars = list(col = 'red', lwd = 1.5),
                 pch = 19, cex = .3, ylab = "Residuals", xlab = i)
}

n = nrow(data)
p = 9
x = matrix(c(rep(1, n), as.numeric(as.matrix(data[,names(ft$coefficients)[-1]]))), nrow = n)
h = x %*% solve(t(x)%*% x) %*% t(x)

fit = unname(ft$fitted.values)
res = unname(ft$residuals)
sigma = sqrt(deviance(ft)/df.residual(ft))
stdRes = unname(res / (sigma*(1-diag(h))))
predRes = unname(res / (1 - diag(h)))
stdPredRes = unname(stdRes*sqrt((n-p-1) / (n-p-stdRes^2)))
cook = stdRes^2 * diag(h) / ((1-diag(h)) * (p+1))
par(mfrow = c(3,3))
# Residuals against fitted values
plot(fit, res, pch = 19, cex = .5, xlab = "Fitted Values", ylab = "Residuals")
# Standardized residuals against fitted values
plot(fit, stdRes, pch = 19, cex = .5, xlab = "Fitted Values", ylab = "Standardized Residuals")
# Residuals against Standardized Residuals
plot(res, stdRes, pch = 19, cex = .5, xlab = "Residuals", ylab = "Standardized Residuals")
# Predicted residuals against fitted values
plot(fit, predRes, pch = 19, cex = .5, xlab = "Fitted Values", ylab = "Predicted Residuals")
# Residuals against predicted residuals
plot(predRes, res, pch = 19, cex = .5, xlab = "Predicted Residuals", ylab = "Residuals")
# Residuals against leverage
plot(diag(h), res, pch = 19, cex = .5, xlab = "Leverage", ylab = "Residuals")
# Predicted residuals against standardized predicted residuals
plot(stdPredRes, predRes, pch = 19, cex = .5, xlab = "Standardized Predicted Residuals", ylab = "Predicted Residuals")
# Standardized residuals against standardized predicted residuals
plot(stdPredRes, stdRes, pch = 19, cex = .5, xlab = "Standardized Predicted Residuals", ylab = "Standardized Residuals")
# Cook's distance against the ID number of the subjects
plot(body$IDNO, cook, pch = 19, cex = .5, xlab = "ID", ylab = "Cook's Distance")

bonferroni = .05 / n
pValues = sapply(stdPredRes, function(t) {
  2*(1-pt(abs(t), n-p-1))
})
which(pValues < .01)
# Returns 39 224
which(pValues < bonferroni)
### Returns integer(0)
ftdrop = lm(BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + HIP + THIGH + FOREARM + WRIST, data[-c(39,224),])
