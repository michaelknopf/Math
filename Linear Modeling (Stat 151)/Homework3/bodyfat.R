setwd("~/Desktop/Stat 151")
body = read.csv("BodyFat.csv")
ft = lm(BODYFAT ~ AGE + WEIGHT + HEIGHT + THIGH, data = body)

n = nrow(body)
p = 4
x = matrix(c(rep(1, n), body$AGE, body$WEIGHT, body$HEIGHT, body$THIGH), nrow = n)
h = x %*% solve(t(x)%*% x) %*% t(x)

fit = unname(ft$fitted.values)
res = unname(ft$residuals)
sigma = sqrt(deviance(ft)/df.residual(ft))
stdRes = unname(res / (sigma*(1-diag(h))))
predRes = unname(res / (1 - diag(h)))
stdPredRes = unname(stdRes*sqrt((n-p-1) / (n-p-stdRes^2)))
cook = stdRes^2 * diag(h) / ((1-diag(h)) * (p+1))
par(mfrow = c(3,3))
# a) Residuals against fitted values
plot(fit, res, pch = 19, cex = .5, xlab = "Fitted Values", ylab = "Residuals")
# b) Standardized residuals against fitted values
plot(fit, stdRes, pch = 19, cex = .5, xlab = "Fitted Values", ylab = "Standardized Residuals")
# c) Residuals against Standardized Residuals
plot(res, stdRes, pch = 19, cex = .5, xlab = "Residuals", ylab = "Standardized Residuals")
# d) Predicted residuals against fitted values
plot(fit, predRes, pch = 19, cex = .5, xlab = "Fitted Values", ylab = "Predicted Residuals")
# e) Residuals against predicted residuals
plot(predRes, res, pch = 19, cex = .5, xlab = "Predicted Residuals", ylab = "Residuals")
# f) Residuals against leverage
plot(diag(h), res, pch = 19, cex = .5, xlab = "Leverage", ylab = "Residuals")
# g) Predicted residuals against standardized predicted residuals
plot(stdPredRes, predRes, pch = 19, cex = .5, xlab = "Standardized Predicted Residuals", ylab = "Predicted Residuals")
# h) Standardized residuals against standardized predicted residuals
plot(stdPredRes, stdRes, pch = 19, cex = .5, xlab = "Standardized Predicted Residuals", ylab = "Standardized Residuals")
# i) Cook's distance against the ID number of the subjects
plot(body$IDNO, cook, pch = 19, cex = .5, xlab = "ID", ylab = "Cook's Distance")

bonferroni = .05 / n
pValues = sapply(stdPredRes, function(t) {
  2*(1-pt(abs(t), n-p-2))
})
which(pValues < .05)
which(pValues < bonferroni)
par(mfrow = c(1,1))
plot(body$IDNO, pValues, pch = 19, cex = .5, xlab = "ID", ylab = "p-values")
abline(h = .05, col = 'red')
abline(h = bonferroni, col = 'blue')

ftDropOutliers = lm(BODYFAT ~ AGE + WEIGHT + HEIGHT + THIGH, data = body[-c(39,42),])
setwd("~/Desktop/Stat 151/Homework3")
sink(file = "ft.R")
summary(ft)
sink()
setwd("~/Desktop/Stat 151/Homework3")
sink(file = "ftDropOutliers.R")
summary(ftDropOutliers)
sink()