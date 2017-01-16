body = read.csv("BodyFat.csv")

fty = lm(BODYFAT ~ THIGH, data = body)
ftx = lm(THIGH ~ BODYFAT, data = body)
a0 = coefficients(ftx)[1]
a1 = coefficients(ftx)[2]

plot(body$BODYFAT ~ body$THIGH, pch = 19, cex = .4, main = "Bodyfat vs. Thigh", xlab = "Thigh", ylab = "Bodyfat",
     xlim = c(50,87))
abline(fty, col = 'red', lwd = 2)
abline(-a0/a1, 1/a1, col = 'blue', lwd = 2)
legend(75,10, bty = 'n', legend = c('predictor: thigh', 'predictor: bodyfat'), cex = .8, lwd = 2, col = c('red', 'blue'))