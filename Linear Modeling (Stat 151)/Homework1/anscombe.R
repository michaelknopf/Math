library(datasets)
a <- anscombe
par(mfrow=c(2,2))

ft1 = lm(y1 ~ x1, data = a)
ft2 = lm(y2 ~ x2, data = a)
ft3 = lm(y3 ~ x3, data = a)
ft4 = lm(y4 ~ x4, data = a)

plot(a$x1,a$y1, main=paste("Dataset One"), pch = 19, cex = .8, xlab = "x1", ylab = 'y1')
abline(ft1, col = 'red', lwd = 2)
plot(a$x2,a$y2, main=paste("Dataset Two"), pch = 19, cex = .8, xlab = "x2", ylab = 'y2')
abline(ft2, col = 'red', lwd = 2)
plot(a$x3,a$y3, main=paste("Dataset Three"), pch = 19, cex = .8, xlab = "x3", ylab = 'y3')
abline(ft3, col = 'red', lwd = 2)
plot(a$x4,a$y4, main=paste("Dataset Four"), pch = 19, cex = .8, xlab = "x4", ylab = 'y4')
abline(ft4, col = 'red', lwd = 2)

predict(ft1, data.frame(x1 = 10))
predict(ft2, data.frame(x2 = 10))
predict(ft3, data.frame(x3 = 10))
predict(ft4, data.frame(x4 = 10))