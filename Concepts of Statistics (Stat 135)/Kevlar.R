kevlar = read.csv("kevlar90.csv", header = FALSE)[,1]
n = nrow(kevlar)
rate = 1/mean(kevlar)
quants = qexp(p = (1:100)/101, rate = rate)
qqplot(x = kevlar, y = quants, main = "Kevlar Time Until Failure Data\nvs.\nExponential Quantiles",
       xlab = "Kevlar Quantiles", ylab = "Exponential Quantiles")
abline(a = 0, b = 1)