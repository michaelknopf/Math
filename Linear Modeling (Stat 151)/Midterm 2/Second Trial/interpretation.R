data = bost[-which(pValues < bonferroni),]
ft = lm(formula = medv ~ londist + crim + zn + chas + nox + rm + dis +
          rad + tax + ptratio + b + lstat, data = data)
variables = names(ft$coefficients)[-1]
summ = data.frame(coef = c(ft$coefficients),
                  min = c(0, apply(data[,variables], 2, min)),
                  firstQuartile = c(0, apply(data[,variables],2, function(x) quantile(x, probs = .25))),
                  median = c(0, apply(data[,variables], 2, median)),
                  thirdQuartile = c(0, apply(data[,variables],2, function(x) quantile(x, probs = .75))),
                  max = c(0, apply(data[,variables], 2, max)))
summ = rbind(c(0,min(data$medv),
               quantile(data$medv, probs = .25),
               median(data$medv),
               quantile(data$medv, probs = .75),
               max(data$medv)),
             summ)
rownames(summ)[1] = "medv"

summ$improvement = abs(summ$coef*(summ$thirdQuartile - summ$firstQuartile))
summ = summ[rev(order(summ$improvement)),]
summ[13,2:7] = rep(NA,6)
summ[14,c(1,7)] = c(NA,NA)

setwd("~/Desktop/Stat 151/Midterm 2/report")
sink(file = "summary.R")
summary(ftDropOutliers)
sink()

setwd("~/Desktop/Stat 151/Midterm 2/report")
sink(file = "interpretation.R")
summ
sink()
