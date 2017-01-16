data = bost[-which(pValues < bonferroni),]
ft = lm(formula = medv ~ rm + crim + ptratio, data = data)
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

summ$improvement = summ$coef*(summ$thirdQuartile - summ$firstQuartile)
summ = summ[rev(order(abs(summ$improvement))),]
summ[4,2:7] = rep(NA,6)
summ[5,c(1,7)] = c(NA,NA)

setwd("~/Desktop/Stat 151/Midterm 2/report")
sink(file = "summary2.R")
summary(ftDropOutliers)
sink()

setwd("~/Desktop/Stat 151/Midterm 2/report")
sink(file = "interpretation2.R")
summ
sink()

dat2 = as.matrix(bost[-which(pValues < bonferroni),c('crim','rm','ptratio')])
cormat2 = cor(dat2)
diag(cormat2) = 0
