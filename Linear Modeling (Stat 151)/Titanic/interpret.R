prediction = function(ft, threshold) {
  as.numeric(sapply(ft$fitted.values, function(x) x > threshold))
}

confusion = function(ft, threshold) {
  pred = prediction(ft, threshold)
  obs = ft$y
  a = sum((1-pred)*(1-obs))
  b = sum(pred*(1-obs))
  c = sum((1-pred)*obs)
  d = sum(pred*obs)
  matrix(c(a,c,b,d), nrow = 2)
}

findthresh.ad = function(ft, thresholds) {
  sapply(thresholds, function(x) {
    mat = confusion(ft, x)
    return(sum(diag(mat)))
  })
}

findthresh.bc = function(ft, thresholds) {
  sapply(thresholds, function(x) {
    mat = confusion(ft, x)
    return(mat[2,1] + mat[1,2])
  })
}

thresholds = (0:1000)/1000

a = findthresh.ad(ft, thresholds)
b = findthresh.bc(ft, thresholds)

which(a == max(a))
which(b == min(b))
### Note that which(a == max(a)) equals which(b == min(b))


qplot(rep(thresholds,2), c(a,b), ylab = "AIC / BIC", xlab = "Index") +
  geom_vline(x= thresholds[median(which(a == max(a)))], col = "blue") +
  geom_hline(y = max(a), col = "red") + geom_hline(y = min(b), col = "red")

threshold = .52

