# n = 10
# trials = 100000
# 
# samp = replicate(n = trials, expr = sample(c(0,1), size = n, replace = TRUE))
# p = apply(X = samp, MARGIN = 2, FUN = sum) / n
# 

sim = function () {
  
  print(mean(replicate(n = 1000000, expr = sum(rpois(n = 10, lambda = .1))) >= 3))
  
}
