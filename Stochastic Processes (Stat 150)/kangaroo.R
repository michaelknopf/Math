go = function(mu) {
  n = rpois(n = 1, lambda = mu)
  y = rexp(n = n, rate = 1)
  x = runif(n = n, min = 0, max = 1)
  return(sum(x+y > 1))
}

a = replicate(n = 1000, expr = go(15))
