a = 1
t = 2
N = 100

samp = function()
{
  x = runif(1,0,1)
  return (a*(1-x)^-(1/t))
}

mle = function(s)
{
  return( ( sum(log(s))/N - log(a))^-1 )
}

m = replicate(n = 1000, expr = mle(replicate(n = N, expr = samp())))
var(m)
