q = function(n,i) return (i**2 / (2*i**2 + n**2 - 2*i*n))

trial = function(n, x0)
{
  q1 = q(n,x0)
  x = x0
  while (x > 0 & x < n)
  {
    pick = runif(1, 0, 1)
    if (pick < q1) x = x - 1
    else x = x + 1
    q1 = q(n,x)
  }
  return (1 - x/n)
}

go = function(times, n, i) return(replicate(n = times, trial(n, i)))