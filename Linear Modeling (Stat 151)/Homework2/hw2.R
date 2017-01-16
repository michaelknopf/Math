f = function(n) {
  a = sample(1:(2*n),size = 2*n, replace = FALSE)
  m = matrix(a,nrow = 2)
  return(m)
}


n=3
A = list()
for(i in 1:2500)
{
  a = f(n)
  if (!(a %in% A))
  {
    A[[length(A)+1]] = a
  }
  if (i%%100 == 0)
  {
    print(i)
  }
}