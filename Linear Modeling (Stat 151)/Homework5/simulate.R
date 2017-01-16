# #beta = round(runif(5,-10,10),1)
# #gamma = round(runif(1,-10,10),1)
# sigma = 2
# n=20000
# p=4
# inter = rep(1,n)
# x1 = rnorm(n, 5, 1)
# x2 = rnorm(n,10,2)
# x3 = rnorm(n,-3,3)
# x4 = rnorm(n,20,4)
# z = rnorm(n,50,5)
# x = matrix(c(inter,x1,x2,x3,x4), byrow = FALSE, nrow = n)
# xz = cbind(x,z)
# betagamma = c(beta,gamma)
# e = rnorm(n,0,sigma)
# mean(e)
# y = xz %*% betagamma + e
# 
# xzdata = data.frame(x[,-1],y)
# ft = lm(y ~ X1 + X2 + X3 + X4, data = xzdata)
# 
# y2 = x %*% beta + e
# xdata = data.frame(x[,-1],y2)
# ft = lm(y2 ~ X1 + X2 + X3 + X4, data = xdata)


run = function(n=1000) {
n=200
p=4
beta = round(runif(5,-10,10),1)
gamma = round(runif(1,-10,10),1)
betagamma = c(beta,gamma)
sigma = .1


inter = rep(1,n)
x1 = rnorm(n, 5, 1)
x2 = rnorm(n,10,2)
x3 = rnorm(n,-3,3)
x4 = rnorm(n,20,4)
z = rnorm(n,50,5)

x = matrix(c(inter,x1,x2,x3,x4), byrow = FALSE, nrow = n)
xz = cbind(x,z)
e = rnorm(n,0,sigma)
y = xz %*% betagamma + e


xdata = data.frame(xz[,-1],y)
ft = lm(y ~ V1 + V2 + V3 + V4, data = xdata)
ft2 = lm(y ~ V1 + V2 + V3 + V4 + z, data = xdata)

h = x %*% solve(t(x)%*% x) %*% t(x)
i = matrix(rep(0,n^2), nrow = n)
diag(i) = 1
v = (i-h) %*% (gamma*z)
exp = sum(v^2) + (n-p-1)*sigma^2
return((deviance(ft) - exp)/exp)
}



ft1 = lm(y ~ x1 + x2 + x3 + x4, data = xdata)
ft2 = lm(z ~ x1 + x2 + x3 + x4, data = xdata)
plot(ft2$residuals, ft1$residuals)
abline(lm(ft1$residuals ~ ft2$residuals))
lm(ft1$residuals ~ ft2$residuals)
lm(y ~ x1 + x2 + x3 + x4 + z, data = xdata)

