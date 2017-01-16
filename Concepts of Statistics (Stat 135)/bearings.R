dat = read.csv("vitaminC3.csv")
plasmaS = dat[[1]]
plasmaC = dat[[3]]
urS = dat[[2]]
urC = dat[[4]]

cor(plasmaS,urS)
cor(plasmaC,urC)

par(mfrow=c(2,3))

boxplot(plasmaS, plasmaC, main = "Plasma\n(Schizophrenic vs. Control)")
boxplot(urS, urC, main = "Urine\n(Schizophrenic vs. Control)")

qqnorm(plasmaS,
       main = "Quantile Plot",
       xlab = "Normal Quantiles",
       ylab = "Plasma (Schizophrenic)",
       pch = 19)

qqnorm(plasmaC,
       main = "Quantile Plot",
       xlab = "Normal Quantiles",
       ylab = "Plasma (Control)",
       pch = 19)

qqnorm(urS,
       main = "Quantile Plot",
       xlab = "Normal Quantiles",
       ylab = "Urine (Schizophrenic)",
       pch = 19)

qqnorm(urC,
       main = "Quantile Plot",
       xlab = "Normal Quantiles",
       ylab = "Urine (Control)",
       pch = 19)
#







# poly = function()
# {
#   sample(1:20, size = sample(3:8), replace = TRUE)
# }


# p = poly()
# f = function(x)
# {
#   sum = 0
#   for (i in 1:length(p))
#   {
#     sum = sum + p[i]*(x**(i-1))
#   }
#   return(sum)
# }


# dat = read.csv("bearings.csv")
# x = dat[[1]]
# y = dat[[2]]
# 
s_p = function(x,y){
  n = length(x)
  m = length(y)
  sqrt(((n-1)*var(x) + (m-1)*var(y))/(m+n-2))
}

s_xy = function(x,y)
{
  s_p(x,y)*sqrt(1/length(x) + 1/length(y))
}
# 
# s_p(x,y)**2
# length(y)

