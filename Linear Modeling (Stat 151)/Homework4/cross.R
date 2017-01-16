models = list()
models[[1]] = backward()
models[[2]] = forward()
models[[3]] = adjRsqr()
models[[4]] = AIC()
models[[5]] = BIC()
models[[6]] = Mallow()

variables = sapply(models, function(x) names(x$coefficients[-1]))
exclude = sapply(0:5, function(k) {
  return(1:42 + 42*k)
})

errs = rep(0,6)

for (i in 0:5){
  error = sapply(1:6, function(k) {
    if(k+i > 6){
      j = (k+i)%%6
    } else {
      j = k+i
    }
    model = lm(BODYFAT ~ ., data[-exclude[,j], c("BODYFAT",variables[[k]])])
    beta = model$coefficients
    prederrors = sapply(exclude[,j], function(obs) {
      x = c(1,as.numeric(data[obs,variables[[k]]]))
      pred = sum(x*beta)
      obs = data[obs,"BODYFAT"]
      return(sum(pred - obs)^2)
    })
    return(sum(prederrors))
  })
  errs = errs + error
}
# errs: 4303.229 4303.229 4303.229 4303.229 4361.413 4303.229