exptOne = function(l, k, mG, mT){
  
  tree = familyTree(lambda = l, kappa = k, maxGen = mG, maxTime = mT)
  
  ### return number of generations, number of offspring
  c(length(tree), sum(sapply(tree, function(x) dim(x)[1])) - 1)
}

MCBA = function(params, repeats = 100){
  
  maxGen = 10
  
  data = apply(params, 1, function(x) replicate(repeats, exptOne(x[1], x[2], maxGen, 5*x[1])))
  
  # put data into an array with 3 dimensions:
  # rows: statistic type (1 is number of generations, 2 is number of offspring)
  # columns: trial number
  # panels: family number
  data = array(data, dim = c(2, repeats, dim(params)[1]))
  
  # For each parameter setting, summarize the experiments by providing:
  # numEnd: number of times the family died out
  # OS50: median number of offspring for each family
  # OS25: lower quartile of the number of offspring for each family
  # OS75: upper quartile of the number of offspring for each family
  # Gen50: median number of generations for each family
  # Gen25: lower quartile of the number of generations for each family
  # Gen75: upper quartile of the number of generations for each family
  
  numEnd = apply(data, 3, function(x) sum(x[1,] < maxGen))
  
  offSum = apply(data, 3, function(x) summary(x[2,]))
  OS25 = offSum[2,]
  OS50 = offSum[3,]
  OS75 = offSum[5,]
  
  genSum = apply(data, 3, function(x) summary(x[1,]))
  Gen25 = genSum[2,]
  Gen50 = genSum[3,]
  Gen75 = genSum[5,]
  
  data.frame(numEnd, OS25, OS50, OS75, Gen25, Gen50, Gen75)
  
}


#   lambdas = c(1, 1, 1, 2, 2)
#   kappas  =  c(1, 2, 3, 1, 2)

# mcOutput = MCBA(params = matrix(c(lambdas, kappas), ncol = 2, byrow = FALSE))





