# load(url("http://www.stat.berkeley.edu/users/nolan/data/EmailsDist.rda"))
# 
# predNeighbor = function(k, distMat, indSpam) {
#   
#   ord = t(apply(distMat, 1, order))[,1:max(k)]
#   votes = apply(ord, 1:2, function(x) indSpam[x])
# 
#   sapply(k, function(x) {
#       avgSpam = apply(votes[,1:x], 1, mean)
#       for (j in 1:length(avgSpam))
#       {
#         if (avgSpam[j] == .5) avgSpam[j] = votes[j,1]
#       }
#       avgSpam > .5 })
#   
# }

predNeighbor = function(k, distMat, indSpam) {
  
  ord = cbind(t(apply(distMat, 1, order))[,1:max(k)])
  votes = apply(ord, 1:2, function(x) indSpam[x])
  
  sapply(k, function(x) {
    avgSpam = apply(cbind(votes[,1:x]), 1, mean)
    for (j in 1:length(avgSpam))
    {
      if (avgSpam[j] == .5) avgSpam[j] = votes[j,1]
    }
    avgSpam > .5 })
}

testRun = function(){
  
  test = sample(2412, size = 242)
  distMat = distEmails[test,-test]
  indSpam = isSpam[-test]
  k = 1
  
  spamTruth = isSpam[test]
#   spamPred = predNeighbor(k, distMat, indSpam)
  spamPred = predNeighbor(k, distMat, indSpam)
  
#   assess = table(spamPred, spamTruth)
#   errs = (assess[1,2] + assess[2, 1]) / length(spamTruth)
  
  assess = table(spamPred, spamTruth)
  errs = (assess[1,2] + assess[2, 1]) / length(spamTruth)
  
  print(assess)
  print(errs)
  print(mean(spamTruth == spamPred))
}



