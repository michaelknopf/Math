# load(url("http://www.stat.berkeley.edu/users/nolan/data/EmailsDist.rda"))
ties <<- 0

predNeighbor = function(k, distMat, indSpam) {
  
  ord = cbind(t(apply(distMat, 1, order))[,1:max(k)])
  votes = apply(ord, 1:2, function(x) indSpam[x])
  
  sapply(k, function(x) {
    avgSpam = apply(cbind(votes[,1:x]), 1, mean)
    for (j in 1:length(avgSpam))
    {
      if (avgSpam[j] == .5)
      {
        ties <<- ties + 1
        avgSpam[j] = votes[j,1]
      }
    }
    avgSpam > .5 })
}

cvKnn = function(distEmails, isSpam, k = 1:20, vfold = 10) {
  
  n = nrow(distEmails)
  groupSize = n %/% vfold + 1
  r = n %% vfold
  ord = sample(n)
  groups = list()
  
  for (i in seq(1,r,length=r)){
    groups[[i]] = ord[1:groupSize]
    ord = ord[-(1:groupSize)]
  }
  
  groupSize = groupSize - 1
  
  for (i in seq(r+1, length = vfold - r)){
    groups[[i]] = ord[1:groupSize]
    ord = ord[-(1:groupSize)]
  }
  
  preds = list()
  
  for (i in 1:vfold){
    test = groups[[i]]
    train = unlist(groups[-i])
    preds[[i]] = predNeighbor(k, distEmails[test,train], indSpam = isSpam[train])
  }
  
  preds = do.call(rbind,preds)
  return(preds[order(as.numeric(rownames(preds))),])
}


### Determine spam status of emails
preds = cvKnn(distEmails, isSpam, k=1000)
### Determine misclassification rate
missRate = 1-apply(cbind(preds), 2, function(x) mean(x == isSpam))
### Determine most successful k-value
best = which(missRate == min(missRate))
### Plot results
scatter.smooth(missRate, main = "Misclassification Rate \nvs.\n Maximum Distance",
               xlab = "Maximum Distance (k)", ylab = "Misclassification Rate",
               col = c("green", "red"),
               pch = 19, cex = .8, axes = FALSE, bty = 'n', ylim = c(min(.08,missRate), max(.105,missRate)),
               span = 15/24, lpars = list(col = rgb(0, 0, 1, 0.5), lwd = 5))

axis(side = 1, at = 0:20)                                   # x-axis
axis(side = 2, at = c(.08,.085,.09,.095, .1, .105))         # y-axis

# best k-value
abline(v=best, lwd = 1.5, col = "dark green")
text(best, .25*min(missRate) + .75*max(missRate),
     labels = paste("k =",best), cex = .9, col = "dark green", pos = 4)

legend("bottomright", bty = 'n',
       c("Odd","Even", "Best k"), cex = .8, lwd = 2,
       lty = c(0,0,1), pch = c(19,19,NA), col = c("green","red", "dark green"))


#############################   Conclusion   ################################

### If I was forced to choose one k-value, I would choose 6.

### As I run the program several times, the graphs usually show the most successful k-value to be
### between 4 and 8.  Running the program 100 times with k = 1:10 and viewing summary statistics showed
### that the average of the k-values that produced the lowest misclassification rates was 5.86.  The
### median was 6, and the mode was 4.  The standard error was 1.93.  As the plot shows, the misclassification
### rate tends to rise almost linearly, but slightly concave downward, as k increases beyond 8.
### If we plot for k = 1:1000, the graph actually shows that, around k=250, the misclassification rate
### begins to level out and asymptotically approach .256219.

### An interesting feature of the graph, which also reflected itself in this 100-element sample,
### was even k-values are more successful than odd k-values.  In fact, the most successful k-value
### was even 95% of the time. This is due to the method by which we evaluate ties, using the closest
### neighbor's vote as a tie-breaker.  If some given email results in a tie (this can only happen
### for an even k-value), then we refer to the closest neighbor to make a decision.  For the next
### k-value, which is odd, this same email will no longer be a tie, since it will be decided based
### on its kth closest neighbor, which we would assume on average to be less accurate than the closest
### neighbor.  So it would make sense to use an even k-value rather than an odd one.

### If this program were actually used as a spam filter, it would make sense not to weight the votes
### equally.  The first few votes should weigh more heavily than the later ones.  It might make sense
### to weigh the first 4 most heavily, then the 5th and 6th, and finally the 7th and 8th should have
### weights small enough that they only factor into a decision in the rare case where the first 6 votes
### essentially even out.




# testRun = function(){
#   preds = cvKnn(distEmails, isSpam, k=1:10)
#   missRate = 1-apply(preds, 2, function(x) mean(x == isSpam))
#   mean(which(missRate == min(missRate)))
# }
# 
# y = replicate(n = 100, testRun())



