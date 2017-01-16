genKids = function(id, bTime, aTime, lambda = 1, kappa = 1) {
  
  nChild = rpois(n = length(id),
                 lambda = lambda*(aTime - bTime))
  
# This code was much slower than the current program.
#   if(all(nChild == 0))
#   {
#     return (data.frame())
#   }
  
  ### Remove parents with no children
  id = id[nChild != 0]
  bTime = bTime[nChild != 0]
  aTime = aTime[nChild != 0]
  nChild = nChild[nChild != 0]
  
#   Rprof(NULL)
#   Rprof('childtimes', append = TRUE)

### This code was much slower than the present program.
#   for (i in seq(length(id)))
#   {
#     bTimeChild[[i]] = runif(nChild[i], bTime[i], aTime[i])
#   }

  bTimeChild = unlist(apply(X = matrix(c(nChild,bTime,aTime),ncol=3),
                            MARGIN = 1,
                            FUN = function(x) runif(x[1],x[2],x[3])))

### This code was much slower than the present program.
# for (i in seq(length(id)))
# {
#   aTimeChild[[i]] = aTime[i] + rexp(nChild[i], kappa)
# }

  aTimeChild = unlist(apply(X = matrix(c(nChild,aTime), ncol = 2),
                            MARGIN = 1,
                            FUN = function(x) x[2] + rexp(x[1], kappa)))
#   Rprof(NULL)
#   Rprof('mainprof', append = TRUE)
  
  data.frame(parentID = rep(id, times = nChild),
             id = seq(bTimeChild),
             birth = bTimeChild,
             death = aTimeChild)
}

familyTree = function(lambda = 1, kappa = 1, maxGen = 10, maxTime = 5*lambda) {

  allGens = list()
  allGens[[1]] = data.frame(parentID = 0,
                            id = 1,
                            birth = 0,
                            death = rexp(1,kappa))
  
  ### Check that maxGen has not been exceeded
  ### and that potential parents still remain
  while(length(allGens) < maxGen & dim(allGens[[length(allGens)]])[1] > 0)
  {
    nextGen = genKids(id = allGens[[length(allGens)]]$id,
                      bTime = allGens[[length(allGens)]]$birth,
                      aTime = allGens[[length(allGens)]]$death,
                      lambda = lambda,
                      kappa = kappa)
    
    ### Remove children born after maxTime
    allGens[[length(allGens) + 1]] = nextGen[nextGen$birth < maxTime,]
  }
  
  ### Check if last generation is empty
  if (dim(allGens[[length(allGens)]])[1] == 0)
  {
    ### Remove last generation
    allGens = allGens[-length(allGens)]
    
    if (dim(nextGen)[1] > 0)
    {
      ### If the program reaches this point, then some generation contains no recorded children,
      ### yet the last recorded generation had survived.  This implies that children were born
      ### after maxTime but the family appears to have died out.  Fill the remainder of allGens
      ### with empty data frames to correct this.
      for (i in seq(length(allGens)+1, 10)) allGens[[i]] = data.frame(NULL)
    }
  }
  
  return(allGens)
}  	   

### Extra Credit
### PROFILE genKids and familyTree
### Identify where your code is taking most of the time
### Improve the efficiency of your code

test.Gen = function(){
  # This function is provided to you to check your code
  # With the seed set at 222222222,
  # You should produce a family tree that looks as follows
  
  #	[[1]]
  #    parentID id birth    death
  #	1        0  1     0 1.465804
  
  #	[[2]]
  #	   parentID id     birth    death
  #	1        1  1 0.3507661 2.483865
  
  #	[[3]]
  #	   parentID id     birth    death
  #	1        1  1 0.5760519 2.755471
  #	2        1  2 1.9161281 5.377054
  
  #	[[4]]
  #	   parentID id    birth    death
  #	1        1  1 1.726824 2.968156
  #	2        1  2 2.438052 3.308342
  #	3        1  3 2.723489 3.720874
  #	4        2  4 2.566992 6.202776
  #	5        2  5 3.029503 6.735076
  
  #	[[5]]
  #	   parentID id    birth     death
  #	1        4  1 3.909691 12.416104
  #	2        4  2 4.211997  6.840689
  #	6        5  6 4.810271  8.878621
  #	7        5  7 4.925031  6.917579
  
  #	[[6]]
  #	   parentID id    birth   death
  #	1        1  1 4.058793 12.8465
  #	2        1  2 4.737611 12.5524
  
  #	[[7]]
  #	   parentID id    birth    death
  #	1        1  1 4.837545 14.37369
  
  #	[[8]]
  #	   parentID id   birth    death
  #	1        1  1 4.84967 16.05662
  
  set.seed(222222222)
  xx = familyTree()
  if (length(xx) == 8) {
    cat("Correct number of generations\n")
  } else cat("Incorrect number of generations\n")
  if (xx[[1]]$death > 1.4658 & xx[[1]]$death < 1.46581) {
    cat("Gen 1 assassinated at correct time\n")
  } else cat("Incorrect assassination time for first person\n")
  if (all(sapply(xx, nrow) == c(1,1,2,5,4,2,1,1))) {
    cat("Correct number of offspring in each generation\n")
  } else cat("Incorrect number of offspring in each generation\n")
  if (xx[[8]]$death > 16.0566 & xx[[8]]$death < 16.05663) {
    cat("Gen 8 assassinated at correct time\n")
  } else cat("Incorrect assassination time for last person\n")
}



