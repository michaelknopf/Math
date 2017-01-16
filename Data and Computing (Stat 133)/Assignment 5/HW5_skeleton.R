################################################################################
# HOMEWORK 5, FALL 2014
# YOUR NAME:


################################################################################
# PROBLEM 1
# don't change this function, just leave it as is
# you'll use it for the first problem
calcMaxDiff = function(x){
  scaled.x = sort(x)/720
  # real data is sorted, but we will use
  # this later for simulated data which is not
  unifQuantiles = seq(1/(length(x)+1), by=1/(length(x)+1), 
                      length.out=length(x))
  maxDiff = max(abs( scaled.x - unifQuantiles ))
  
  return(maxDiff)  
}



################################################################################
# PROBLEM 2
# flesh this function out:

simMaxDiff = function(ARGUMENTS_HERE){
  # create a matrix simUnifMatrix of unif[0, 1] data with
  # num of rows = length of the data vector and
  # num of cols = n
  simUnifMatrix = 
  
  # use apply() and calcMaxDiff() to get a vector
  # simMaxDiffs which gives max differences
  # for each column of this simulated data
  simMaxDiffs = 
    
  # apply calcMaxDiff() to the data vector, and store the 
  # result as actualMaxDiff
  actualMaxDiff = 
  
  # use rank() to determine how the actual data result,
  # actualMaxDiff, ranks among the simulated max diffs.
  # store the result as simRank
  simRank =
  
  # use return() to return the value of simRank

}



################################################################################
# PROBLEM 3
# flesh this function out:

histRanks = function(ARGUMENTS_HERE){
  # find the simulated rank of each website using simMaxDiff()
  websiteRanks = 
  
  # scale the ranks to fall between 0 and 1 by dividing by n+1
  scaledRanks = 
  
  png("LastnameFirstname_Hist.png", width=500, height=500)
  # put your plotting code here:
  # make a histogram with an edge at 0.95
  hist(...)
  # draw a vertical red line through 0.95
  
  dev.off()
  
  # use return() to return the vector scaledRanks
  
}


################################################################################
# PROBLEM 4

# whichOK, given here for you to use!
whichOK = c(6L, 41L, 44L, 49L, 59L, 72L, 102L, 121L, 162L, 166L, 180L, 
187L, 204L, 220L, 273L, 286L, 300L, 339L, 349L, 358L, 395L, 404L, 
489L)

# apply histRanks to the sites given in whichOK



################################################################################
# PROBLEM 5

# pick a site that has rank 1 (give the index)
hiRankSite = 
# make a quantile plot like you did in the last homework
# for this site, and save to a png
unifQuantiles = seq(0.01, 0.99, 0.01)
hiRankQuantiles =

# write to a png titled LastnameFirstname_HW5.png
png("LastnameFirstname_QQ.png", width=500, height=500)
# make your quantile plot here
plot(unifQuantiles, hiRankQuantiles, ...)
# add a blue horizontal lines for the range where you observe
# that the website has the largest deviation from uniform

# use text to indicate whether this means more or less than unif
text(...)

dev.off()

