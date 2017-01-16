#################### Part 2 ###########################
### Team Name: MMAD

### Members:
### Brooks, Marc
### Cheng, Yizhuang Alden
### Fenson, Derek
### Knopf, Michael

setwd("~/Desktop/Stat 133/Final Project")

library("XML")
library("class")
library(maps)
options(stringsAsFactors = FALSE)


#### NOTE:  The first part of this program, Voting Data, is setup to record the election results
#### for every single candidate, not just Obama and Romney.  If a candidate was not on the ballot
#### in a particular state, an NA was recorded in that candidate's column.  Professor Ibser later
#### mentioned that he only wanted results for Obama and Romney.  To fix this, all we had to do
#### was set candidates = candidates[1:2] on line 50.  However, the program may now be longer because
#### it was originally created to do a more general task.








######################    Voting Data    ######################


statenames = as.character(read.table(
  "http://www.stat.berkeley.edu/users/nolan/data/Project2012/countyVotes2012/stateNames.txt", header = TRUE)[,1])
statenames = statenames[-2]

path = "http://www.stat.berkeley.edu/users/nolan/data/Project2012/countyVotes2012/"
states = lapply(statenames, function(x) xmlParse(file = paste(path, x, ".xml", sep = "")))

countyID = unname(unlist(lapply(states, function(state) xmlSApply(xmlRoot(state), function(x) xmlGetAttr(node = x, "id")))))

reporting = unlist(lapply(states,
                          function(state) xpathSApply(state, path = "//span[@class='precincts-reporting']", function(x) xmlValue(x))))

reporting = as.numeric(gsub("[^[:digit:].]", "", reporting))

### List of every candidate on ballot anywhere
candidates = unique(unlist(lapply(states,
                                  function(state) xpathSApply(state, "//th[@class='results-candidate']", xmlValue))))[-1]
candidates = candidates[1:2]
# candidates[c(26,29)] = candidates[c(29,26)]  ### Move "None of these candidates" to end

### Create matrix of percentages (row : county, col: candidate)
percentage = lapply(states, function(state) {
  xmlSApply(xmlRoot(state), function(county) {
    county_cands = xmlSApply(county, function(x) xmlValue(x[[1]]))
    county_cands[1] = xmlValue(county[[1]][[3]])
    
    sapply(candidates, function(candidate) {
      cand_ind = which(county_cands == candidate)
      if (length(cand_ind) == 0) return(NA)
      else if (cand_ind == 1) return(xmlValue(county[[1]][[7]]))
      else return(xmlValue(county[[cand_ind]][[5]]))
    })
  })
})
percentage = lapply(percentage, function(x) unname(x[,-1]))
percentage = t(do.call(cbind, percentage))
percentage = gsub("[^[:digit:].]", "", percentage)
percentage = apply(percentage, 2, as.numeric)
colnames(percentage) = paste(gsub("[[:blank:]]*", "", candidates), "%", sep = "")

### Create matrix of popular vote (row : county, col: candidate)
popular = lapply(states, function(state) {
  xmlSApply(xmlRoot(state), function(county) {
    county_cands = xmlSApply(county, function(x) xmlValue(x[[1]]))
    county_cands[1] = xmlValue(county[[1]][[3]])
    
    sapply(candidates, function(candidate) {
      cand_ind = which(county_cands == candidate)
      if (length(cand_ind) == 0) return(NA)
      else if (cand_ind == 1) return(xmlValue(county[[1]][[9]]))
      else return(xmlValue(county[[cand_ind]][[7]]))
    })
  })
})
popular = lapply(popular, function(x) unname(x[,-1]))
popular = t(do.call(cbind, popular))
popular = gsub("[^[:digit:]]", "", popular)
popular = apply(popular, 2, as.numeric)
colnames(popular) = paste(gsub("[[:blank:]]*", "", candidates), "_pop", sep = "")

### Merge matrices and create data frame
countyID = substring(countyID, 7, length(countyID))
votes = cbind(GEO.id2 = as.numeric(countyID), reporting, percentage, popular)
votes = data.frame(votes)



######################    Demographic Data    ######################

path = "http://www.stat.berkeley.edu/~nolan/data/Project2012/census2010/"
b = read.csv(paste(path, "B01003.csv", sep = ""))
d2 = read.csv(paste(path, "DP02.csv", sep = ""))
d3 = read.csv(paste(path, "DP03.csv", sep = ""))
names(d2)[-(1:5)] = sapply(names(d2)[-(1:5)], function(x) paste(x, ".2", sep = ""))
names(d3)[-(1:5)] = sapply(names(d3)[-(1:5)], function(x) paste(x, ".3", sep = ""))

bLab = read.csv(paste(path, "B01_metadata.txt", sep = ""), header = FALSE)
d2Lab = read.csv(paste(path, "DP02_metadata.txt", sep = ""), header = FALSE)
d3Lab = read.csv(paste(path, "DP03_metadata.txt", sep = ""), header = FALSE)

d2Lab[[1]][-(1:5)] = paste(d2Lab[[1]][-(1:5)], ".2", sep = "")
d3Lab[[1]][-(1:5)] = paste(d3Lab[[1]][-(1:5)], ".3", sep = "")








######################    Geographic Data    ######################


gml = xmlParse(file = "http://www.stat.berkeley.edu/users/nolan/data/Project2012/counties.gml")
root = xmlRoot(gml)

counties = xpathSApply(doc = gml, path = "//county/gml:name", fun = xmlValue)
X = xpathSApply(doc = gml, path = "//gml:X", fun = xmlValue)
Y = xpathSApply(doc = gml, path = "//gml:Y", fun = xmlValue)
geoStates = xpathSApply(doc = gml, path = "//state/gml:name", fun = xmlValue)
nCounties = xpathSApply(doc = gml, path = "//state", fun = xmlSize) - 1

counties = gsub(pattern = "\\n[[:blank:]]*", replacement = "", x = counties)
X = gsub(pattern = "\\n[[:blank:]]*", replacement = "", x = X)
Y = gsub(pattern = "\\n[[:blank:]]*", replacement = "", x = Y)
geoStates = tolower(gsub(pattern = "\\n[[:blank:]]*", replacement = "", x = geoStates))

### We want the counties in the format "County, State" so that they agree with the rest of the data
capitalize = function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}
geoStates = unname(sapply(geoStates, capitalize))
geoStates = rep(geoStates, times = nCounties)

gmlFrame = data.frame(GEO.display.label = paste(counties, geoStates, sep = ", "), X=X, Y=Y)















######################    Mash Data    ######################

### Turn total, white, and black population data into columns

total = which(b$POPGROUP.id == 1)
white = which(b$POPGROUP.id == 2)
black = which(b$POPGROUP.id == 4)

total = b[total,-c(4,5)]
names(total)[4:5] = c("Total.Pop", "Total.Pop.Error")

white = b[white, c(2,6,7)]
names(white)[2:3] = c("White.Pop", "White.Pop.Error")

black = b[black,c(2,6,7)]
names(black)[2:3] = c("Black.Pop", "Black.Pop.Error")

bFrame = merge(merge(total, white, all=TRUE), black, all=TRUE)

rm(black,white)  ### Collect garbage


### Remove redundant/unnecessary columns from d2 and d3 and merge data frames

dFrame = merge(d2[,-c(1,3,4,5)], d3[,-c(1,3,4,5)], by = "GEO.id2", all = TRUE)
frame = merge(bFrame, dFrame, by = "GEO.id2", all = TRUE)
frame = merge(votes, frame, by = "GEO.id2", all = TRUE)
frame = merge(gmlFrame, frame, by = "GEO.display.label", all = TRUE)

rm(gmlFrame, bFrame, dFrame, b, d2, d3, percentage,
   popular, total, votes, candidates, counties, countyID,
   geoStates, gml, nCounties, path, reporting, root,
   statenames, states, X, Y)    ### Collect garbage

### Split the county and state information into two separate columns

county_state = encodeString(frame[[1]])
county_state = gsub("\\\\x[[:alnum:]]{2}", "", county_state)
county_state = strsplit(county_state, split = ", ")
county_state = t(sapply(county_state, function(x) {
  if (length(x) == 1) return(c(NA,NA))
  else return(x)}))

frame$county = county_state[,1]
frame$state = county_state[,2]

rm(county_state)   ### Collect garbage

### Reorder columns.
### Order rows alphabetically according to state (automatically orders counties within
### state alphabetically as well).

n = ncol(frame)

frame = frame[order(frame$state),c(n-1,n,2:3,10,4:9,11:(n-2))]
names(frame)[8:11] = c("GOP", "Dem", "GOP_pop", "Dem_pop")

### Remove rows missing either geographic or voting information
frame = frame[apply(frame[,c(3:4, 8:11)], 1, function(x) any(is.na(x)) == FALSE),]
rownames(frame) = NULL




#################### Part 2 ###########################
## Get 2004 voting data into frame2

## First, load data into a dataframe, and change name of first column to coincide with frame2
Vote2004 = read.table("http://www.stat.berkeley.edu/~nolan/data/Project2012/countyVotes2004.txt",header=T)
names(Vote2004)[1] = 'county'

## Get a column for the states
Vote2004$state = sapply(Vote2004$county, function(x) gsub(",.*","",x))

## Clean up the county names
Vote2004$county = sapply(Vote2004$county, function(x) gsub(".*,","",x))

## Now, lets get the county names from frame2 to coincide with the county names from Vote2004

## First, make everything lower case:
frame$county = tolower(frame$county)
frame$state = tolower(frame$state)

## Next, remove the 'county' from each county name. Actually, not every county is labeled as county. Some are 'parish', or 'city'.
## So, we string split and get rid of the last word for every element. We use a helper variable, and then will assign it to frame2$county
helper = strsplit(frame$county,"[[:blank:]]")
helper = sapply(helper, function(x) 
  if(length(x)>=2){
    tolower(x[-length(x)])
  } else{
    tolower(x)
  })

## Now put it all togther, and double check those multi word names:
helper = sapply(helper,paste,collapse=" ")
helper = gsub("[[:punct:]]","",helper)
names(helper) = NULL




frame$county = helper
county=frame$county
frame$county = sapply(frame$county,function(x) gsub("[[:blank:]]","",x))
Vote2004$county = sapply(Vote2004$county,function(x) gsub("[[:blank:]]","",x))
frame$county[frame$county=='miamidade'] = c('dade')
Vote2004$county[Vote2004$county=='carsoncity'] = c('carson')



## Finally, we can merge:

frame = merge(frame, Vote2004)
frame = frame[order(frame$state),c(1:7,ncol(frame)-1, ncol(frame), 8:(ncol(frame)-2))]

frame$Winner04 = as.factor(c('Dem','GOP')[as.numeric(frame$bushVote > frame$kerryVote)+1])
frame$Winner12 = as.factor(c('Dem','GOP')[as.numeric(frame$GOP > frame$Dem)+1])

rm(Vote2004, county, helper)   ### Collect garbage


### Obtain a subset of the the original data frame containing only numeric vectors,
### since only these variables have a correlation with the winner

numframe = data.frame(sapply(frame, function(x) as.numeric(as.character(x))))
numframe = numframe[, colSums(is.na(numframe)) == 0]

win = frame$Winner04
win = as.character(win)
win = sapply(win, function(x) x == 'Dem')



#### Get rid of Margin of Errors #######

Marg3 = grep("(HC02_)|(HC04_)",d3Lab$V1)
Marg2 = grep("(HC02_)|(HC04_)",d2Lab$V1)

Variables3 = d3Lab$V1[-Marg3]
Variables2 = d2Lab$V1[-Marg2]

# Variables3 = sapply(Variables3, function(x) paste(x,".3",sep=""))
# Variables2 = sapply(Variables2, function(x) paste(x,".2",sep=""))

indices3 = unlist(sapply(Variables3, function(x) grep(x,names(numframe))))
indices2 = unlist(sapply(Variables2, function(x) grep(x,names(numframe))))

newFrame = numframe[,c(indices2,indices3)]



#num = sapply(frame, function(x) is.numeric(x)|is.logical(x))
#numframe = frame[,num]
#numframe$winner = NULL

### Find correlations and absolute correlations

correlations = sapply(newFrame, function(x) cor(win,x))
abs.correlations = abs(correlations)

### Find highest absolute correlations
ord = order(abs.correlations, decreasing = TRUE)
top.correlations = names(newFrame)[head(ord, 100)]


d2.variables = top.correlations[grep('\\.2', top.correlations)]
top.d2.variables = matrix(0,ncol=2,nrow=length(d2.variables))
for (k in 1:length(d2.variables)){
  if (length(d2Lab$V2[d2Lab$V1 == d2.variables[k]]) == 0){
    top.d2.variables[k,] = c(NA,NA)
  }else{
    top.d2.variables[k,] = c(d2Lab$V2[d2Lab$V1 == d2.variables[k]],
                             d2Lab$V1[d2Lab$V1 == d2.variables[k]])
  }
}
top.d2.variables = na.omit(top.d2.variables)


d3.variables = top.correlations[grep('\\.3', top.correlations)]
top.d3.variables = matrix(0,ncol=3,nrow=length(d3.variables))
for (k in 1:length(d3.variables)){
  if (length(d3Lab$V2[d3Lab$V1 == d3.variables[k]]) == 0){
    top.d3.variables[k,] = c(NA,NA,NA)
  }else{
    top.d3.variables[k,] = c(d3Lab$V2[d3Lab$V1 == d3.variables[k]],
                             d3Lab$V1[d3Lab$V1 == d3.variables[k]],
                             which(top.correlations == d3.variables[k]))
  }
}
top.d3.variables = na.omit(top.d3.variables)



### Normalize estimates and GML coordinates
frame$X = as.numeric(frame$X)
frame$Y = as.numeric(frame$Y)
normedFrame = frame
frame$Total.Pop = as.numeric(normedFrame$Total.Pop)


dLab = rbind(d2Lab, d3Lab)
estimates = dLab[grepl("Estimate", dLab[,2]), 1]
for(colname in estimates) {
  normedFrame[[colname]] = as.numeric(normedFrame[[colname]]) / frame$Total.Pop * 100
}
normedFrame$X = normedFrame$X / 10**6
normedFrame$Y = normedFrame$Y / 10**6


rm(newFrame, numframe, colname, estimates, indices2, indices3, ord, win)


###  Create matrix of predictor variables

vars2 = top.d2.variables[c(1,7,9,28,22,34),2]
vars3 = top.d3.variables[c(1,5,10,14,2),2]
vars = normedFrame[c('X', 'Y', vars2, vars3)]


getMissRate = function(preds) {
# This function returns the miss rates for a given matrix of knn predictions.
  pred12 = apply(pred12, 2, function(x) as.numeric(as.factor(x)))%%2
  results12 = as.numeric(as.factor(frame$Winner12))%%2
  missRate = numeric()
  for (i in 1:ncol(pred12)){
    assess = table(pred12[,i], results12)
    missRate = c(missRate, ((assess[1,2] + assess[2,1])/length(results12)))
  }
  return(missRate)
}


### Run a single trial of knn for k = 1:20
Knn = function(k = 1:20){sapply(k, function(k){
  knn.cv(train = vars, cl = frame$Winner04, k = k, prob = TRUE)
})}

pred12 = Knn()
missRate = getMissRate(pred12)

### Determine most successful k-value
best = which(missRate == min(missRate))
### Plot results
scatter.smooth(missRate, main = "Misclassification Rate \nvs.\n k-value",
               xlab = "k-value", ylab = "Misclassification Rate",
               col = c("green", "red"),
               pch = 19, cex = .8, axes = FALSE, bty = 'n', ylim = c(min(.1,missRate), max(.18,missRate)),
               span = 15/24, lpars = list(col = rgb(0, 0, 1, 0.5), lwd = 5))

axis(side = 1, at = 0:20)                                   # x-axis
axis(side = 2, at = seq(from = .1, to = .2, length = 11))         # y-axis

# best k-value
abline(v=best, lwd = 1.5, col = "dark green")
text(best, .25*min(missRate) + .75*max(missRate),
     labels = paste("k =",best), cex = .9, col = "dark green", pos = 4)

legend("bottomright", bty = 'n',
       c("Odd","Even", "Best k"), cex = .8, lwd = 2,
       lty = c(0,0,1), pch = c(19,19,NA), col = c("green","red", "dark green"))


### In predicting the results of the 2012 election,
### the most consistently successful k-value is 9.




# map(database = "county", xlim = c(-130,-60), ylim = c(20,50), fill = TRUE, boundary = FALSE, col = c(grey'))





# ks = numeric(0)

# for (i in 1:20){
# 
# ### Run a single trial of knn for k = 1:20
# Knn = function(k = 1:20){sapply(k, function(k){
#   knn.cv(train = vars, cl = frame$Winner04, k = k)
# })}
# 
# pred12 = Knn()
# missRate = getMissRate(pred12)
# 
# ### Determine most successful k-value
# best = which(missRate == min(missRate))
# ### Plot results
# scatter.smooth(missRate, main = "Misclassification Rate \nvs.\n Maximum Distance",
#                xlab = "Maximum Distance (k)", ylab = "Misclassification Rate",
#                col = c("green", "red"),
#                pch = 19, cex = .8, axes = FALSE, bty = 'n', ylim = c(min(.1,missRate), max(.18,missRate)),
#                span = 15/24, lpars = list(col = rgb(0, 0, 1, 0.5), lwd = 5))
# 
# axis(side = 1, at = 0:20)                                   # x-axis
# axis(side = 2, at = seq(from = .1, to = .2, length = 11))         # y-axis
# 
# # best k-value
# abline(v=best, lwd = 1.5, col = "dark green")
# text(best, .25*min(missRate) + .75*max(missRate),
#      labels = paste("k =",best), cex = .9, col = "dark green", pos = 4)
# 
# legend("bottomright", bty = 'n',
#        c("Odd","Even", "Best k"), cex = .8, lwd = 2,
#        lty = c(0,0,1), pch = c(19,19,NA), col = c("green","red", "dark green"))
# print(i)
# ks = c(ks, best)
# }
# 


# 
# length(winners)
# dim(mapcounties)
# a = sort(mapcounties$county)
# b = sort(merge(mapcounties, data, all.x = TRUE)$county)
# min(which(a != b))
# c$col = countycols
# c[which(c$state == "virginia"),]
# c[which(c$col == "blue"),]
# mapcounties[which(mapcounties$state == "virginia"),]
# dim(mapcounties[which(mapcounties$state == "virginia"),])
# dim(c[which(c$state == "virginia"),])
# 
# ### 2792 Windsor, Vermont
# ### 289 Newcastle, Delaware
# 
# map("county",col = c(countycols[1:2650],rep('white', 4000)), 
#     fill = TRUE,resolution = 0,lty = 0, projection = "polyconic")



