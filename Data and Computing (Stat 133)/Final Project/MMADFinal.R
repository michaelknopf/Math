#################### Part 2 ###########################
### Team Name: MMAD

### Members:
### Brooks, Marc
### Cheng, Yizhuang Alden
### Fenson, Derek
### Knopf, Michael


require(XML)
require(class)
require(maps)
require(rpart)
require(ggplot2)
require(rpart.plot)
require(RColorBrewer)
require(mapproj)
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


#################### Part 3 ###########################
## Merge 2004 voting data into frame

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
frame$county = sapply(frame$county,function(x) gsub("[[:blank:]]","",x))
Vote2004$county = sapply(Vote2004$county,function(x) gsub("[[:blank:]]","",x))
frame$county[frame$county=='miamidade'] = c('dade')
Vote2004$county[Vote2004$county=='carsoncity'] = c('carson')

## Finally, we can merge:

frame = merge(frame, Vote2004)
frame = frame[order(frame$state),c(1:7,ncol(frame)-1, ncol(frame), 8:(ncol(frame)-2))]

frame$Winner04 = as.factor(c('Dem','GOP')[as.numeric(frame$bushVote > frame$kerryVote)+1])
frame$Winner12 = as.factor(c('Dem','GOP')[as.numeric(frame$GOP > frame$Dem)+1])

rm(Vote2004, helper)   ### Collect garbage


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

indices3 = unlist(sapply(Variables3, function(x) grep(x,names(numframe))))
indices2 = unlist(sapply(Variables2, function(x) grep(x,names(numframe))))

numframe = numframe[,c(indices2,indices3)]

### Find correlations and absolute correlations

correlations = sapply(numframe, function(x) cor(win,x))
abs.correlations = abs(correlations)

### Find highest absolute correlations
ord = order(abs.correlations, decreasing = TRUE)
top.correlations = names(numframe)[head(ord, 100)]

### Find the highest absolute correlated variables in d2 and d3 respectively

d2.variables = top.correlations[grep('\\.2', top.correlations)]
top.d2.variables = matrix(0,ncol=3,nrow=length(d2.variables))
for (k in 1:length(d2.variables)){
  if (length(d2Lab$V2[d2Lab$V1 == d2.variables[k]]) == 0){
    top.d2.variables[k,] = c(NA,NA,NA)
  }else{
    top.d2.variables[k,] = c(d2Lab$V2[d2Lab$V1 == d2.variables[k]],
                             d2Lab$V1[d2Lab$V1 == d2.variables[k]],
                             which(top.correlations == d2.variables[k]))
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

########## Part 3a ##############

set.seed(222222222)

######### Rpart ##############


### Function to extract correct names #####
where2 = c(1,8,30,24,37,34,10)
where3 = c(1,2,7,13,18,3)

getIndex = function(where2,where3){
  for3 = as.numeric(top.d3.variables[where3,3])
  for2 = as.numeric(top.d2.variables[where2,3])
  index = top.correlations[sort(c(for2,for3))]
  return(index)
}

makeTree = function(where2,where3){
  index = getIndex(where2,where3)
  where = sapply(index, function(x) which(names(frame)==x))
  names(frame)[where] = LETTERS[1:length(where)]
  form = as.formula(paste("Winner04","~",paste(names(frame)[where],collapse="+")))
  fit = rpart(form, data = frame)
  prp(fit,uniform=T,fallen.leaves=T,type=2, yesno=T,extra = 2, box.col = c(adjustcolor('blue',0.7),adjustcolor('red',0.7))[fit$frame$yval], branch.col='black')
  return(fit)
}

fit = makeTree(where2,where3)

### Rpart predictions and error rates 
rp.predictions = as.factor(predict(fit, type = 'vector'))
assess = table(rp.predictions, frame$Winner12)
error.rate.rpart = (assess[1,2] + assess[2,1])/length(rp.predictions)

######### Part 3b ############

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


rm(numframe, colname, estimates, indices2, indices3, ord, win) #garbage pick up


###  Create matrix of predictor variables

vars2 = top.d2.variables[where2,2]
vars3 = top.d3.variables[where3,2]
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

pred12 = pred12[,5]
### In predicting the results of the 2012 election,
### the most consistently successful k-value is 9.

########## Part 4a ###########

###################### PLOTS   ##########################################

###################### Regional Plots ##################################

### rpart and knn side by side ###

## We first sort the states by region:

Pacific = grep("(california)|(washington)|(oregon)",frame$state)
SouthWest = grep("(arizona)|(texas)|(new mexico)|(oklahoma)",frame$state)
RockyMtn = grep("(idaho)|(montana)|(wyoming)|(colorado)|(nevada)|(utah)",frame$state)
MidWest = grep("(north dakota)|(south dakota)|(nebraska)|^(kansas)|(minnesota)|(iowa)|(missouri)|(wisconsin)|(illinois)|(michigan)|(indiana)|(ohio)",frame$state)
NorthEast = grep("(maine)|(new hampshire)|(vermont)|(new york)|(rhode island)|(massachusetts)|(connecticut)|(pennsylvania)|(new jersey)",frame$state)
SouthEast = (1:nrow(frame))[-c(Pacific,SouthWest,RockyMtn,MidWest,NorthEast,SouthWest)]

## We then get the indicies for each region into a list to use later:
groups = list(Pacific = Pacific,SouthWest = SouthWest,RockyMtn = RockyMtn,MidWest = MidWest,NorthEast = NorthEast,SouthEast = SouthEast)


## The following functions are generic to either knn or rpart; they take a prediction (pred) and an index (x), and produce an error rate based on the true winner:
errors.dem = function(pred,x){
  assess = table(pred[x],frame$Winner12[x])
  return(assess[1,2]/length(x))
}

errors.gop = function(pred,x){
  assess = table(pred[x],frame$Winner12[x])
  return(assess[2,1]/length(x))
}

## We then get all the error rates based on region:
tot.errors.dem = sapply(groups, function(x) errors.dem(pred=rp.predictions,x))
tot.errors.gop = sapply(groups, function(x) errors.gop(pred=rp.predictions,x))

## We are using ggplot; so, we need so put our data into a data.frame, with a factor as the regions:
newgroups = factor(rep(names(groups),times=2))
levels(newgroups) = names(groups)
newgroups = newgroups[c(order(as.numeric(newgroups)[1:6]),order(as.numeric(newgroups)[7:12]))]

tot.errors =  data.frame(Regions = newgroups, errors = c(tot.errors.gop,tot.errors.dem), party = rep(c('GOP','Dem'), each=6))

## We can now finally plot our bar plots using ggplot:

## Rpart plot ##

error.bar = ggplot(data = tot.errors, aes(x=Regions,y=errors,fill=factor(party)))+
  geom_bar(stat="identity",position="dodge") +
  scale_fill_manual(values = c(brewer.pal(3,'Set1')[2],brewer.pal(3,'Set1')[1])) + 
  theme(axis.line = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank()) +
  ylab('Error Rate') +
  theme(axis.title.y = element_text(size=15)) +
  ggtitle('RPart Prediction Error') + 
  theme(plot.title = element_text(lineheight=.8, face="bold", vjust = -4)) + 
  theme(plot.title = element_text(size=25)) + 
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=15)) +
  theme(legend.position = c(0.94,0.55), legend.background = element_blank())

## knn plot ##

tot.errors.gop.knn = sapply(groups, function(x) errors.gop(pred = pred12,x))
tot.errors.dem.knn = sapply(groups, function(x) errors.dem(pred = pred12,x))

tot.errors.knn =  data.frame(Regions = newgroups, errors = c(tot.errors.gop.knn,tot.errors.dem.knn), party = rep(c('GOP','Dem'), each=6))

error.bar.knn = ggplot(data = tot.errors.knn, aes(x=Regions,y=errors,fill=factor(party)))+
  geom_bar(stat="identity",position="dodge") +
  scale_fill_manual(values = c(brewer.pal(3,'Set1')[2],brewer.pal(3,'Set1')[1])) + 
  theme(axis.line = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank()) +
  ylab('Error Rate') +
  theme(axis.title.y = element_text(size=15)) +
  ggtitle('KNN Prediction Error') + 
  theme(plot.title = element_text(lineheight=.8, face="bold", vjust = -4)) + 
  theme(plot.title = element_text(size=25)) + 
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=15)) +
  theme(legend.position = c(0.94,0.55), legend.background = element_blank())

## The following function was taken from Winston Chang's Cookbook for R website. We only use it to plot side by side in ggplot.

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# This code produces our plot. We leave it as a comment:
multiplot(error.bar, error.bar.knn, cols=2)






#############################################


######### Fancy Plot 1 ##########

prop_04 = sapply(1:nrow(frame), function(x)
                      frame$bushVote[x]/(frame$bushVote[x]+frame$kerryVote[x]))

prop_12 = sapply(1:nrow(frame), function(x)
                      frame$GOP_pop[x]/(frame$GOP_pop[x]+frame$Dem_pop[x]))

prop_change = prop_12 - prop_04

## add 0.6 to the magnitude change for our plotting character size
mag_change= abs(prop_change)+0.6

## Next, we make a function that defines color for each plotting character
makecolor = function(prop_change){
  if(prop_change > 0){
    col = rgb(1,0,0,1)
  } else {
    col = rgb(0,0,1,1)
  }
  return(col)
}

## Use makecolor to get a vector of colors:
col = sapply(c(1:nrow(frame)), function(x){
  makecolor(prop_change[x])})

## get the USA mape based on states:
all_states=map_data("state")

## Normalize the gml coordinates

frame$X = as.numeric(frame$X)/1000000
frame$Y = as.numeric(frame$Y)/1000000

## Function that defines swing states, to use to color them blue later:
helpStuff = sapply(all_states$region, function(x)
  if(x=='ohio' || x=='new mexico' || x=='nevada' || x=='colorado' || x=='iowa' || x=='florida'){
    1
  } else {
    0
  }
)

## Plotting time:

fancyPlot1 = ggplot() + 
  geom_polygon(data = all_states, aes(x=long, y=lat, group = group, fill = c("blue","grey")[helpStuff+1],colour='white'),
               colour="white", projection = 'polyconic') + 
  coord_map(projection = 'polyconic') + 
  geom_point(data=frame, aes(x=X, y=Y, size=mag_change,colour=col), pch=19, position = position_jitter(0.1,0.1)) + 
  scale_colour_manual(values =  col) + scale_fill_manual(values = c('grey',adjustcolor('blue',0.5))) + 
  theme(legend.position="none") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_blank(), 
        axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        panel.background = element_blank())
print(fancyPlot1)

######### Fancy Plot 2 ########### 

mapcounties = strsplit(map('county', plot = FALSE)$name, ',', )
mapcounties = data.frame(t(sapply(mapcounties, function(x) x)))
names(mapcounties) = c('state', 'county')
data = frame[,c('state', 'county', 'Winner12', 'Total.Pop')]

mapcounties$statecounty = paste(mapcounties$state, mapcounties$county, sep = "")
mapcounties$statecounty = gsub("[[:blank:][:punct:]]", "", mapcounties$statecounty)
data$statecounty = paste(data$state, data$county, sep = "")
data$statecounty = gsub("[[:blank:]]", "", data$statecounty)

mapcounties = data.frame(mapcounties[,3])
names(mapcounties) = "statecounty"
data = data[,c(5,3,4)]

mapcounties[1162,1] = "marylandbaltimorecity"
data[1549,1] = "missouristlouiscity"

winners = merge(mapcounties, data, all.x = TRUE)[[2]]
countycols = as.factor(as.numeric(winners) %% 2)
levels(countycols) = c('red', 'blue')
countycols = as.character(countycols)
countycols[is.na(countycols)] = "grey"
countycols[2792] = "grey"


map("county", col = countycols, bg = 'white',
    fill = TRUE,resolution = 0,lty = 0, projection = "polyconic")
map("state",col = "white",fill=FALSE,add=TRUE,lty=1,lwd=2,projection="polyconic")
legend("bottomleft", box.col = "grey", fill = c('red','blue'),
       legend = c('GOP','Democrat'), bty = 'n',
       cex = 0.7, text.width = .1)






# ##### Plots by unemployment quartiles #######
# 
# ### rpart ###
# unemploy = summary(frame$HC03_VC13.3)
# unemploy.q1 = which(frame$HC03_VC13.3 < unemploy['1st Qu.'])
# unemploy.q2 = which(frame$HC03_VC13.3 >= unemploy['1st Qu.']
#                     & frame$HC03_VC13.3 < unemploy['Median'])
# unemploy.q3 = which(frame$HC03_VC13.3 >= unemploy['Median']
#                     & frame$HC03_VC13.3 < unemploy['3rd Qu.'])
# unemploy.q4 = which(frame$HC03_VC13.3 >= unemploy['3rd Qu.'])
# 
# unemploy.quartiles = list(unemploy.q1, unemploy.q2,
#                           unemploy.q3, unemploy.q4)
# 
# unemploy.errors.bush = sapply(unemploy.quartiles,
#                               function(x) errors.gop(rp.predictions,x))
# 
# unemploy.errors.kerry = sapply(unemploy.quartiles,
#                                function(x) errors.dem(rp.predictions,x))
# 
# unemploy.error.types = matrix(c(unemploy.errors.bush,unemploy.errors.kerry),
#                               byrow = TRUE, nrow = 2)
# colnames(unemploy.error.types) = c('1st Quartile', '2nd Quartile', '3rd Quartile', '4th Quartile')
# barplot(unemploy.error.types, beside = TRUE, col = c('red','blue'),
#         main = 'Error rates for Counties by Unemployment Quartile (rpart)',
#         xlab = 'Unemployment Quartile', ylab = 'Error Rate')
# legend('topright', fill = c('red','blue'),
#        legend = c('Error in Favor of Bush','Error in Favor of Kerry'),
#        bty = 'n', cex = 0.7)
# 
# ### Knn ###
# unemploy.errors.bush.knn = sapply(unemploy.quartiles,
#                                   function(x) errors.gop(pred12,x))
# 
# unemploy.errors.kerry.knn = sapply(unemploy.quartiles,
#                                    function(x) errors.dem(pred12,x))
# 
# unemploy.error.types.knn = matrix(c(unemploy.errors.bush.knn,unemploy.errors.kerry.knn),
#                                   byrow = TRUE, nrow = 2)
# colnames(unemploy.error.types.knn) = c('1st Quartile', '2nd Quartile',
#                                        '3rd Quartile', '4th Quartile')
# barplot(unemploy.error.types.knn, beside = TRUE, col = c('red','blue'),
#         main = 'Error rates for Counties by Unemployment Quartile (knn)',
#         xlab = 'Unemployment Quartile', ylab = 'Error Rate')
# legend('topright', fill = c('red','blue'),
#        legend = c('Error in Favor of Bush','Error in Favor of Kerry'),
#        bty = 'n', cex = 0.7)