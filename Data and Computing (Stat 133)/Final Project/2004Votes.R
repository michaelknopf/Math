## Get 2004 voting data into frame2

## First, load data into a dataframe, and change name of first column to coincide with frame2
Vote2004 = read.table("http://www.stat.berkeley.edu/~nolan/data/Project2012/countyVotes2004.txt",header=T)
names(Vote2004)[1] = 'county'
dim(Vote2004)
head(Vote2004)

## Get a column for the states
newState = sapply(Vote2004$county, function(x) gsub(",.*","",x))
Vote2004$state=newState
head(Vote2004)

## Clean up the county names
Vote2004$county = sapply(Vote2004$county, function(x) gsub(".*,","",x))

#Check it out
head(Vote2004)

## Now, lets get the county names from frame2 to coincide with the county names from Vote2004

## First, make everything lower case:
frame$county = tolower(frame$county)
frame$state = tolower(frame$state)
head(frame$county)
head(frame$state)

## Next, remove the 'county' from each county name. Actually, not every county is labeled as county. Some are 'parish', or 'city'.
## So, we string split and get rid of the last word for every element. We use a helper variable, and then will assign it to frame2$county
helper = frame$county
helper = strsplit(helper,"[[:blank:]]")
head(helper)

helper = sapply(helper, function(x) 
  if(length(x)>=2){
    tolower(x[-length(x)])
  } else{
    tolower(x)
  })
helper

## Now put it all togther, and double check those multi word names:
helper = sapply(helper,paste,collapse=" ")
helper = gsub("[[:punct:]]","",helper)
names(helper) = NULL
head(helper)




frame$county = helper
county=frame$county
frame$county = sapply(frame$county,function(x) gsub("[[:blank:]]","",x))
Vote2004$county = sapply(Vote2004$county,function(x) gsub("[[:blank:]]","",x))
frame$county[frame$county=='miamidade'] = c('dade')
Vote2004$county[Vote2004$county=='carsoncity'] = c('carson')



## Finally, we can merge:

newFrame = merge(frame, Vote2004)
dim(newFrame)
head(newFrame)

newFrame = newFrame[order(newFrame$state),]
dim(newFrame)


newFrame$Winner04 = as.numeric(newFrame$bushVote > newFrame$kerryVote)+1
newFrame$Winner04 = c('Dem','GOP')[newFrame$Winner04]
newFrame$Winner04 = as.factor(newFrame$Winner04)


newFrame$Winner12 = c('Dem','GOP')[as.numeric(newFrame$GOP > newFrame$Dem)+1]

### Obtain a subset of the the original data frame containing only numeric vectors,
### since only these variables have a correlation with the winner

numframe = data.frame(sapply(newFrame, function(x) as.numeric(as.character(x))))
numframe = numframe[, colSums(is.na(numframe)) == 0]

win = newFrame$Winner04
win = as.character(win)
win = sapply(win, function(x) if(x=='GOP'){
  return(0)
}else{
  return(1)
})


#### Get rid of Margin of Errors #######

Marg3 = grep("(HC02_)|(HC04_)",d3Lab$V1)
Marg2 = grep("(HC02_)|(HC04_)",d2Lab$V1)

Variables3 = d3Lab$V1[-Marg3]
Variables2 = d2Lab$V1[-Marg2]

Variables3 = sapply(Variables3, function(x) paste(x,".3",sep=""))
Variables2 = sapply(Variables2, function(x) paste(x,".2",sep=""))

indicies3 = unlist(sapply(Variables3, function(x) grep(x,names(numframe))))
indicies2 = unlist(sapply(Variables2, function(x) grep(x,names(numframe))))

newFrameTest = numframe[,c(indicies2,indicies3)]
head(newFrameTest)



#num = sapply(frame, function(x) is.numeric(x)|is.logical(x))
#numframe = frame[,num]
#numframe$winner = NULL

### Find correlations and absolute correlations

correlations = sapply(newFrameTest, function(x) cor(win,x))
abs.correlations = abs(correlations)

### Find highest absolute correlations
top.correlations = names(newFrameTest)[head(order(abs.correlations, decreasing = TRUE), 100)]

d2.variables = gsub('.2', '', top.correlations[grep('\\.2', top.correlations)])
top.d2.variables = matrix(0,ncol=2,nrow=length(d2.variables))
for (k in 1:length(d2.variables)){
  if (length(d2Lab$V2[d2Lab$V1 == d2.variables[k]]) == 0){
    top.d2.variables[k,] = c(NA,NA)
  }else{
    top.d2.variables[k,] = c(d2Lab$V2[d2Lab$V1 == d2.variables[k]],d2Lab$V1[which(d2Lab$V1 == d2.variables[k])])
  }
}
top.d2.variables = na.omit(top.d2.variables)

d3.variables = gsub('.3', '', top.correlations[grep('\\.3', top.correlations)])
top.d3.variables = matrix(0,ncol=2,nrow=length(d3.variables))
for (k in 1:length(d3.variables)){
  if (length(d3Lab$V2[d3Lab$V1 == d3.variables[k]]) == 0){
    top.d3.variables[k,] = c(NA,NA)
  }else{
    top.d3.variables[k,] = c(d3Lab$V2[d3Lab$V1 == d3.variables[k]],d3Lab$V1[which(d3Lab$V1 == d3.variables[k])])
  }
}
top.d3.variables = na.omit(top.d3.variables)

