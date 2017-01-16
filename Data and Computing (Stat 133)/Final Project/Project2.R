### Team Name: MMAD

### Members:
### Brooks, Marc
### Cheng, Yizhuang Alden
### Fenson, Derek
### Knopf, Michael


library("XML")
library("class")
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


### Remove redundant/unnecessary columns from d2 and d3 and merge data frames

dFrame = merge(d2[,-c(1,3,4,5)], d3[,-c(1,3,4,5)], by = "GEO.id2", all = TRUE)
frame = merge(bFrame, dFrame, by = "GEO.id2", all = TRUE)
frame = merge(votes, frame, by = "GEO.id2", all = TRUE)
frame = merge(gmlFrame, frame, by = "GEO.display.label", all = TRUE)


### Split the county and state information into two separate columns

county_state = encodeString(frame[[1]])
county_state = gsub("\\\\x[[:alnum:]]{2}", "", county_state)
county_state = strsplit(county_state, split = ", ")
county_state = t(sapply(county_state, function(x) {
  if (length(x) == 1) return(c(NA,NA))
  else return(x)}))

frame$county = county_state[,1]
frame$state = county_state[,2]

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
frame$mergeCol = sapply(sapply(strsplit(frame[[1]], " "), function(x) x[-length(x)]), function(x) do.call(paste, as.list(x)))
frame$mergeCol = tolower(gsub("[[:punct:][:blank:]]", "", frame$mergeCol))

votes04 = read.table("http://www.stat.berkeley.edu/~nolan/data/Project2012/countyVotes2004.txt", header = TRUE)
countyName = votes04[[1]]
county_state = data.frame(t(apply(sapply(strsplit(countyName, ","), function(x) x[2:1]), 1:2, capitalize)))
colnames(county_state) = c('county', 'state')
votes04 = data.frame(county_state, votes04[,-1])
votes04$county = tolower(gsub("[[:punct:][:blank:]]", "", votes04$county))
colnames(votes04)[1] = "mergeCol"
votes04$mergeCol[326] = "miamidade"
votes04$mergeCol[1676] = "carson"

frame = merge(frame, votes04, by = c("mergeCol", "state"))[,-1]

frame$winner04 = factor(apply(as.matrix(frame[c('bushVote', 'kerryVote')]), 1, function(x){
  if (x[1] > x[2]) return('GOP')
  else return('Dem')
}))

frame$winner12 = factor(apply(as.matrix(frame[c('GOP', 'Dem')]), 1, function(x){
  if (x[1] > x[2]) return('GOP')
  else return('Dem')
}))

frame = frame[order(frame$state), c(2:1,768:769,3:11,766:767,12:765)]

# different = sort(unique(c(which(is.na(frame3$GOP)), which(is.na(frame3$kerryVote)))))
# a = frame3[1:2][different,]
# a = a[a[,2] != "Virginia" & a[,2] != "Hawaii",]
# a[order(a[,2]),]

# vars = as.matrix(frame[c('X', 'Y')])


# a = knn(train = vars, test = vars, cl = frame$winner04, k = 1)








