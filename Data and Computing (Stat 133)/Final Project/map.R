library(maps)
library(mapproj)
library(ggplot2)

# load("data.rda")

mapcounties = strsplit(map('county', plot = FALSE)$name, ',', )
mapcounties = data.frame(t(sapply(mapcounties, function(x) x)))
names(mapcounties) = c('state', 'county')
data = frame[,c('county', 'state', 'Winner12', 'Total.Pop')]

mapcounties$statecounty = paste(mapcounties$state, mapcounties$county, sep = "")
mapcounties$statecounty = gsub("[[:blank:]]", "", mapcounties$statecounty)
data$statecounty = paste(data$state, data$county, sep = "")
data$statecounty = gsub("[[:blank:]]", "", data$statecounty)

mapcounties = mapcounties[,3]
data = data[,4:3]

mapcounties[1162] = "marylandbaltimorecity"
data[1549,1] = "missouristlouiscity"

# This is just for examination, never used
m = merge(mapcounties, data, all.x = TRUE)
#

winners = merge(mapcounties, data, all.x = TRUE)[[3]]
countycols = as.factor(as.numeric(winners) %% 2)
levels(countycols) = c('red', 'blue')
countycols = as.character(countycols)
countycols[is.na(countycols)] = "grey"
countycols[2792] = "grey"


map("county",col = countycols,
    fill = TRUE,resolution = 0,lty = 0, projection = "polyconic")
map("state",col = "white",fill=FALSE,add=TRUE,lty=1,lwd=2,projection="polyconic")

c = c[order(paste(mapcounties$state, mapcounties$county, sep = "")),]

d = cbind(mapcounties,c)
names(d)[1:4] = c("state.m", "county.m", "state.c", "county.c")
head(d[which(d$county.m != d$county.c),],100)






# cols = c('red', rep('blue', 49))
# cols = cols[c(2:50,1)]
# 
# map("state",col = cols, 
#     fill = TRUE,resolution = 0,lty = 0, projection = "polyconic")
# map("state",col = "white",fill=FALSE,add=TRUE,lty=1,lwd=2,projection="polyconic")

