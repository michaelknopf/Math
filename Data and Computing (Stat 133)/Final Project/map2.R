library(maps)
library(mapproj)
library(ggplot2)

# load("data2.rda")

mapcounties = strsplit(map('county', plot = FALSE)$name, ',', )
mapcounties = data.frame(t(sapply(mapcounties, function(x) x)))
names(mapcounties) = c('state', 'county')
data = frame[,c('state', 'county', 'Winner12')]

data$Winner12 = rp.predictions

mapcounties$statecounty = paste(mapcounties$state, mapcounties$county, sep = "")
mapcounties$statecounty = gsub("[[:blank:][:punct:]]", "", mapcounties$statecounty)
data$statecounty = paste(data$state, data$county, sep = "")
data$statecounty = gsub("[[:blank:]]", "", data$statecounty)

mapcounties = data.frame(mapcounties[,3])
names(mapcounties) = "statecounty"
data = data[,4:3]

mapcounties[1162,1] = "marylandbaltimorecity"
data[1549,1] = "missouristlouiscity"


# This is just for examination, never used
m = merge(mapcounties, data, all.x = TRUE)
#

winners = merge(mapcounties, data, all.x = TRUE)[[2]]
countycols = as.factor(as.numeric(as.factor(winners)) %% 2)
levels(countycols) = c('red', 'blue')
countycols = as.character(countycols)
countycols[is.na(countycols)] = "grey"
countycols[2792] = "grey"


map("county", col = countycols, bg = 'black',
    fill = TRUE,resolution = 0,lty = 0, projection = "polyconic")
map("state",col = "white",fill=FALSE,add=TRUE,lty=1,lwd=2,projection="polyconic")

m[which(is.na(m$Winner12)),]




