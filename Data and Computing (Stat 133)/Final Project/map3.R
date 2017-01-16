library(maps)
library(mapproj)
library(ggplot2)

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
