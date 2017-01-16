### Load HW7.rda and attach the XML library
### Knopf, Michael

library("XML")
options(stringsAsFactors = FALSE)

### Part 1.  Create the data frame
### Look at the instructions in HW7.pdf.
### Functions you'll want to use: xmlParse(), xmlRoot(), xpathSApply(), xmlGetAttr().
### It also might make it easier to use: xmlToList(), merge().

### Load the data frame called LatLon from HW7.rda.  
load("HW7.rda")

## Parse the XML document at:
### http://www.stat.berkeley.edu/users/nolan/stat133/data/factbook.xml
### and create an XML "tree" in R 
fact = xmlParse("http://www.stat.berkeley.edu/users/nolan/stat133/data/factbook.xml")

### Use XPath to extract the infant mortality and the CIA country codes from the XML tree
###

IMrates = as.numeric(xpathSApply(doc = fact, path = "//*[@name='Infant mortality rate']/rank",
                                 function(node) xmlGetAttr(node = node, name = "number")))

IMcountries = as.character(xpathSApply(doc = fact, path = "//*[@name='Infant mortality rate']/rank",
                                       function(node) xmlGetAttr(node = node, name = "country")))

# imSet = getNodeSet(doc = fact, path = "//*[@name='Infant mortality rate']")[[1]]
# IMrates = as.numeric(xmlApply(imSet, function(node) xmlGetAttr(node = node, name = "number"))[-1])
# IMcountries = as.character(xmlApply(imSet, function(node) xmlGetAttr(node = node, name = "country"))[-1])

### Create a data frame called IM using this XML file.
### The data frame should have 2 columns: for Infant Mortality and CIA.Codes.

IM = data.frame(CIA.Codes = IMcountries, InfantMortality = IMrates)

### Extract the country populations from the same XML document
### Create a data frame called Pop using these data.
### This data frame should also have 2 columns, for Population and CIA.Codes.

popRates = as.numeric(xpathSApply(doc = fact, path = "//*[@name='Population']/rank",
                                  function(node) xmlGetAttr(node = node, name = "number")))

popCountries = as.character(xpathSApply(doc = fact, path = "//*[@name='Population']/rank",
                                  function(node) xmlGetAttr(node = node, name = "country")))

# popSet = getNodeSet(doc = fact, path = "//*[@name='Population']")[[1]]
# popRates = as.numeric(xmlApply(popSet, function(node) xmlGetAttr(node = node, name = "number"))[-1])
# popCountries = as.character(xmlApply(popSet, function(node) xmlGetAttr(node = node, name = "country"))[-1])

Pop = data.frame(CIA.Codes = popCountries, Population = popRates)

### Merge the two data frames to create a data frame called IMPop with 3 columns:
### IM, Pop, and CIA.Codes

IMpop = merge(x = IM, y = Pop, all = TRUE)

### Now merge IMPop with LatLon (from HW7.rda) to create a data frame called AllData that has 6 columns
### for Latitude, Longitude, CIA.Codes, Country Name, Population, and Infant Mortality

IMpop$CIA.Codes = toupper(IMpop$CIA.Codes)
AllData = na.omit(merge(LatLon, IMpop, all = TRUE))
rownames(AllData) = 1:nrow(AllData)

### Part 2.  Create a KML document
### Make the KML document described in HW7.pdf.  It should have the basic
### structure shown in that document.  You can use the addPlacemark function below to make
### the Placemark nodes, you just need to complete the line for the Point node and
### figure out how to use the function.

makeBaseDocument = function(){
### This code creates the template KML document 
  kmlDoc <<- newXMLDoc()
  kmlRoot <<- newXMLNode("kml", doc = kmlDoc)
  DocN <<- newXMLNode("Document", parent = kmlRoot)
  newXMLNode("name", "Country Facts", parent = DocN)
#   newXMLNode(name = "Description", "Infant Mortality")
  apply(AllData, 1, function(x) addPlacemark(x[3], x[4], x[1], x[2], x[6], x[5], DocN))
  saveXML(kmlDoc, file = "Part2.kml")
}

addPlacemark = function(lat, lon, ctryCode, ctryName, pop, infM, parent)
{
  pm = newXMLNode("Placemark", 
                  newXMLNode("name", ctryName), attrs = c(id = ctryCode), 
                  parent = parent)
  newXMLNode("description", paste(ctryName, "\n Population: ", pop, 
                                  "\n Infant Mortality: ", infM, sep =""),
             parent = pm)

  newXMLNode("Point", newXMLNode("coordinates", paste(lon,lat,"0", sep = ",")), parent = pm)
             
### You need to fill in the code for making the Point node above, 
### including coordinates.

}

makeBaseDocument()


### Save your KML document here, call it Part2.kml, and open it in Google Earth.
### (You will need to install Google Earth.)  
### It should have pushpins for all the countries.



### For this assignment, you only need to submit your code, 
### nothing else.  You can assume that the grader has already loaded HW7.rda.
