### YOUR NAME HERE
##Mengying Li

### Load HW6.rda and attach the XML library
library("XML")

### Part 1.  Create the data frame
### Look at the instructions in HW6.pdf.
### Functions you'll want to use: xmlParse(), xmlRoot(), xpathSApply(), xmlGetAttr().
### It also might make it easier to use: xmlToList(), merge().

### Load the data frame called LatLon from HW6.rda.  

### Parse the XML document at:
### http://www.stanford.edu/~vcs/StatData/factbook.xml
### and create an XML "tree" in R 

doc = xmlParse("http://www.stanford.edu/~vcs/StatData/factbook.xml")
root = xmlRoot(doc)
### Use XPath to extract the infant mortality and the CIA country codes from the XML tree
###   
### Create a data frame called IM using this XML file.
### The data frame should have 2 columns: for Infant Mortality and CIA.Codes.

### Extract the country populations from the same XML document
### Create a data frame called Pop using these data.
### This data frame should also have 2 columns, for Population and CIA.Codes.
infant_mortality = getNodeSet(doc,"/factbook//field[@name='Infant mortality rate']")
infant_number = xmlSApply(infant_mortality,xmlGetAttr,"number")
infant_country = xmlSApply(infant_mortality[[1]],xmlGetAttr,"country")
population = getNodeSet(doc,"/factbook//field[@name='Population']")
population_number = xmlSApply(population[[1]],xmlGetAttr,"number")
population_country = xmlSApply(population[[1]],xmlGetAttr,"country")
vec1 = as.numeric(unlist(infant_number[]))
vec2 = toupper(unlist(infant_country))
infant_data = data.frame("IM"=(vec1),CIA.Codes=(vec2))
vec1.1 = as.numeric(unlist(population_number))
vec2.1 = toupper(unlist(population_country))
population_data = data.frame("Pop"=vec1.1,CIA.Codes = vec2.1)
### Merge the two data frames to create a data frame called IMPop with 3 columns:
### IM, Pop, and CIA.Codes
IMPop = merge(infant_data, population_data, by = intersect(names(infant_data),names(population_data)))
### Now merge IMPop with LatLon (from HW8.rda) to create a data frame called AllData that has 6 columns
### for Latitude, Longitude, CIA.Codes, Country Name, Population, and Infant Mortality
AllData = merge(IMPop, LatLon, by = intersect(names(IMPop),names(LatLon)))
### Part 2.  Create a KML document
### Make the KML document described in HW6.pdf.  It should have the basic
### structure shown in that document.  You can use the addPlacemark function below to make
### the Placemark nodes, you just need to complete the line for the Point node and
### figure out how to use the function.


makeBaseDocument = function(){docKML = newXMLDoc()
                              root = newXMLNode("kml", doc = docKML)
                              doct = newXMLNode("Document", parent = root)
                              docKML
### This code creates the template KML document 



}
docKML = makeBaseDocument()
attach(AllData)


addPlacemark = function(lat, lon, ctryCode, ctryName, pop, infM, parent, 
                        inf1, pop1, style = FALSE)
{
  pm = newXMLNode("Placemark", 
                  newXMLNode("name", ctryName), attrs = c(id = ctryCode), 
                  parent = parent)
  newXMLNode("description", paste(ctryName, "\n Population: ", pop, 
                                  "\n Infant Mortality: ", infM, sep =""),
             parent = pm)
  
  newXMLNode("Point",newXMLNode("coordinates", paste(lon, lat, 0, sep=",")),parent = pm)
             
### You need to fill in the code for making the Point node above, including coordinates.
### The line below won't work until you've run the code for the next section to set up
### the styles.

}
for (i in 1:length(Country.Name)){addPlacemark(AllData[i,5],AllData[i,6],AllData[i,1],AllData[i,4],AllData[i,3],AllData[i,2],parent = getNodeSet(docKML,"/kml/Document"))}

### Save your KML document here, call it Part2.kml, and open it in Google Earth.
### (You will need to install Google Earth.)  
### It should have pushpins for all the countries.
saveXML(docKML, "Part2.kml")
### Part 3.  Add Style to your KML
### Now you are going to make the visualizatiion a bit fancier.  Pretty much all the code is given to you
### below to create style elements that are to be placed near the top of the document.
### These , you just need to figure out what it all does.

### Start fresh with a new KML document, by calling makeBaseDocument()
doc2 = makeBaseDocument()
### The following code is an example of how to create cut points for 
### different categories of infant mortality and population size.
### Figure out what cut points you want to use and modify the code to create these 
### categories.
infCut = cut(AllData[,2], breaks = quantile(AllData[,2], seq(0,1,length = 6)), include.lowest = TRUE) #c(0, 10, 25, 50, 75, 200)
infCut = as.numeric(infCut)
popCut = cut(log(AllData[,3]), breaks =quantile(log(AllData[,3]), seq(0,1,length = 6)), include.lowest = TRUE)
popCut = as.numeric(popCut)

### Now figure out how to add styles and placemarks to doc2
### You'll want to use the addPlacemark function with style = TRUE
addPlacemark = function(lat, lon, ctryCode, ctryName, pop, infM, parent, 
                        inf1, pop1, style = FALSE)
{
  pm = newXMLNode("Placemark", 
                  newXMLNode("name", ctryName), attrs = c(id = ctryCode), 
                  parent = parent)
  newXMLNode("description", paste(ctryName, "\n Population: ", pop, 
                                  "\n Infant Mortality: ", infM, sep =""),
             parent = pm)
  
  newXMLNode("Point",newXMLNode("coordinates", paste(lon, lat, 0, sep=",")),parent = pm)
  
  ### You need to fill in the code for making the Point node above, including coordinates.
  ### The line below won't work until you've run the code for the next section to set up
  ### the styles.
  
  if(style) newXMLNode("styleUrl", paste("#YOR", inf1, "-", pop1, sep = ''), parent = pm)
}
for (i in 1:length(Country.Name)){addPlacemark(AllData[i,5],AllData[i,6],AllData[i,1],AllData[i,4],AllData[i,3],AllData[i,2],parent = getNodeSet(doc2,"/kml/Document"),infCut[i],popCut[i],style = TRUE)}
### Below is code to make style nodes. 
### You should not need to do much to it.

### You do want to figure out what scales to you for the sizes of your circles
scales1 = c(0.5, 1, 3, 5, 10)
col = c("blue","green","yellow","orange","red")
addStyle = function(col1, pop1, parent, urlBase, scales = scales1)
{
  st = newXMLNode("Style", attrs = c("id" = paste("YOR", col1, "-", pop1, sep="")), parent = parent)
  newXMLNode("IconStyle", 
             newXMLNode("scale", scales[pop1]), 
             newXMLNode("Icon", paste(urlBase, "color_label_circle_", col[col1], ".png", sep ="")), parent = st)
  
}

for (popCut in 1:5)
{
  for (infCut in 1:5)
  {
    addStyle(infCut,popCut,getNodeSet(doc2,"/kml/Document"),'http://www.stanford.edu/~vcs/StatData/circles/')
  }
}


### You will need to figure out what order to call addStyle() and addPlacemark()
### so that the tree is built properly. You may need to adjust the code to call the png files

### Finally, save your KML document, call it Part3.kml and open it in Google Earth to 
### verify that it works.  For this assignment, you only need to submit your code, 
### nothing else.  You can assume that the grader has already loaded HW6.rda.
saveXML(doc2, "Part3.kml")
