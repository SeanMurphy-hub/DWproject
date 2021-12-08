###### Other links:
# (https://www.kaggle.com/debdutta/cost-of-living-index-by-country)
# (https://www.numbeo.com/cost-of-living/rankings.jsp)

#install.packages('stringr')
library(stringr)
rm(list=ls())

# (https://www.kaggle.com/rishidamarla/sociological-metrics-of-all-50-states)
# Sociological Data File (from Kaggle ['data.csv'])
# Features:
# State [chr]
# Percent.Educational.Attainment [num]
# Percent.Peace.Index [num]
# Percent.Above.Poverty.Rate [num]
# Percent.Non.religious [int]
socioData <- read.csv("data.csv")



##### Code that scrapes all the data and turns it into a data frame in R:
library(xml2)
page<-read_html("https://www.numbeo.com/cost-of-living/rankings.jsp?title=2021-mid") 
class(page) # Result is class xml_document
page # Print HTML to console

##scrape the main table on the site
city <- xml_text(xml_find_all(page, "//td[@class='cityOrCountryInIndicesTable']"))

cli<-xml_text(xml_find_all(page,"//tbody/tr/td[3]"))

RentIndex<-xml_text(xml_find_all(page,"//tbody/tr/td[4]"))

cliPlusRentIndex <- xml_text(xml_find_all(page,"//tbody/tr/td[5]"))

GroceriesIndex <- xml_text(xml_find_all(page,"//tbody/tr/td[6]"))

RestaurantPriceIndex <- xml_text(xml_find_all(page,"//tbody/tr/td[7]"))

LocalPurchasingPowerIndex <-xml_text(xml_find_all(page,"//tbody/tr/td[8]"))

ID <- c(1:563)

#create data frame
indexes <- data.frame(ID, city, cli,RentIndex,cliPlusRentIndex,GroceriesIndex,RestaurantPriceIndex,LocalPurchasingPowerIndex)

#Filter out the foreign cities
us_indexes<-subset(indexes,grepl('United States',indexes$city)==T)
us_indexes$city<-gsub(", United States","",us_indexes$city)

#split up the city and state into separate columns
us_indexes$state <- c(NA) 
cityst <- str_split_fixed(us_indexes$city, ', ',2)
us_indexes$state <- cityst[,2]
us_indexes$city <- cityst[,1]

#transform name of the states in sociodata to match the state abbreviations in us_indexes
socioData$State <- state.abb[match(socioData$State,state.name)]

#merge the sociodata with us_indexes
mergeddata <- merge(us_indexes,socioData, by.x = 'state',by.y = 'State', all.x =T )

################# Analysis Questions: #################

# (1) - Which region of the U.S. saw cities with the highest population increase and what are those cities' average cost of living index?
# How does that compare to cities with population decreases?

# (2) - Are there any relationships between population change and the cost of housing?

# (3) - Did cities in states with decreasing populations have a lower educational achievement then those with higher educational achievement?
# Are there any relationships that can be extracted from educational achievement and other sociological factors?

# (4) - What are the top three cities that have the highest and lowest local purchasing power and what are those cities' state crime index?
