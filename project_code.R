###### Other links:
# (https://www.kaggle.com/debdutta/cost-of-living-index-by-country)
# (https://www.numbeo.com/cost-of-living/rankings.jsp)

#install.packages('stringr')
library(stringr)
rm(list=ls())

# (https://www.kaggle.com/rishidamarla/sociological-metrics-of-all-50-states)
# Sociological Data File (from Kaggle ['data.csv'])
socioData <- read.csv("data.csv")

##### Code that scrapes all the data and turns it into a data frame in R:
library(xml2)
page<-read_html("https://www.numbeo.com/cost-of-living/rankings.jsp?title=2021-mid") 
class(page) # Result is class xml_document
page # Print HTML to console

##scrape the main table on the site
city <- xml_text(xml_find_all(page, "//td[@class='cityOrCountryInIndicesTable']"))

cli<-xml_text(xml_find_all(page,"//tbody/tr/td[3]"))
cli <- as.numeric(cli)

RentIndex<-xml_text(xml_find_all(page,"//tbody/tr/td[4]"))
RentIndex <- as.numeric(RentIndex)

cliPlusRentIndex <- xml_text(xml_find_all(page,"//tbody/tr/td[5]"))
cliPlusRentIndex <- as.numeric(cliPlusRentIndex)

GroceriesIndex <- xml_text(xml_find_all(page,"//tbody/tr/td[6]"))
GroceriesIndex <- as.numeric(GroceriesIndex)

RestaurantPriceIndex <- xml_text(xml_find_all(page,"//tbody/tr/td[7]"))
RestaurantPriceIndex <- as.numeric(RestaurantPriceIndex)

LocalPurchasingPowerIndex <-xml_text(xml_find_all(page,"//tbody/tr/td[8]"))
LocalPurchasingPowerIndex <- as.numeric(LocalPurchasingPowerIndex)

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

# Create a 'Region' Factor column
# According to U.S. Census (https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf)
mergeddata$Region <- as.factor(mergeddata$state) #Copy of state column
# Merge/Condense levels
levels(mergeddata$Region)[levels(mergeddata$Region) == "AK" |
                            levels(mergeddata$Region) == "HI" |
                            levels(mergeddata$Region) == "CA" |
                            levels(mergeddata$Region) == "OR" |
                            levels(mergeddata$Region) == "WA"] <- "Pacific West"

levels(mergeddata$Region)[levels(mergeddata$Region) == "AZ" |
                            levels(mergeddata$Region) == "CO" |
                            levels(mergeddata$Region) == "ID" |
                            levels(mergeddata$Region) == "NM" |
                            levels(mergeddata$Region) == "UT" |
                            levels(mergeddata$Region) == "NV"] <- "Mountain West"

levels(mergeddata$Region)[levels(mergeddata$Region) == "AR" |
                            levels(mergeddata$Region) == "LA" |
                            levels(mergeddata$Region) == "OK" |
                            levels(mergeddata$Region) == "TX"] <- "West South Central"

levels(mergeddata$Region)[levels(mergeddata$Region) == "AL" |
                            levels(mergeddata$Region) == "KY" |
                            levels(mergeddata$Region) == "TN"] <- "East South Central"

levels(mergeddata$Region)[levels(mergeddata$Region) == "DC" |
                            levels(mergeddata$Region) == "FL" |
                            levels(mergeddata$Region) == "GA" |
                            levels(mergeddata$Region) == "MD" |
                            levels(mergeddata$Region) == "NC" |
                            levels(mergeddata$Region) == "SC" |
                            levels(mergeddata$Region) == "VA"] <- "South Atlantic"

levels(mergeddata$Region)[levels(mergeddata$Region) == "IA" |
                            levels(mergeddata$Region) == "KS" |
                            levels(mergeddata$Region) == "MN" |
                            levels(mergeddata$Region) == "MO" |
                            levels(mergeddata$Region) == "NE"] <- "West North Central"

levels(mergeddata$Region)[levels(mergeddata$Region) == "IN" |
                            levels(mergeddata$Region) == "IL" |
                            levels(mergeddata$Region) == "MI" |
                            levels(mergeddata$Region) == "OH" |
                            levels(mergeddata$Region) == "WI"] <- "East North Central"

levels(mergeddata$Region)[levels(mergeddata$Region) == "NJ" |
                            levels(mergeddata$Region) == "NY" |
                            levels(mergeddata$Region) == "PA"] <- "Middle Atlantic"

levels(mergeddata$Region)[levels(mergeddata$Region) == "MA"] <- "New England"

######## Change appropriate columns to numeric ########
mergeddata$cli <- as.numeric(mergeddata$cli)
mergeddata$RentIndex <- as.numeric(mergeddata$RentIndex)
mergeddata$cliPlusRentIndex <- as.numeric(mergeddata$cliPlusRentIndex)
mergeddata$GroceriesIndex <- as.numeric(mergeddata$GroceriesIndex)
mergeddata$RestaurantPriceIndex <- as.numeric(mergeddata$RestaurantPriceIndex)
mergeddata$LocalPurchasingPowerIndex <- as.numeric(mergeddata$LocalPurchasingPowerIndex)

################# Analysis Questions: #################

# (1) - Which region of the U.S. saw cities with the highest population increase and what are those cities' average cost of living index?
# How does that compare to cities with population decreases?

# (2) - Are there any relationships between population change and the cost of housing?

# (3) - Did cities in states with decreasing populations have a lower educational achievement then those with higher educational achievement?
# Are there any relationships that can be extracted from educational achievement and other sociological factors?

# (4) - What are the top three cities that have the highest and lowest local purchasing power and what are those cities' state crime index?
