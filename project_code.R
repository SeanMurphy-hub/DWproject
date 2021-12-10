###### Other links:
# (https://www.numbeo.com/cost-of-living/rankings.jsp) ##site we scraped from
#
rm(list=ls())
#install.packages('stringr')
#install.packages("dplyr")
library(dplyr)
library(stringr)

# (https://www.kaggle.com/rishidamarla/sociological-metrics-of-all-50-states)
# Sociological Data File (from Kaggle ['data.csv'])
socioData <- read.csv("data.csv")

#(https://worldpopulationreview.com/us-cities) population growth data
  ##CSV download from the site is named csvData.csv
population_change<-read.csv('csvData.csv') 

##### Code that scrapes all the data and turns it into a data frame in R:
library(xml2)
page<-read_html("https://www.numbeo.com/cost-of-living/rankings.jsp?title=2021-mid") 
class(page) # Result is class xml_document
page # Print HTML to console

###Scrape the main table on the site and make appropriate columns numeric
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

### Clean and combine the population growth data with mergeddata 
# Remove unwanted data columns, only keep state name and population growth
population_change <- population_change[,c(2,6)]
# Change NYC name to New York to match mergeddata. 
population_change[1,1] <- "New York"

alldatamerged <- merge(mergeddata,population_change,by.x='city',by.y = 'name')


### Summary statistics ###

# Mean, minimum, maximum cli, and the quartiles (50% =Q2 = median)
mean(alldatamerged$cli)
min(alldatamerged$cli)
max(alldatamerged$cli)
quantile(alldatamerged$cli)

# Histograms of the cli, RentIndex, and GroceriesIndex to compare distributions
hist(alldatamerged$cli, main="Distribution of Cost of Living", xlab = "Cost of Living", col='orange')
hist(alldatamerged$RentIndex, main="Distribution of Rent Index", xlab = "Rent Index", col='pink')
hist(alldatamerged$GroceriesIndex, main="Distribution of Groceries Index", xlab = "Groceries Index", col='yellow')

# Average cli by region
meanCliByRegion<-aggregate(alldatamerged$cli,
              by=list(alldatamerged$Region), # grouping
              FUN=mean)

################# Analysis Questions: #################

# (1) - Which region of the U.S. saw cities with the highest population increase and what are those cities' average cost of living index?

question_1a<-alldatamerged %>%
  group_by(Region) %>%
  summarise(mean_population_change=mean(growth)) %>%
  arrange(desc(mean_population_change))
question_1b<-alldatamerged %>%
  filter(Region=='South Atlantic') %>%
  summarise(south_atlantic_mean_cli=mean(cli))


# Bar plot visualizing average CLI by region
barplot(meanCliByRegion$x,xlab="CLI", main="CLI By Region", names.arg=meanCliByRegion$Group.1, horiz=T, las=1, col=c("darkgray","darkgray","darkgray",
                                                                                                                     "darkgray","red","darkgray",
                                                                                                                     "darkgray","darkgray","darkgray"))


#The South Atlantic region had the highest mean population growth among its included cities and those cities' average CLI was 73.12667. 

# (2) - Are there any relationships between population change and the cost of housing?

reg<-lm(alldatamerged$growth~alldatamerged$cli+alldatamerged$RentIndex+alldatamerged$GroceriesIndex+alldatamerged$RestaurantPriceIndex+alldatamerged$LocalPurchasingPowerIndex)
summary(reg)

rent_reg<-lm(alldatamerged$RentIndex~alldatamerged$growth)
summary(rent_reg)

cor(alldatamerged$growth, alldatamerged$RentIndex) # Correlation = 0.2377455

plot(alldatamerged$RentIndex~alldatamerged$growth, main = "Population Growth vs. Rent Index", xlab = "Population Growth", ylab = "Rent Index")
abline(rent_reg, col = "red")

#The r-squared value is very small, so population growth does not appear to account for much of the variability in the cost of housing, but there is a positive correlation 
#between the two variables, and the p-value (.0337) of the growth variable indicates it is a significant predictor of rent index. 

# (3) - Did cities with decreasing populations have a lower educational attainment than those with increasing populations?

alldatamerged<-alldatamerged %>%
  mutate(growth_binary=case_when(
    growth>0~"Growth",
    growth<=0~"Non-Growth"
  ))

question_3<-alldatamerged %>%
  group_by(growth_binary) %>%
  summarise(mean_education=mean(Percent.Educational.Attainment,na.rm = T))
  
#No, the two mean education levels are almost identical, and if anything, cities that did not have population growth had higher educational attainment.

#t-test to confirm our analysis from summary table
decreasing_pop_edu <- alldatamerged[alldatamerged$growth_binary=="Non-Growth",
                                    "Percent.Educational.Attainment"]
increasing_pop_edu <- alldatamerged[alldatamerged$growth_binary=="Growth",
                                    "Percent.Educational.Attainment"]

t.test(decreasing_pop_edu, increasing_pop_edu)

#The t-test has a p-value of 0.9018. Thus, we cannot reject HO that the average mean educational attainment
#differs for states with population increase vs. those with population decrease.

# (4) - What are the top three cities that have the highest and lowest local purchasing power?

top_three_df<-alldatamerged %>%
  top_n(3,alldatamerged$LocalPurchasingPowerIndex) %>%
  arrange(desc(LocalPurchasingPowerIndex))

top_three<-top_three_df$city
top_three

bottom_three_df<-alldatamerged %>%
  top_n(-3,alldatamerged$LocalPurchasingPowerIndex) %>%
  arrange(LocalPurchasingPowerIndex)

bottom_three<-bottom_three_df$city
bottom_three
