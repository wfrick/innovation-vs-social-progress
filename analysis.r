#A quick exercise to correlate innovation to GDP and social progress indicators

#Load Social Progress Index, drop rows without an SPI value
SPI <- read.csv("Social progress rankings.csv")
SPI <- SPI[!is.na(SPI$Social.Progress.Index), ]
print(dim(SPI))

#Load WEF innovation rankings
WEF <- read.csv("WEF rankings.csv")
print(dim(WEF))

#Match the two datasets by country name
combined <- merge(SPI, WEF, by.x=c("Country"), by.y=c("Economy"))
print(dim(combined))

#Innovation is extremely correlated with measures of human progress (r=.89)
cor(combined$Score,combined$Social.Progress.Index)

#Scatterplot of innovation score vs. social progress score
#The correlation levels off at higher levels
plot(combined$Score,combined$Social.Progress.Index)

#Fit regression model: coefficient for SPI score is .8
model1 <- lm(combined$Score ~ combined$Social.Progress.Index)
summary(model1)

#Load GDP per capita data from World Bank
GDP <- read.csv("gdp per capita.csv")

#Merge GDP data with combined dataframe
combined <- merge(combined, GDP, by="Country.Code")

#GDP per capita is extremely correlated with innovation (.87)
cor(combined$Score,combined$X2016)

#GDP per capita is correlated strongly with social progress (.79)
cor(combined$Social.Progress.Index,combined$X2016)

#Scatterplot of innovation score vs GDP per capita
#The relationship is nonlinear
plot(combined$X2016,combined$Score)

#Likewise, the relationship between GDP and social progress is not linear
plot(combined$X2016,combined$Social.Progress.Index)

#Fitting a model with all three variables
model2 <- lm(combined$Score ~ combined$X2016)
summary(model2)

#Fitting a model with all three variables
model3 <- lm(combined$Score ~ combined$Social.Progress.Index + combined$X2016)
summary(model3)
#The relationship between social progress and innovation remains, but is less strong

#R-squared with just GDP per capita: .76
#With just social progress: .79
#With both: .87
