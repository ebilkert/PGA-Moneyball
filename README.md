# PGA-Moneyball

#Loading the rvest package

library('rvest')
library(dplyr)

#2017 Money


#Specifying the url for desired website to be scrapped
url <- 'https://www.pgatour.com/stats/stat.109.2017.html'

#Reading the HTML code from the website
webpage1 <- read_html(url)

#Using CSS selectors to scrap the names section
money_data_html <- html_nodes(webpage1,'td')

#Converting the ranking data to text
money_data <- html_text(money_data_html)

#Data-Preprocessing: removing '\n' & column header
money_data<-gsub("\n","",money_data)
money_data<-gsub("\\$","",money_data)
money_data<-gsub(",","",money_data)
money_data<-money_data[c(4:1515)]

#Let's have a look at the names
head(money_data)

#Turn string into Dataframe
money_data<-matrix(money_data,nrow=252,ncol=6,byrow = TRUE)

#Create Column Names
colnames(money_data)=c("TW","LW","PlayerName", "Events", "Money", "YTD")

#Keep desired columns
money_data<-money_data[,c("PlayerName", "Events", "Money")]


#2016 Money Data


#Specifying the url for desired website to be scrapped
url <- 'https://www.pgatour.com/stats/stat.109.2016.html'

#Reading the HTML code from the website
webpage1 <- read_html(url)

#Using CSS selectors to scrap the names section
money_data_html <- html_nodes(webpage1,'td')

#Converting the ranking data to text
money_data2 <- html_text(money_data_html)

#Data-Preprocessing: removing '\n' & column header
money_data2<-gsub("\n","",money_data2)
money_data2<-gsub("\\$","",money_data2)
money_data2<-gsub(",","",money_data2)
money_data2<-money_data2[c(4:1509)]

#Let's have a look at the names
head(money_data2)

#Turn string into Dataframe
money_data2<-matrix(money_data2,nrow=251,ncol=6,byrow = TRUE)

#Create Column Names
colnames(money_data2)=c("TW","LW","PlayerName", "Events", "Money", "YTD")

#Keep desired columns
money_data2<-money_data2[,c("PlayerName", "Events", "Money")]

#Combine the two dataframes
combinedmoneydata<-rbind(money_data,money_data2)

#Convert from matricies to dataframe
combinedmoneydata<-as.data.frame(combinedmoneydata)

#Convert money column to numeric
combinedmoneydata$Money<-as.numeric(as.character(combinedmoneydata$Money))
combinedmoneydata$Events<-as.numeric(as.character(combinedmoneydata$Events))

#Group by playername
combinedmoneydata<-aggregate(.~combinedmoneydata$PlayerName, combinedmoneydata, sum)
combinedmoneydata$PlayerName<-NULL

#Name Columns
colnames(combinedmoneydata)=c('PlayerName','Events', 'Winnings')


#2017 Distance Data


#Specifying the url for desired website to be scrapped
url <- 'https://www.pgatour.com/stats/stat.101.2017.html'

#Reading the HTML code from the website
webpage2 <- read_html(url)

#Using CSS selectors to scrap the money section
distance_data_html <- html_nodes(webpage2,'td')

#Converting the money data to text
distance_data <- html_text(distance_data_html)

#Data-Preprocessing: Removing column headers and convert distance data to numerical
distance_data<-distance_data[c(6:1333)]
distance_data<-gsub(",","",distance_data)
distance_data<-gsub("\n","",distance_data)

#Let's have a look at the distance data
head(distance_data)

#Convert distance data string to a data frame with four columns
distance_data<-matrix(distance_data,nrow=190,ncol=7,byrow = TRUE)


#2016 Distance Data

#Specifying the url for desired website to be scrapped
url <- 'https://www.pgatour.com/stats/stat.101.2016.html'

#Reading the HTML code from the website
webpage2 <- read_html(url)

#Using CSS selectors to scrap the money section
distance_data_html <- html_nodes(webpage2,'td')

#Converting the money data to text
distance_data2 <- html_text(distance_data_html)

#Data-Preprocessing: Removing column headers and convert distance data to numerical
distance_data2<-distance_data2[c(6:1298)]
distance_data2<-gsub(",","",distance_data2)
distance_data2<-gsub("\n","",distance_data2)

#Let's have a look at the distance data
head(distance_data2)

#Convert distance data string to a data frame with four columns
distance_data2<-matrix(distance_data2,nrow=185,ncol=7,byrow = TRUE)

#Combining the two matricies
combineddistance<-rbind(distance_data,distance_data2)

#Convert to data frame
combineddistance<-as.data.frame(combineddistance)

#Drop Unneeded columns
combineddistance$V2<-NULL
combineddistance$V3<-NULL
combineddistance$V6<-NULL
combineddistance$V7<-NULL

#Creating column names
colnames(combineddistance)=c("PlayerName", "TotalDist", "TotalDrives")

#Change column data types
combineddistance$TotalDist<-as.numeric(as.character(combineddistance$TotalDist))
combineddistance$TotalDrives<-as.numeric(as.character(combineddistance$TotalDrives))

#Group total distance and drives by playername
combineddistance<-aggregate(.~combineddistance$PlayerName, combineddistance, sum)
#Drop unncessary column created and rename
combineddistance$PlayerName<-NULL
colnames(combineddistance)=c("PlayerName", "TotalDist", "TotalDrives")

#Inner Join dataframes on playername
combineddata<-merge(combinedmoneydata,combineddistance,by='PlayerName')

#create average distance column
combineddata$averagedist<- combineddata$TotalDist/combineddata$TotalDrives

#Creating the correlation plots
library(ggplot2)
library(scales)
ggplot(combineddata,aes(x=combineddata$averagedist,y=combineddata$Winnings)) + geom_point(aes(color = 'cyl')) + ggtitle("Tour Winnings by Average Drive Distance") +
  labs(x="Average Drive Distance (Yds)", y="Winnings (USD)") + scale_y_continuous(name = "Winnings (USD)" ,labels = comma) + theme(legend.position="none") + geom_smooth(method='lm')

summary(lm(Winnings ~ averagedist,combineddata))

#Create new column of winnings/round
combineddata$winnings.event<- combineddata$Winnings/combineddata$Events

#Creating the new correlation plot
ggplot(combineddata,aes(x=combineddata$averagedist,y=combineddata$winnings.event)) + geom_point(aes(color = 'cyl')) + ggtitle("Winnings Per Event by Average Drive Distance") +
  labs(x="Average Drive Distance (Yds)", y="Winnings Per Event (USD)") + scale_y_continuous(name = "Winnings (USD)" ,labels = comma) + theme(legend.position="none") + geom_smooth(method='lm')

summary(lm(winnings.event ~ averagedist,combineddata))

#Histogram of events
hist(combineddata$Events, col = 'blue', breaks = 24, main = "Events Played", xlab = 'Events')

#New correlation plot, events played >= 32
event<-subset(combineddata, Events >= 32)

ggplot(event,aes(x=event$averagedist,y=event$winnings.event)) + geom_point(aes(color = 'cyl')) + ggtitle("Winnings Per Event by Average Drive Distance (Events >= 32)") +
  labs(x="Average Drive Distance (Yds)", y="Winnings Per Event (USD)") + scale_y_continuous(name = "Winnings (USD)" ,labels = comma) + theme(legend.position="none") + geom_smooth(method='lm')

summary(lm(winnings.event ~ averagedist,event))

#Fitting an exponential curve to the data
ggplot(event,aes(x=event$averagedist,y=event$winnings.event)) + geom_point(aes(color = 'cyl')) + ggtitle("Winnings Per Event by Average Drive Distance (Events >= 32)") +
  labs(x="Average Drive Distance (Yds)", y="Winnings Per Event (USD)") + scale_y_continuous(name = "Winnings (USD)" ,labels = comma) + theme(legend.position="none") + geom_smooth(method="lm", aes(color="Exp Model"), formula= (y ~ x + I(x^2)), se=FALSE, linetype = 1)

summary(lm(winnings.event ~ averagedist + I(averagedist^2), data = event))

#Best R^2 chosen from a variety of exponential models
mdl1 <- lm(y ~ x, data = xy)
mdl2 <- lm(y ~ x + I(x^2), data = xy)
mdl3 <- lm(y ~ x + I(x^2) + I(x^3), data = xy)
mdl4 <- lm(y ~ I(x^2), data = xy)

#Scoring measures


##2017 Money


#Specifying the url for desired website to be scrapped
url <- 'https://www.pgatour.com/stats/stat.109.2017.html'

#Reading the HTML code from the website
webpage1 <- read_html(url)

#Using CSS selectors to scrap the names section
money_data_html <- html_nodes(webpage1,'td')

#Converting the ranking data to text
money_data <- html_text(money_data_html)

#Data-Preprocessing: removing '\n' & column header
money_data<-gsub("\n","",money_data)
money_data<-gsub("\\$","",money_data)
money_data<-gsub(",","",money_data)
money_data<-money_data[c(4:1515)]

#Let's have a look at the names
head(money_data)

#Turn string into Dataframe
money_data<-matrix(money_data,nrow=252,ncol=6,byrow = TRUE)

#Create Column Names
colnames(money_data)=c("TW","LW","PlayerName", "Events", "Money", "YTD")

#Keep desired columns
money_data<-money_data[,c("PlayerName", "Events", "Money")]

#2017 Scoring

#2017 Distance Specifying the url for desired website to be scrapped
url <- 'https://www.pgatour.com/stats/stat.120.2017.html'

#Reading the HTML code from the website
webpage2 <- read_html(url)

#Using CSS selectors to scrap the money section
distance_data_html <- html_nodes(webpage2,'td')

#Converting the money data to text
distance_data <- html_text(distance_data_html)

#Data-Preprocessing: Removing column headers and convert distance data to numerical
distance_data<-distance_data[c(4:1523)]
distance_data<-gsub(",","",distance_data)
distance_data<-gsub("\n","",distance_data)

#Let's have a look at the distance data
head(distance_data)

#Convert distance data string to a data frame with four columns
distance_data<-matrix(distance_data,nrow=190,ncol=8,byrow = TRUE)

#Name columns
colnames(distance_data)=c("TW","LW","PlayerName", "Events", "Average", "YTD","na",'fill')

#Merge tables
combineddata<-merge(distance_data,money_data,by='PlayerName')

#Keep desired columns
combineddata<-combineddata[,c("PlayerName", "Average", "Money")]

#Convert to numeric
combineddata$Average<-as.numeric(as.character(combineddata$Average))
combineddata$Money<-as.numeric(as.character(combineddata$Money))


#Plot scatter
library(ggplot2)
library(scales)
ggplot(combineddata,aes(x=combineddata$Average,y=combineddata$Money)) + geom_point(aes(color = 'cyl')) + ggtitle("2017 Tour Winnings by Scoring Average") +
  labs(x="Scoring Average", y="Winnings (USD)") + scale_y_continuous(name = "Winnings (USD)" ,labels = comma) + theme(legend.position="none") + geom_smooth(method='lm')

summary(lm(Money ~ Average,combineddata))

#Fitting an exponential curve

#Possible models
Summary(lm(Money ~ Average, data = combineddata))
summary(lm(Money ~ Average + I(Average^2), data = combineddata))
summary(lm(Money ~ Average + I(Average^2) + I(Average^3), data = combineddata))
summary(lm(Money ~ I(Average^2), data = combineddata))


library(ggplot2)
library(scales)
ggplot(combineddata,aes(x=combineddata$Average,y=combineddata$Money)) + geom_point(aes(color = 'cyl')) + ggtitle("2017 Tour Winnings by Scoring Average") +
  labs(x="Scoring Average", y="Winnings (USD)") + scale_y_continuous(name = "Winnings (USD)" ,labels = comma) + theme(legend.position="none") + geom_smooth(method="lm", aes(color="Exp Model"), formula= (y ~ x + I(x^2) + I(x^3)) , se=FALSE, linetype = 1)

summary(lm(Money ~ Average + I(Average^2) + I(Average^3), data = combineddata))

Summary(lm(Money ~ Average, data = combineddata))
summary(lm(Money ~ Average + I(Average^2), data = combineddata))
summary(lm(Money ~ Average + I(Average^2) + I(Average^3), data = combineddata))
summary(lm(Money ~ I(Average^2), data = combineddata))


#2017 GIR Data
#Specifying the url for desired website to be scrapped
url <- 'https://www.pgatour.com/stats/stat.103.2017.html'

#Reading the HTML code from the website
webpage1 <- read_html(url)

#Using CSS selectors to scrap the names section
gir_data_html <- html_nodes(webpage1,'td')

#Converting the ranking data to text
gir_data <- html_text(gir_data_html)

#Data-Preprocessing: removing '\n' & column header
gir_data<-gsub("\n","",gir_data)
gir_data<-gsub("\\$","",gir_data)
gir_data<-gsub(",","",gir_data)
gir_data<-gir_data[c(4:1523)]

#Let's have a look at the names
head(gir_data)

#Turn into data frame
gir_data<-matrix(gir_data,nrow=190,ncol=8,byrow = TRUE)


#2017 Money


#Specifying the url for desired website to be scrapped
url <- 'https://www.pgatour.com/stats/stat.109.2017.html'

#Reading the HTML code from the website
webpage1 <- read_html(url)

#Using CSS selectors to scrap the names section
money_data_html <- html_nodes(webpage1,'td')

#Converting the ranking data to text
money_data <- html_text(money_data_html)

#Data-Preprocessing: removing '\n' & column header
money_data<-gsub("\n","",money_data)
money_data<-gsub("\\$","",money_data)
money_data<-gsub(",","",money_data)
money_data<-money_data[c(4:1515)]

#Let's have a look at the names
head(money_data)

#Turn string into Dataframe
money_data<-matrix(money_data,nrow=252,ncol=6,byrow = TRUE)

#Inner join on GIR and Money 2017
data2017<-merge(money_data,gir_data,by='V3')

#Keep desired columns
data2017<-data2017[,c("V3", "V5.x", "V5.y")]

#Name columns
colnames(data2017)=c("PlayerName", 'Winnings', 'GIR')

#Convert to numeric
data2017$Winnings<-as.numeric(as.character(data2017$Winnings))
data2017$GIR<-as.numeric(as.character(data2017$GIR))

#Plot scatter
library(ggplot2)
library(scales)
ggplot(data2017,aes(x=data2017$GIR,y=data2017$Winnings)) + geom_point(aes(color = 'cyl')) + ggtitle("2017 Tour Winnings by GIR Percentage ") +
  labs(x="GIR Percentage", y="Winnings (USD)") + scale_y_continuous(name = "Winnings (USD)" ,labels = comma) + theme(legend.position="none") + geom_smooth(method='lm')

summary(lm(Winnings ~ GIR,data2017))

#Fit an exponential model
Summary(lm(Winnings ~ GIR, data = data2017))
summary(lm(Winnings ~ GIR + I(GIR^2), data = data2017))
summary(lm(Winnings ~ GIR + I(GIR^2) + I(GIR^3), data = data2017))
summary(lm(Winnings ~ I(GIR^2), data = data2017))

#Plot scatter
library(ggplot2)
library(scales)
ggplot(data2017,aes(x=data2017$GIR,y=data2017$Winnings)) + geom_point(aes(color = 'cyl')) + ggtitle("2017 Tour Winnings by GIR Percentage ") +
  labs(x="GIR Percentage", y="Winnings (USD)") + scale_y_continuous(name = "Winnings (USD)" ,labels = comma) + theme(legend.position="none") + geom_smooth(method="lm", aes(color="Exp Model"), formula= (y ~ x + I(x^2) + I(x^3)) , se=FALSE, linetype = 1)


#Putting Data



#Combining GIR and Distance Data

#2017 Distance Specifying the url for desired website to be scrapped
url <- 'https://www.pgatour.com/stats/stat.101.2017.html'

#Reading the HTML code from the website
webpage2 <- read_html(url)

#Using CSS selectors to scrap the money section
distance_data_html <- html_nodes(webpage2,'td')

#Converting the money data to text
distance_data <- html_text(distance_data_html)

#Data-Preprocessing: Removing column headers and convert distance data to numerical
distance_data<-distance_data[c(6:1333)]
distance_data<-gsub(",","",distance_data)
distance_data<-gsub("\n","",distance_data)

#Let's have a look at the distance data
head(distance_data)

#Convert distance data string to a data frame with four columns
distance_data<-matrix(distance_data,nrow=190,ncol=7,byrow = TRUE)
