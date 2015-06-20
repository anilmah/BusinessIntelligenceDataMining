library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("gplots", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

divvydata <- read.csv("/Users/janardhanbonu/OneDrive/MUM/datamining/rawdata/Divvy_Stations_Trips_2014-Q1Q2/Divvy_Trips_2014_Q1Q2.csv")
names(divvydata)
nrows(divvydata)
# splitdf function will return a list of training and testing sets
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}
splits <- splitdf(divvydata, seed=20000)
lapply(splits,nrow)
testdata <- splits$trainset
if (!is.null(seed)) set.seed(seed)
index <- 1:nrow(dataframe)
divvydata$starttime <- strptime(divvydata$starttime,"%m/%d/%Y %H:%M")
divvydata$stoptime <- strptime(divvydata$stoptime,"%m/%d/%Y %H:%M")
divvydata$weekday <- weekdays(divvydata$starttime)
divvydata$month <- months(divvydata$starttime)
divvydata$season[divvydata$month=="January"] <- "Winter"
divvydata$season[divvydata$month=="February"] <- "Winter"
divvydata$season[divvydata$month=="March"] <- "Spring"
divvydata$season[divvydata$month=="April"] <- "Spring"
divvydata$season[divvydata$month=="May"] <- "Spring"
divvydata$season[divvydata$month=="June"] <- "Summer"
divvydata$season[divvydata$month=="July"] <- "Summer"
divvydata$season[divvydata$month=="August"] <- "Summer"
divvydata$season[divvydata$month=="September"] <- "Fall"
divvydata$season[divvydata$month=="October"] <- "Fall"
divvydata$season[divvydata$month=="November"] <- "Fall"
divvydata$season[divvydata$month=="December"] <- "Winter"
divvydata$season <- as.factor(divvydata$season)
divvydata$month <- as.factor(divvydata$month)
divvydata$weekday <- as.factor(divvydata$weekday)
divvydata$hour <- format(divvydata$starttime,"%H")
divvydata$stationpair <- paste(divvydata$from_station_name,"&&",divvydata$to_station_name)
divvydata$season <- as.factor(divvydata$season)
divvydata$month <- as.factor(divvydata$month)
divvydata$weekday <- as.factor(divvydata$weekday)
divvydata$stationpair <- as.factor(divvydata$stationpair)
qplot(table(divvydata@stationpair))
barplot(table(divvydata$usertype, divvydata$weekday),  beside=T, col=heat.colors(2), xlab="Customer/Subscriber on Weekday", ylab="Number of Trips", legend=T)
barplot(table(divvydata$usertype, divvydata$stationpair),  beside=T, col=heat.colors(2), xlab="Customer/Subscriber - Station ", ylab="Number of Trips", legend=T)
barplot(table( divvydata$stationpair, divvydata$usertype),  beside=T, col=heat.colors(2), xlab="Customer/Subscriber - Station ", ylab="Number of Trips", legend=T)
barplot(table(divvydata$usertype, divvydata$stationpair),  beside=T, col=heat.colors(2), xlab="Customer/Subscriber - Station ", ylab="Number of Trips", legend=T)
barplot(table(divvydata$usertype, divvydata$season),  beside=T, col=heat.colors(2), xlab="Customer/Subscriber - Station ", ylab="Number of Trips", legend=T)
barplot(table(divvydata$hour, divvydata$stationpair),  beside=T, col=heat.colors(2), xlab="Customer/Subscriber - Season ", ylab="Number of Trips", legend=T)

testdata$hour <- format(testdata$starttime,"%H")
testdata$stationpair <- paste(testdata$from_station_name,"&&",testdata$to_station_name)
percent <- round(summary(testdata$weekday) * 100 / nrow(testdata))
labels <- sprintf("%s (%d%%)", levels(testdata$weekday), percent)
pie(summary(testdata$weekday), lab=labels)
pie(table(usertype))
percent <- round(summary(testdata$usertype) * 100 / nrow(testdata))
labels <- sprintf("%s (%d%%)", levels(testdata$usertype), percent)
pie(summary(testdata$usertype), lab=labels)scatterplotMatrix(libertyRegressionData, spread=FALSE, lty.smooth=2,main="Scatter Plot Matrix")
confint(fit, conf.level=0.95)
confint(fit, conf.level=0.99)
summary(fit)
cor(fit)
cor(libertyRegressionData)
cor(libertyRegressionData)
fit <- lm(Revenue~Items, data=libertyRegressionData)
plot(fit)
fit <- lm(Revenue~Items+Employees, data=libertyRegressionData)
plot(fit)
summary(fit)
cor(fit)
summary(fit)
fit <- lm(Revenue~Items, data=libertyRegressionData)
summary(fit)
fit <- lm(Revenue~Items+Employees, data=libertyRegressionData)
summary(fit)
names(libertyRegressionData)
fit <- lm(Revenue~Items+Employees+GlobalGDPIndexperCapita, data=libertyRegressionData)
summary(fit)
fit <- lm(Revenue~Items+Employees, data=libertyRegressionData)
plot(fit)
fit <- lm(Revenue~Items, data=libertyRegressionData)
plot(fit)
fit <- lm(Revenue~Items+Employees, data=libertyRegressionData)
plot(fit)
fit <- lm(Revenue~Items+Employees+GlobalGDPIndexperCapita, data=libertyRegressionData)
plot(fit)
plot(Revenue~Items+Employees, data=libertyRegressionData)
plot(Revenue~Items+Employees, data=libertyRegressionData)
abline()
model <- (Revenue~Items+Employees, data=libertyRegressionData)
model <- lm(Revenue~Items+Employees, data=libertyRegressionData)
abline(model)
plot(model)
annova
anova
anova(model)
anova(model,fit)
names(libertyRegressionData)
predict(model, list(Year=13,GlobalGDPIndexperCapita=190, CustServCalls=50,Employees=100,Items=25))
predict(model, list(Employees=100,Items=25))

barplot(table(hour))
barplot(table(month))
barplot(table(season))
barplot(table(testdata$season))
barplot(table(testdata$weekday))
testdata$stationpair <- as.factor(testdata$stationpair)
summary(testdata$stationpair)
qplot(testdata$starttime, testdata$trip_id, geom = "jitter", alpha = I(1/50), size=testdata$from_station_name)
