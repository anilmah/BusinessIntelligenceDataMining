divvydata <- read.csv("/Users/janardhanbonu/OneDrive/MUM/datamining/rawdata/Divvy_Stations_Trips_2014-Q1Q2/Divvy_Trips_2014_Q1Q2.csv")
names(divvydata)
nrows(divvydata)
# splitdf function will return a list of training and testing sets
splitdf <- function(dataframe, seed=NULL) {
if (!is.null(seed)) set.seed(seed)
index <- 1:nrow(dataframe)
trainindex <- sample(index, trunc(length(index)/2))
trainset <- dataframe[trainindex, ]
testset <- dataframe[-trainindex, ]
list(trainset=trainset,testset=testset)
}
splits <- splitdf(divvydata, seed=10000)
str(splits)
lapply(splits,nrow)
training <- splits$trainset
testing <- splits$testset
View(testing)
View(training)

nrow(testing)
testdata <- training
View(testdata)
didvvyata$starttime <- strptime(testdata$starttime,"%m/%d/%Y %H:%M")
testdata$stoptime <- strptime(testdata$stoptime,"%m/%d/%Y %H:%M")
testdata$weekday <- weekdays(testdata$starttime)
testdata$month <- months(testdata$starttime)
View(testdata)
testdata$season[testdata$month=="January"] <- "Winter"
testdata$season[testdata$month=="February"] <- "Winter"
testdata$season[testdata$month=="March"] <- "Spring"
testdata$season[testdata$month=="April"] <- "Spring"
testdata$season[testdata$month=="May"] <- "Spring"
testdata$season[testdata$month=="June"] <- "Summer"
testdata$season[testdata$month=="July"] <- "Summer"
testdata$season[testdata$month=="August"] <- "Summer"
testdata$season[testdata$month=="September"] <- "Fall"
testdata$season[testdata$month=="October"] <- "Fall"
testdata$season[testdata$month=="November"] <- "Fall"
testdata$season[testdata$month=="December"] <- "Winter"
View(testdata)
testdata$hour <- format(testdata$starttime,"%H")
testdata$stationpair <- paste(testdata$from_station_name,"&&",testdata$to_station_name)
percent <- round(summary(testdata$weekday) * 100 / nrow(testdata))
labels <- sprintf("%s (%d%%)", levels(testdata$weekday), percent)
pie(summary(testdata$weekday), lab=labels)
pie(table(usertype))
percent <- round(summary(testdata$usertype) * 100 / nrow(testdata))
labels <- sprintf("%s (%d%%)", levels(testdata$usertype), percent)
pie(summary(testdata$usertype), lab=labels)
barplot(table(hour))
barplot(table(month))
barplot(table(season))
barplot(table(testdata$season))
barplot(table(testdata$weekday))
testdata$stationpair <- as.factor(testdata$stationpair)
summary(testdata$stationpair)
qplot(testdata$starttime, testdata$trip_id, geom = "jitter", alpha = I(1/50), size=testdata$from_station_name)


#---------
divvydata <- read.csv("/Users/janardhanbonu/OneDrive/MUM/datamining/divvy/Data/divvy2014/divvydata2014.csv")
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
