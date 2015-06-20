libertyClusteringData <- read.csv("/Users/janardhanbonu/OneDrive/MUM/datamining/libertystore/LibertyClustering.csv")
km <- kmeans(libertyClusteringData,3)
km
km <- kmeans(libertyClusteringData)
km <- kmeans(libertyClusteringData,1)
plot(km)
km <- kmeans(libertyClusteringData,3)
km
km <- kmeans(libertyClusteringData,1)
km1 <- kmeans(libertyClusteringData,1)
km2 <- kmeans(libertyClusteringData,2)
km3 <- kmeans(libertyClusteringData,3)
summary(km1)
summary(km2)
summary(km3)
km1
km2
km3
km3 <- kmeans(libertyClusteringData[-1],3)
km3
plot(km3)
table(libertyClusteringData$Income, km3$cluster)

plot(libertyClusteringData[c("NoOfTransactions", "TotalPurchase")], col = km2$cluster)
par(mfrow = c(1,1))
plot(libertyClusteringData[c("NoOfTransactions", "TotalPurchase")], col = km2$cluster)
plot(libertyClusteringData[c("NoOfTransactions", "TotalPurchase", "Income")], col = km2$cluster)
plot(libertyClusteringData[c("NoOfTransactions", "TotalPurchase")], col = km2$cluster)
points(km3$centers[, c("NoOfTransactions", "TotalPurchase")],col = 1:3, pch = 8, cex = 2)
plot(libertyClusteringData[c("NoOfTransactions", "TotalPurchase")], col = km3$cluster)
points(km3$centers[, c("NoOfTransactions", "TotalPurchase")],col = 1:3, pch = 8, cex = 2)

