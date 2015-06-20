# Install car package

install.packages(c("car", "gvlma", "MASS", "leaps"))
library(car)

# Import liberty data
libertyRegressionData <- read.csv("/Users/janardhanbonu/OneDrive/MUM/datamining/LibertyRegressionData.csv")
names(libertyRegressionData)

# Scatter plot
scatterplotMatrix(libertyRegressionData, spread=FALSE, lty.smooth=2,main="Scatter Plot Matrix")

# Correlation
cor(libertyRegressionData)

fit <- lm(Revenue~Items, data=libertyRegressionData)
plot(fit)
summary(fit)
fit <- lm(Revenue~Items+Employees, data=libertyRegressionData)
plot(fit)
summary(fit)

fit <- lm(Revenue~Items+Employees+GlobalGDPIndexperCapita, data=libertyRegressionData)
plot(fit)
summary(fit)

# Best fit model
model <- lm(Revenue~Items+Employees, data=libertyRegressionData)
abline(model)
plot(model)

# Prediction

predict(model, list(Year=13,GlobalGDPIndexperCapita=190, CustServCalls=50,Employees=100,Items=25))
predict(model, list(Employees=100,Items=25))
