# Necessary Libraries
library(car)
library(ggplot2)
library(olsrr)
library(lmtest)
library(dplyr)

# Read in Data
kbb = read.csv('C:/Users/abedu/Downloads/KBB.csv')

# Make things into factors for plots
kbb$Cylinder = as.factor(kbb$Cylinder)
kbb$Doors = as.factor(kbb$Doors)
kbb$Sound = as.factor(kbb$Sound)
kbb$Cruise = as.factor(kbb$Cruise)
kbb$Leather = as.factor(kbb$Leather)
kbb$Make = as.factor(kbb$Make)
kbb$Trim = as.factor(kbb$Trim)
kbb <- kbb %>% select(-Price,Price)

# EDA
ggplot(kbb, aes(x=Mileage,y=Price)) + geom_point() + geom_smooth(method = "lm", se=FALSE)
ggplot(kbb, aes(x=Make,y=Price)) + geom_boxplot()
ggplot(kbb, aes(x=Model,y=Price)) + geom_boxplot()
ggplot(kbb, aes(x=Trim,y=Price)) + geom_boxplot()
ggplot(kbb, aes(x=Type,y=Price)) + geom_boxplot()
ggplot(kbb, aes(x=Cylinder,y=Price)) + geom_boxplot()
ggplot(kbb, aes(x=Doors,y=Price)) + geom_boxplot()
cor(kbb$Mileage,kbb$Price)

# Fitting an initial model
fullmodel <- lm(Price~., data = kbb)
summary(fullmodel)


# Finding the Best Subset of the data to use
k = ols_step_best_subset(fullmodel, by = AIC)
plot(k)

#Fitting the new model
newmodel <- lm(Price~Model+Mileage+Trim+Leather+Sound, data = kbb)
summary(newmodel)

# Checking for Interactions
ggplot(kbb, aes(x=Price,y=Mileage, color = Model)) + geom_point() + geom_smooth(method = "lm", se=FALSE)
ggplot(kbb, aes(x=Price,y=Mileage, color = Make)) + geom_point() + geom_smooth(method = "lm", se=FALSE)

# Looking at different transformations, comparing models
intmodel <- lm(sqrt(Price)~Model+Mileage+Trim+Leather+Sound, data = kbb)
intmodel2 <- lm(Price~Model+Mileage+Trim+Leather+Sound, data = kbb)
summary(intmodel)
summary(intmodel2)


# collinearity check
vif(intmodel)
alias(lm(Price~Model+Mileage+Trim+Leather+Sound, data = kbb))

# Getting some predictions
some.preds <- predict.lm(object=intmodel, newdframe=kbb, level=.95)
some.preds <- some.preds**2


# Validating Assumptions
kbb[650,]
some.preds[650]
plot(intmodel)
hist(intmodel$residuals)
bptest(intmodel)
ggplot(kbb, aes(x=intmodel$fitted.values,y=intmodel$residuals)) + geom_point()
avPlots(intmodel)


#CV
n.cv <- 100 #Number of CV studies to run
n.test <- round(nrow(kbb)*.1) #Number of observations in a test set
rpmse <- rep(x=NA, times=n.cv)
bias <- rep(x=NA, times=n.cv)
wid <- rep(x=NA, times=n.cv)
cvg <- rep(x=NA, times=n.cv)
for(cv in 1:n.cv){
  ## Select test observations
  test.obs <- sample(x=1:nrow(kbb), size=n.test)
  
  ## Split into test and training sets
  test.set <- kbb[test.obs,]
  train.set <- kbb[-test.obs,]
  
  ## Fit a lm() using the training data
  train.lm <- lm(sqrt(Price)~Model+Mileage+Trim+Leather+Sound+Mileage:Model, data = train.set)
  
  ## Generate predictions for the test set
  test.preds <- predict(object=train.lm, newdata=test.set, level=.95)**2
  
  ## Calculate bias
  bias[cv] <- mean(test.preds-test.set[['Price']])
  
  ## Calculate RPMSE
  rpmse[cv] <- (test.set[['Price']]-test.preds)^2 %>% mean() %>% sqrt()
  
  ## Calculate Coverage
  ##cvg[cv] <- ((test.set[['Price']] > my.preds[,'lwr']) & (test.set[['Final']] < my.preds[,'upr'])) %>% mean()
  
  ## Calculate Width
  ##wid[cv] <- (my.preds[,'upr'] - my.preds[,'lwr']) %>% mean()
}

# Mean values of bias, rpmse, cvg, and wid
mean(bias)
mean(rpmse)
#mean(cvg)
#mean(wid)


#Inference and answering questions
setmiles <- kbb
setmiles$Mileage <- 15000
miles.preds <- predict(object=intmodel, newdata=setmiles, level=.95)
miles.preds**2
which.max(miles.preds)
miles.preds[151]**2
setmiles[151,]
max(miles.preds)**2
kbb[151,]

#Prediction
ourcar <- c(17000, "Cadillac", "CTS", "Sedan 4D", "Sedan", 6, 2.8, 4, 1, 1, 1, 10)
pred.df <- rbind(kbb, ourcar)
pred.df$Mileage <- as.numeric(pred.df$Mileage)
ourcar.pred <- predict(object=intmodel, newdata=pred.df[805,], level=.95, interval = "prediction")
ourcar.pred**2


# Interactions inference
intmodel1 <- lm(sqrt(Price)~Make+Mileage+Trim+Leather+Sound, data = kbb)
intmodel2 <- lm(sqrt(Price)~Make+Mileage+Trim+Leather+Sound+Mileage:Make, data = kbb)
summary(intmodel1)
summary(intmodel2)

