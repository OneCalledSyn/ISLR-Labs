#Load packages needed for lab
library(MASS)
library(ISLR)

#See variable names in dataset
fix(Boston)
names(Boston)

#Perform linear regression fit
lm.fit <- lm(medv~lstat, data = Boston)
attach(Boston)
lm.fit <- lm(medv~lstat)

#Show details about contents
lm.fit
summary(lm.fit)

#Variable names of lm.fit
names(lm.fit)

#Check out the models coefficients
coef(lm.fit)

#Confidence interval for the model coefficients
confint(lm.fit)

#Generate confidence interval for medv predictions given an lstat value
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "confidence")

#Generate prediction interval for medv predictions given an lstat value
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "prediction")

#Plot to visually examine the data
plot(lstat, medv)
abline(lm.fit)

#Playing around with abline and plot markers
abline(lm.fit, lwd = 3)
abline(lm.fit, lwd = 3, col = 'red')
plot(lstat, medv, col = 'red')
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = '+')
plot(1:20, 1:20, pch = 1:20)

#Plot residuals and their studentized forms
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

#Compute leverage statistics and display the largest value
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

#Taking a look at multiple linear regression
lm.fit <- lm(medv~lstat+age, data = Boston)
summary(lm.fit)

library(car)
vif(lm.fit)

#Perform regression but exclude the age predictor
lm.fit1 <- lm(medv~. -age, data = Boston)
summary(lm.fit1)

#Interaction terms 
summary(lm(medv~lstat*age, data = Boston))

#Quadratic transform on the predictor
lm.fit2 <- lm(medv~lstat + I(lstat^2))
summary(lm.fit2)

#ANOVA test to see which model fits the data better
lm.fit <- lm(medv~lstat)
anova(lm.fit, lm.fit2)

#Split plot window into 4 parts to see nifty results
par(mfrow = c(2,2))
plot(lm.fit2)

#5th order polynomial fit
lm.fit5 <- lm(medv~poly(lstat,5))
summary(lm.fit5)

#Log transformation of the predictors
summary(lm(medv~log(rm), data = Boston))

#Examining qualitative predictors
fix(Carseats)
names(Carseats)


lm.fit <- lm(Sales~.+Income:Advertising+Price:Age, data = Carseats)
summary(lm.fit)

#View the contrasts for dummy variables
attach(Carseats)
contrasts(ShelveLoc)
