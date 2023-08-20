#set the working directory to where your data files are saved
setwd("C:\\Users\\sabar\\OneDrive\\Desktop")

#install the required packages and call them using library() command
install.packages("nlme")
install.packages("lmtest")
install.packages("sandwich")
install.packages("estimatr")
install.packages("texreg")
install.packages("readxl")

library("readxl")
library(nlme)
library("lmtest")
library("sandwich")
library(estimatr)
library(texreg)


#Load the dataset meap00_01 into R
load("401ksubs.RData")
#call the data , math
math <- data
#run an OLS regression
ols.model1 <- lm(math4 ~ lunch+lenroll+lexppp,data=math)
#see summary of the regression result
summary(ols.model1)
#plot the results of the regression (residuals vs. fitted values)
plot(ols.model1)

#Run a Breusch-Pagan Test
bptest(ols.model1, data=math)

?lm_robust
#run the OLS regression with robust Standard errors
ols.robust1 <- lm_robust(math4 ~ lunch+lenroll+lexppp, data=math, se_type = "stata")
summary(ols.robust1)

u_hat <- ols.model1$residuals 
g_hat <- log(u_hat^2)
g_hat_reg <- lm(g_hat ~ lunch+lenroll+lexppp, data=math)
h_hat <- exp(g_hat_reg$fitted.values)
h <- 1/sqrt(h_hat)

#Run a fgls model with unknown functional form for het
gls.model1 <- lm(math4 ~ lunch+lenroll+lexppp, data=math, weights = h)

#see summary of the results
summary(gls.model1)

?screenreg
screenreg(list(ols.model1, gls.model1, ols.robust1),include.ci = FALSE, digits=6)

#load the second data set (crime1.RData)
load("crime1.RData")
#save this data as crime
crime <- data
#create a dummy which =1 if person arrested in 1986 and zero otherwise
crime$arr86 <- ifelse(crime$narr86>0,1,0)
#run the following ols regression using the lm() function
ols.model2 <- lm(arr86 ~ pcnv+avgsen+tottime+ptime86+qemp86 ,data=crime)
#show the results of the regression
summary(ols.model2)

#test whether there exists heteroscedasticity using Breuch-Pagan test
bptest(ols.model2,data=crime)

#see if there are outliers in your fitted values
plot(x=ols.model2$fitted.values, y=crime$arr86,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')

#define the h function to remove het
h <- sqrt(1/(ols.model2$fitted.values * (1-ols.model2$fitted.values)))

#use h to run a weighted least squares regression (WLS) using gls() function
wls.model2 <- lm_robust(arr86 ~ pcnv+avgsen+tottime+ptime86+qemp86, data=crime, weights=h)
#view the results of the wls
summary(wls.model2)

#run an ols with lm_robust that returns robust standard errors, t-stats and F-stat
ols.robust2 <- lm_robust(arr86 ~ pcnv+avgsen+tottime+ptime86+qemp86, data=crime, se_type = "stata")
summary(ols.robust2)

screenreg(list(ols.model2,wls.model2, ols.robust2),include.ci = FALSE, digits = 6)

#Load fertil2 dataset
fertil <- read_excel("fertil.xls")

#Run OLS regression
ols.model3 <- lm(children~age+agesq+educ+electric+urban, data=fertil)
summary(ols.model3)#View the ols result

#test for het
bptest(ols.model3,data=fertil)
#run ols with robust standard errors
ols.robust3 <- lm_robust(children~age+agesq+educ+electric+urban, data=fertil, se_type = "stata")
summary(ols.robust3)

u_hat1 <- ols.model3$residuals
g_hat1 <- log(u_hat1^2)
g_hat_reg1 <- lm(g_hat1 ~ age+agesq+educ+electric+urban, data=fertil)
h_hat1 <- exp(g_hat_reg1$fitted.values)
h1 <- 1/sqrt(h_hat1)
gls.model3 <- lm(children~age+agesq+educ+electric+urban, data=fertil, weight=h1)

screenreg(list(ols.model3,gls.model3, ols.robust3),include.ci = FALSE, digits=6)
