#to know where your working directory is:
getwd()

#to set your working directory to where your data files are:
setwd("C:\\Users\\sabar\\OneDrive\\Desktop\\R Tutorials")

#Now if you run getwd() again, the working directory is where you set it
getwd()

#To read excel data files first you need to install and call the required package
install.packages("readxl")
library(readxl)

#Create a data frame called data0 and read the data file in excel format from the working dir
data0 <- read_xls("NLSY79_data_NEW.xls")

#You could instead choose your data file manually by adding choose.files() function into
# the read_xls() function. let's call the new data frame, data1
data1 <- read_xls(choose.files())

#Now let's take a look at the data with View() function (Capital V)
View(data0)

#To look at a specific variable in the data set, use $ after the name of the data set
data0$master
data0$wage
data0$wage
#see how many unique values there are in a variable
unique(data0$educ)

#gives the mean of the variable
mean(data0$wage)
#gives the variance of the variable
var(data0$wage)
#gives the minimum value of the variable
min(data0$wage)
#gives the minimum value of the variable
max(data0$wage)
sd(data0$wage)

#function table() creates a frequency table for you
table(data0$educ)

#From this table you can calculate the proportions of each educational level,
#for example: people for whom the years of education is 10, are high school dropouts
#so proportion of HS-dropouts are (217 / 1208) * 100
#we already have dummies for each educational category, imagine we want to find out
#how many of the 217 dropouts are male:

male_dropout <- data0$male * data0$dropout
table(male_dropout)
table(data0$male)
#another way is to use the group_by() function, for this we need the library "tidyverse"
install.packages("tidyverse")
library(tidyverse)

data0 %>% group_by(male) %>% count(data0$dropout)

#you can add more categorical variables to group_by()

data0 %>% group_by(male, hispanic) %>% count(dropout)
#function summary() shows some useful information about your dataset
summary(data0)

#Add a new variable to your dataset, called lnw, and assign ln of wage to it
data0$lnw <- log(data0$wage)
#If you View the dataset now, you see that a new column has been added to data0 called lnw
View(data0)

#Now create the avgerage family income squared, to take a var to the power n, you use **n
data0$squared <- data0$avg_inc**2
View(data0)

#You can also take the square root of a variable by using sqrt() function
data0$squared <- sqrt(data0$squared)
View(data0)

#to remove a variable from your dataset, assign NULL to it
data0$squared <- NULL
View(data0)

#to get more help on any command in R just type the name with a question mark before it
?mean()

#----------------------------------OLS------------------------------------------
#To run an OLS regression you need to use the lm() function. You can see what arguments
#the function takes and more information about the function by calling the help:
?lm()

#Now let's run a regression: for example regress lnw on educ, male, hispanic and black and call it model1
model1 <- lm(wage ~ educ + male + hispanic + black, data=data0)

#to see the results of your regression you use the summary() function
summary(model1)

#model1 also has some information saved in its structure that can be useful,
#to see what other information model1 contains, you can check its attributes

attributes(model1)

#let's test if the mean of the residuals is zero
mean(model1$residuals)

#Now as in class and in the tutorial we've seen, there may exists some omitted variables bias
#One missing variable may be cognitive ability, we have a variable called cognitive
#Let's see by adding that to the regression are we able to remove some of the bias?
model2 <- lm(wage ~ educ + male + hispanic + black + cognitive, data = data0)
summary(model2)

#Next run a regression for wage on 5 different educational category dummies in the data: 
# 1) whether the person dropped out of high school
# 2) whether the person has a high school diploma and no further
# 3) whether the person has an associate degree and no further
# 4) whether the person has a bachelor's degree and no further
# 5) whether the person has a master's degree (that is the maximum level of education in our data)
#We know that if we have N dummies that covers all the sample, we have to add N-1 dummies
# in the regression, otherwise we will have perfect collinearity in which case the variance
# of the beta_hat will be infinity. Now let's add all 5 dummies and see what happens:

model3 <- lm(wage ~ dropout + hs + associate + bachelor + master, data=data0)
summary(model3)

#Now let's run the regression on only 4 dummies, leaving dropout out as the base and try to 
#interpret the coefficients
model4 <- lm(wage ~ hs + associate + bachelor + master, data=data0)
summary(model4)

#-------------------------------Frisch-Waugh Method-----------------------------
#Now let's try and test if the coefficient calculated through Frisch-Waugh method 
# is the same as the one we get from a multiple regression
model5 <- lm(wage~educ+hispanic+black+male+cognitive, data=data0)
summary(model5)

model6 <- lm(educ~hispanic + black+ male + cognitive, data=data0)

beta_educ <- sum(model6$residuals*data0$wage)/sum(model6$residuals**2)
#or you can just regress y on the residuals:

model7 <- lm(wage~model6$residual, data=data0)
summary(model7)
#-------------------------------Dummy Variables---------------------------------

#Next let's try making dummy variables ourselves
unique(data0$educ)
data0$dropout1 <- ifelse(data0$educ==10, 1,0)
data0$hs1 <- ifelse(data0$educ==12, 1,0)
data0$associate1 <- ifelse(data0$educ==14,1,0)
data0$bachelor1 <- ifelse(data0$educ==16, 1,0)
data0$master1 <- ifelse(data0$educ==18,1,0)

#another way to make dummies which is easier, is using the package: fastDummies
install.packages("fastDummies")
library('fastDummies')
data0 <- dummy_cols(data0, select_columns = 'educ')
View(data0)
