####################################################################
## Methods II                                                     ##
## Data Lab 3                                                     ##
##                                                                ##
## University of Salzburg                                            ##
## Dr Doug Atkinson                                              ##
####################################################################

###########First things first, clear your work space and set your working directory###########

rm(list = ls())


setwd("C:/Users/HP USER/Desktop/Ola/Lab 3")


#######Install stargazer it makes really nice tables easy

install.packages("stargazer") 
install.packages("calibrate") 
install.packages("texreg") 


#########Here are the packages you will need today##########




library(foreign) 
library(stargazer)

library(readxl)

# -- Your Data -----------------------------------------------------------------

setwd("C:/Users/HP USER/Desktop/Ola/Lab 3")

# These are some of the variables from a survey of British Residents during the Second World War
#If you are interested in historical polling data, the Uni of Salzburg has an online resource called
#Roper iPoll

##Remember we have to assign our data a name

dataset1<-read.csv("wwiisicknew.csv", header=TRUE, sep=",")

###Let's attach the dataset

attach(dataset1)


# the names of the variables
names(dataset1)

summary(dataset1)
View(dataset1)
names(dataset1)
nrow(dataset1)
ncol(dataset1)
dim(dataset1)


# -- Part 1: Descriptive statistics -------------------------------------------- 

# -- Some calculations 
# A nice function to check out data is the summary function 

summary(sick)

summary(gender)
summary(temp)

summary(factory)




# We now find the sample size, mean, variance, standard deviation for days sick per month
n <- length(sick)
n
mean.sick <- mean(sick)
mean.sick
var.sick <- var(sick)
var.sick 
sd.sick <- sd(sick)
sd.sick




# -- Plotting 
## Continuous Variables
# Average temparutre for month poll was taken


hist(temp)
hist(temp, breaks = 40)


# boxplots

boxplot(temp)


## Counts
# take a look at the distributino of your data
table(sick)
# save that result
sick.counts <- table(sick)
# now plot
barplot(sick.counts)





# -- Inference  

# Let us assume that the sample poll takers drew during wwii is representative 
# of the British population . Let us also assume, that any values in that 
# population are normally distributed. 
# Given our sample, find the range of values for which we are 95% certain that 
# it covers the true average value for sleep at night 
# Or put differently: calculate the 95% confidence interval 
# for the hours of sleep per night. 

n <- length(sick)
se.sick <- sd(sick)/sqrt(n)
se.sick

ci.sleep <- c(mean.sick - 1.96*se.sick, mean.sick + 1.96*se.sick)

ci.sleep

# What does the result tell us in substantive terms?
# What makes a confidence intervall smaller/larger? 






# -- Part 2: Two Variables at a Time  ------------------------------------------



## One categorical and one continuous data
table(sick, gender)
boxplot(sick ~ gender)





## Now over to you: Which relationships would you expect to observe in the data?
# Which variables do you expect to co-vary? Why do you expect them to co-vary?



########Let's look at sick days by gender

t.test(sick ~ gender)





##########Let's get into some bivariate regression



## Bivariate Regression:

B1 <- lm(sick  ~  gender )
summary(B1)
stargazer(B1, type="text")

rm(B1) # object B1, which currently contains output of regression analysis gets deleted 


B1.1 <- lm(sick ~  temp )
summary(B1.1)
stargazer(B1.1, type="text")




#3 multiple regressions
#No 1:
M1 <- lm(sick ~ factory + gender + temp  )
summary(M1)
stargazer(M1, type="text")


library(texreg) # Library for displaying model estimates
htmlreg(M1.3, file = "regression.doc", stars = c(0.01,0.05, 0.1))

#Scatterplot with regression line

dataset2<-read.dta("trade1.dta")

attach(dataset2)

#Scatterplot with regression line
library(calibrate)
plot(openc, unemploy, main="Scatterplot Example", xlab="Trade Openness", ylab="Unemployment", pch=19)
textxy(openc, unemploy, labs=country)
abline(lm(unemploy~openc), col="red") # regression line (y~x)





