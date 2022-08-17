#BUS 194A ASSIGNMENT 2 PROBLEM 3


#install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom"))

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)


#Hypothesis 
##Ho : R1 = R2 = R3 = R4
###Ha : Not all populations are equal

#Step 1
robot_df <- read.csv("C:/Users/Brian/Downloads/PROBLEM_3_CSV.csv")
summary(robot_df)
robot_df
#Interpretation: The categorial Variable Robots(1,2,3,4)has five observations
##


#step 2: Data Visualization

boxplot(Time ~ ï..Robot, data=robot_df, main="Filtration System Analysis", xlab="ï..Robot", ylab="Time")

robot_anova <- aov(Time ~ ï..Robot, data = robot_df)
summary(robot_anova)
robot_anova
#1: The ANOVA test shows us the variance (MSE) in the data set as 43.25
##2: The P-Value is less than .05 so we reject the Ho Hypothesis
##3 Not all Populations means are equal
## We can Conclude that there are significant differences between the 4 Robots and the Time its takes to dry the paint


plot(robot_anova, 1)
#Interpretation: Points 12,18,20 are detected as outliers, which can severly affect the normality and variance assumptions
##It can be useful to remove outliers from the data to meet the tesst assumptions

library(car)
leveneTest(Time ~ ï..Robot, data = robot_df)
#interpretation: from the output we can see that the p-value is not less that the significance level of .05
##this means that there is no eveidence to suggest taht the variance accross methods is statistically signigicantly different.
###Therefore, we can assume the homogeneity of variances in the different treatment groups.
plot(robot_anova, 2)
#interpretation: The points do seem to follow a straight line so we can assume normality
#

robot_residuals <- residuals(object = robot_anova)
robot_residuals

shapiro.test(x = robot_residuals)
#normality assumption is not violated due to p-value being .1724 which is greater that .05 at a 95% confidence interval




#Assignment2 Problem 4
