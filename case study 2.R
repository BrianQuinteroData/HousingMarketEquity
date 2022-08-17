library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
theme_set(theme_pubr())


#San Jose State University
#Case Study 2 , Medical2
#Group 1
#Bus2 194A - Dr. Etu, Online

# Medical Study # 2

# Step 0: Hypothesis
#H0: u1 = u2 = u3 (all populations are equal)
#Ha: all populations are not equal


# Step 1: Import data from excel file
med2_df <- read.csv("Medical2_Stacked.csv")
head(med2_df)

#Interpretation of the descriptive statistics below. Each categorical variable (i.e. City (Florida, New York, North Carolina)) has 20 observations.
#The quantitative variable "Test Scores" has a numeric summar with mean = 14.57 and median = 14. On average the number of recorded elderly with
#underlying health conditions who tested for depresssion is 60.
#summary
chem_df <- read.csv("Medical2_Stacked.csv", header = TRUE, colClasses = c("factor", "numeric")) #This tells R that you've categorical/numeric data
summary(chem_df) #This should output the proper summary of the data

#Data Visualization
boxplot(Test.Score ~ City, data = med2_df, main = "Depression Test 2", 
        xlab = "City", ylab = "Depression w/ Chronic Illness")

#ANOVA TEST
med2_anova <- aov(Test.Score ~ City, data = med2_df)
summary(med2_anova)

#Interpretation:
#(1) The ANOVA test shows us the variance (MSE) in the data as 11.925
#(2) The p-value is 0.494 which is greater than 0.1, which means we do not have enough evidence to reject the null.
#Therefore, we assume the population proportions are equal.

#Assumptions Test #1
#(1) Check for homogeneity of variance
#Graphical analysis to check for variance using the Residuals vs Fitted plot
plot(med2_anova, 1)
#Interpretation: Points 16, 38 and 17 are detected outliers, which can severely affect the normality and variance assumptions.
#It can be useful to remove outliers from the data to meet the test assumptions.

#(2) Levene's Test to check for variance
library(car)
leveneTest(Test.Score~City, data=med2_df)
#Interpretation:
#From Levene's Test we can see that the p-value 0.4662 is not less than the level of significance 0.1. This means there
#is no evidence to suggest that the variance across methods is statistically significantly different. Therefore, we can 
#assume the homogeneity of variances in the different treatment groups.

#Assumption Test #2
#(1) Check for normality using normal probability plot of residuals (Normal Quantile plot = Q-Q Plot)
plot(med2_anova, 2)
#Interpretation:
#The points on this plot does in fact follow a straight line, we can can assume normality

#(2) Perform second test to confirm normality using Shapiro-Wilk
med2_residuals <- residuals(object=med2_anova)
# Run Shapiro-Wilk test
shapiro.test(x=med2_residuals)
#Interpretation
# w = 0.99 , p-value = 0.65 which indicates that the normality assumption is not violated.

