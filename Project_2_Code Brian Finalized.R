install.packages(c("ggplot2","ggpubr","tidyverse", "broom"))
##Interpret the $$ amount

#1.296e is less than .05 we reject the null hyppthesis

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)

#San Jose Housing Single, Town, Condo Median Average

#Step 0: Define Hypothesis
#H0: u1 = u2 = u3
#Ha: Not all the population means are equal
# u1 = Mean Number of Single Family Home $
# u2 = Mean Number of Townhouse $
# u3 = Mean Number of Condo $

Sjhomes_df <- read.csv("C:/Users/Brian/Downloads/Project_2_Data.csv", colClasses = c("factor", "numeric"))
Sjhomes_df

# Step 1: Descriptive Stat Geographical Depression
summary(Sjhomes_df)
# Interpretation:
# The categorical variable (i.e, Condo, Single Family Home, Townhouse)
# have 5 observations each. They represent the average median from years 2016 to 2020 in the San Jose Area
# The quantitative variable "Pricing.Avg.Med" has a numeric summary.
# with mean = 699.9 and median 697.7.
# The average home in San Jose from recent history would be listed at $699,900.

# Step 2: Data Visualization using Box plot
boxplot(Pricing.Avg.Med ~ Home.Type, data = Sjhomes_df, main = "San Jose Home Pricing 2016-2020", 
        xlab ="", ylab = "Equity in Thousands")
# Interpretation:
# It is observed from the plot that the median values of all three home types 
# are very different as the boxes do not touch eachother. 
# There are no outliers. The graph suggests that Single Family Homes 
# have retained the highest equity in the past 4 years. 
# Condos have the smallest pricing in the city.

# The boxplot shows with the dotted lines that you can purchase a
# Single Family Home at a low price and sell at a higher value when compared
# to the difference in margin between Townhouse and Condo.

#Step 3: ANOVA Test 
Sjhomes_anova <- aov(Pricing.Avg.Med ~ Home.Type, data = Sjhomes_df)
summary(Sjhomes_anova)
# Interpretation:
# From figure 3, the ANOVA test shows us the variance (MSE) in the data as 6590. 
# The standard deviation is the square root of MSE, which is 81.179. 
# The p-value is greater than our alpha, so we do not reject the null hypothesis.

# Step 4: Conclusion from ANOVA
# Interpretation:
# We can conclude that there are significant differences between the 
# three cities' test scores on depression. They all seem to have depression scores 
# within the same ballpark. It may be useful to test another variable, other than 
# city, to find a significant variable.

plot(Sjhomes_anova, 1)
#Interpretation:
# Outliers are 7 and 13 are detected in the plot, which can severely effect the
# normality and the variance assumptions. It can be useful to take those out.
# However, the data seems 

library(car)
leveneTest(Pricing.Avg.Med ~ Home.Type, data = Sjhomes_df)
#Interpretation:
# Since our P-value, 0.2834, is over our alpha of 0 our data set meets the variance assumption. 
# There is no evidence to suggest that the variance across  is statistically 
# significantly different. This means that the variance for all three are the same.

#The Shapiro-Wilk test on the ANOVA residuals is used to confirm normality
#Extract the residuals
Sjhomes_residuals <- residuals(object = Sjhomes_anova)
Sjhomes_residuals
#Run Shapiro-Wilk test 
shapiro.test(x = Sjhomes_residuals)
#Interpretation: W=.95077, p = .5367 whioich indicates that the normality assumption is validated

#Multiple Comparison procedure t test
#The pairwise.t.test function helps us to perform the F
pairwise.t.test(Sjhomes_df$Pricing.Avg.Med,Sjhomes_df$Home.Type, p.adjust.method = "bonf")

#Another Multiple comparisson test is Tukey's Honest Significant Differences test 
TukeyHSD(Sjhomes_anova,p.adjust.methods = "bonf")

#Interpretation A: With an Alpha value of .05 (P-value =.0000009<.05) we can reject the null hypothesis that
#the mean avg equity produced between Single Family Home is equal to the mean avg equity produced by condos 

#Interpretation B: With an Alpha value of .05 (P-value =.0067641<.05) we can reject the null hypothesis that
#the mean avg equity produced between Townhouse is equal to the mean avg equity produced by condos 

#Overall interpretation: In effect, our conclusion is that the population means for method A and Method B both differ from the population mean for Method C

