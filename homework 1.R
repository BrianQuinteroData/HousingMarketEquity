

##2 

flight_df <- read.csv("C:/Users/Brian/Downloads/Q2 (1).csv") #Save the data into a dataframe called bank_df
#Make sure the data is saved as a csv on your desktop or else it won't read
flight_df


Current.Yeardf <- subset(flight_df, select = ï..Current.Year) #This enables us to focus only on current year
Current.Yeardf

Previous.Yeardf <- subset(flight_df, select = Previous.Year) #This enables us to focus only on previous year
Previous.Yeardf



Current.Year <- Current.Yeardf$ï..Current.Year # The dollar sign helps us to call a particular feature in the data
Current.Year #Outputs a vector to enable us do some calculations. Vector is just an output of the data structure
Previous.Year <- Previous.Yeardf$Previous.Year
Previous.Year

mean(Current.Year)
mean(Previous.Year)
#mean of Current.Year 487, mean of Previous.Year  464

d = Current.Year - Previous.Year
d

d_mean = mean(d)
d_mean

d_std = sd(d)
d_std




test <- t.test( Current.Year, Previous.Year, paired = TRUE, alternative = "greater", conf.level = 0.95) # Computes the t statistics
list(test)

p-valueC <- pnorm(test_Z, lower.tail= TRUE)

#QUESTION 2 PROBLEM 1
#Ho :M1-M2 = 0
#Ha :M1-M2 /= 0
##p-value = 0.03229
### t-value 2.0536
#### Interpretation : reject null since p value is less than .05

mean1 = mean(Current.Year)
mean1
mean2 = mean(Previous.Year)
mean2
##2: mean of Current.Year 487, mean of Previous.Year  464

Percentage = (( mean1 - mean2) / mean2) * 100
Percentage

###3: the percentage change in the airfare for the one-year period is approximately 5% (4.956897)






##Problem 3 


#step 0 
##Ho : u1 - u2 = 0
####Ha : u1- u2 /= 0

# Step 1 - we import the dataset (i.e., an excel file)
school_df <- read.csv("C:/Users/Brian/Downloads/Q3.csv") #Save the data into a dataframe called school_df
#Make sure the data is saved as a csv on your desktop or else it won't read
school_df #Outputs the dataframe here helps us to view the imported data

# Step 2 - data cleaning. 
#After viewing our data in step 1, we see there's some missing points
#We've to clean and address those missing points in Private Flights
private_df <- subset(school_df, select = ï..Private) #This enables us to focus only on private school
private_df #Outputs only private school scores

public_df <- subset(school_df, select = Public) #This enables us to focus only on public schools
public_df

public_df <- na.omit(public_df) #na.omit helps us to delete those rows that contain NA as a value
public_df

private_df <- na.omit(private_df) #na.omit helps us to delete those rows that contain NA as a value
private_df #Outputs only the rows with data in privatebranch

# Setp 3 - we need to create a vector that stores the individual features 
private <- private_df$ï..Private # The dollar sign helps us to call a particular feature in the data
private #Outputs a vector to enable us do some calculations. Vector is just an output of the data structure
public <- school_df$Public
public#Outputs only the cherry branch location data

mean1 = mean(private) 
mean1

mean2 = mean(public) 
mean2

Private_sd <- sd(private)
Private_sd

public_sd <- sd(public)
public_sd

##1 The Mean for public schools is 42.5,
##The mean for private schools is 22.3
##The standard deviation for Public school is 4.532308
##The standard deviation for Private school is 6.980608


point_estimate = mean1 - mean2
point_estimate

##2 :  Point estimate is 20.2

# Step 4 - Compute the test statistics
#Note - the "t.test" function helps us to run the t statistics on the given data
test <- t.test(private, public, paired = FALSE, alternative = "two.sided", conf.level = 0.95) # Computes the t statistics
list(test) # Outputs a summary result of the t statistics
##3: We are 95% confident that there is a price difference for attending private and public colleges between 14.7282(in thousands) and 25.6718(in thousands)


