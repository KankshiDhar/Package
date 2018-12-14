#Loading the data in R as comma separated values and storing in #variable 'Dataset'

Dataset <- read.csv(file.choose())

#Function file.choose() is a default function to open the Windows #Explorer and 
#let's the user choose the required file We then choose the file #INFM600_CleanedLA.csv 
#and use the below command head() to confirm that the data has #correctly loaded which returns
#first six values of all variables. The summary() function gives #the aggregate data, most frequently travelled 
#stations with their respective max and min. duration and average #time of trip and most frequent trip quality
#pass holder type by trip category (One way/Round trip)

View(Dataset)
head(Dataset)
summary(Dataset)


#We have three research questions to predict trends in different #cities for Bicycle sharing
# the main one being; How can Seattle create the best bike share #system based on trends from New York, Chicago and Los Angeles?
#(Variables: Trip Duration in Seconds, Plan Duration, Start #Station, End Station)
#For the above variables we plot the histograms and check the #normality of the distribution. 
#The central tendency for a normally distributed variable would be  
#mean and the central tendency of a skewed distribution would be #median
#We plot the histogram using the hist(dataset$variable) command

hist(Dataset$durationInSeconds) #Right skewed data and Unimodal
hist(Dataset$plan_duration)     #Right skewed data and Unimodal with an outlier

#Inspecting the histograms for the above variables, we see that #they are skewed in nature
#with the extremes dwindling to the left and hence to understand the data better and the most common
#trip duration and plan duration relationsips we calculate the mean as central tendency
#median and standard deviation for each variable using commands #such as 
#mean(variable), median(variable) and sd(variable). We also add #na.rm=TRUE to remove null values present
#in the dataset so as to draw actual means and deviations without #null values skewing results
#Here our purpose of finding descriptive statistics of the Trip duration is to analyze the average trip length of a user
#and plan duration statistics to analyze the average plan duration of a user, which has come up to be 28.55 which means
#an average user takes a monthly plan and makes a trip of 36.7 minutes

mean(Dataset$durationInSeconds, na.rm = TRUE)    #value = 2207.17 = 36.7 minutes
sd(Dataset$durationInSeconds, na.rm = TRUE)      #value = 7383.015 = 123.05 minutes = roughly 4 hours
median(Dataset$durationInSeconds, na.rm = TRUE)  #value = 720 = 12 minutes

mean(Dataset$plan_duration, na.rm = TRUE)        #value = 28.55 days
sd(Dataset$plan_duration, na.rm = TRUE)          #value = 62.16511 days
median(Dataset$plan_duration, na.rm = TRUE)      #value = 30 days


#We use the plot function to determine the relationship between #trip duration and 
#plan duration of users in the dataset with the x-axis having Trip #Duration and the y-axis having Plan Duration.
#We will be using linear regression to determine trends occuring #in present dataset 
#So we make as close to accurate conclusions as possible to #predict for Seattle bike sharing
#We will convert plan duration from minute to seconds and trip #duration from hours/minutes to seconds 
#in future to maintain relationship analysis. Linear Regression is #used to describe data and 
#to explain the relationship between one dependent variable and #one or more independent variables.
#At the center of the regression analysis is the task of fitting a #single line through a plot.

scatter.smooth(Dataset$durationInSeconds, Dataset$plan_duration)


#Since we can see that the relationship between plan and trip #duration is connected but not exponentially
#we will delve further and determine if they have any correlation #at all
#We wil use the cor() function for this purpose. This computes the #correlation of x and y 
#if these are vectors. If x and y are matrices then the #covariances (or correlations) between 
#the columns of x and the columns of y are computed.

cor(Dataset$durationInSeconds, Dataset$plan_duration) 

#value = -0.071 implying the relationship is almost linear and strongly correlated implying that the 
#users trip duration and plan duration have a relationship. It might be that users who have monthly plans
#are taking longer trips or vice versa. We will analyze this in future.


#We now attempt to build linear regression model on full data #using the lm() function.
#We use this to understand how much our predictor variable (Duration in seconds) affects our response variable(plan_duration)
#We also use the summary() function to find the Residual standard #error, Significance codes and residual 
#standard error

linearMod <- lm(Dataset$durationInSeconds~Dataset$plan_duration) 
print(linearMod)   #Intercept = 2449.323 and Plan duration = -8.479
summary(linearMod) #Standard Error = 7364 on 77355 degrees of freedom which is a 0.09% standard error.


#Further analysis of the reseach question has to be done by #assigning weights to all trip durations from all datasets 
#inspecting their effect on Plan duration, Station from and #Station to. Team MAKers continues to work 
#on how to analyze further 
