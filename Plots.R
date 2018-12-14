#This document shares the analysis projected on Los Angeles’ dataset and the same analysis is done on #the other datasets as well. For draft purposes we have taken only one dataset.

#Loading the data in R as comma separated values and storing in variable 'LAFinal'
LAFinal <- read.csv(file.choose())
View(LAFinal)

#Taking count of all values present in the LA dataset
nrow(LAFinal)  #93378 values

#Since we have a primary key of trip id, we try and see if we have any duplicate values and dedupe the #dataset by using the dedupe() function and then calculating the final count of all values present 
Deduped <- unique(LAFinal)
nrow(Deduped)     #93105 values left

#The following plot function uses ggplot to derive a relation between the hours of the day the bikes are #operated and considered months to check if there is any difference of usage in these months
ggplot(LAFinal, aes(LAFinal$Month, LAFinal$Hour))+ geom_count(col="maroon",show.legend = F) + labs(x="Month", y='Hours of Day', title = "Monthly Bike Usage - Hourly Distribution")

#Our first research question for the project is :
#Q1. How can Seattle create the best bike share system based on trends from other cities?

#For the above question, we try to find relationships between various factors such as trip duration, plan #holder types, busiest days of week and busiest stations.The following plot function uses ggplot to derive #a relation between the hours of the day the bikes are operated and days of the week to see a relation #between them and try to find a pattern or relation between the trip duration and the route trip to #understand the which users have round trips and how many have one way trips w.r.t the trip duration #undertaken

ggplot(LAFinal,aes(LAFinal$trip_route_category,LAFinal$duration))+geom_jitter(col="salmon",show.legend = F) + labs(x="Route Type", y='Trip Duration’', title = "Trip Duration and Route Category relation")

#we try and analyze if there is any relation between  busy hours of the day and busy days of the week and #to understand if any of the relationships are consistent or exponential in nature. 

ggplot(LAFinal, aes(LAFinal$WDAY, LAFinal$Hour))+ geom_count(col="maroon",show.legend = F) + labs(x="Day of Week", y='Hour of Day', title = "Weekly Bike Usage - Hourly Distribution")

#Q2. Should Seattle be proposed dockless or docked bicycles?
#Next we try to decipher a pattern between the trip route category and passholder type. This would help #us understand if flexible, weekly or daily passholders use the bicycles and how often are these trips #made. Are they round trips or one way trips? If one way trips are more, we would propose docked #systems.

ggplot(LAFinal,aes(LAFinal$trip_route_category,LAFinal$passholder_type))+geom_count(col="salmon",show.legend = F) + labs(x="Route Category", y="Pass Holder", title = "Trip Duration Based on Passholder Type")

#Q3. How do busy hours affect the Bicycle patterns?
#For this question and the sheer vastness of the probably factors, we have at first taken the summary() #function to find the busiest start and end stations for LA to determine the stations which need to be most #populated with bicycles.
summary(CGFinal)

#Following are additional plots for contextual research and analysis of the system. Overall, the following plots are responsible for relations between
#member type, total bike usage, route type, gender distribution and age distribution along with relation between ridership and temperature as well

ggplot() + geom_col(data=LAFinal, aes(x=LAFinal$Date, y = LAFinal$Duration/360), color="darkcyan") + geom_line(data=LAWeather, aes(x=LAWeather$Date, y = LAWeather$Avg.Temp),size =2, color="red") + labs(x="Month", y="Duration of Trips/ Avg. Temperature", title = "LA - Trip Duration (Mutiple of 6 hours) Distribution vs Temperature Distribution")

ggplot() + geom_col(data=DCFinal, aes(x=DCFinal$Date, y = DCFinal$Duration/1800), color="darkcyan") + geom_line(data=DCWeather, aes(x=DCWeather$Date, y = DCWeather$Avg.Temp),size =2, color="red") + labs(x="Month", y="Duration of Trips/ Avg. Temperature", title = "DC - Trip Duration (Mutiple of 60 hours) Distribution vs Temperature Distribution")

ggplot(LAFinal, aes(LAFinal$Member.type)) + geom_bar(fill = "#008081") +labs(title = "LA - Bike Usage by Customer Type", x="Customer Type", y="Usage Count")

ggplot(DCFinal, aes(DCFinal$Member.type)) + geom_bar(fill = "#008081") +labs(title = "DC - Bike Usage by Customer Type", x="Customer Type", y="Usage Count")

ggplot(CGFinal, aes(CGFinal$Member.type)) + geom_bar(fill = "#008081") +labs(title = "CG - Bike Usage by Customer Type", x="Customer Type", y="Usage Count")

ggplot(LAFinal,aes(LAFinal$Trip.route,LAFinal$Member.type))+geom_jitter(col="#008081",show.legend = F, size = 0.5) + labs(x="Route Category", y="Pass Holder", title = "LA - Membership Distribution based on Route")

ggplot(LAFinal, aes(LAFinal$Month, LAFinal$Hour))+ geom_count(col="maroon",show.legend = F) + labs(x="Month", y='Hours of Day', title = "Monthly Bike Usage - Hourly Distribution")

ggplot(DCFinal, aes(DCFinal$WDAY, DCFinal$Hour))+ geom_count(col="#008081",show.legend = F) + labs(x="Day of Week", y='Hour of Day', title = "DC - Weekly Bike Usage - Hourly Distribution")

ggplot(LAFinal,aes(LAFinal$Member.type,LAFinal$Duration))+geom_jitter(col="#008081",show.legend = F) + labs(x="Member Type", y='Trip Duration', title = "LA - Trip Duration and Membership Relationship")



DCBikesAvailable<-length(unique(DCFinal$Bike.number))
LABikesAvailable<-length(unique(LAFinal$Bike.number))
CGBikesAvailable<-length(unique(CGFinal$Bike.number))

#Summary is used to find most popular spots in the cities and google maps are used to plot these spots for visualization
summary(LAFinal)
#Top 5 popular start stations :
#Ocean Front Walk & Navy               
#7th & Flower                                    
#Ocean Front Walk & North Venice   
#Union Station West Portal
#Main & 1st                            
#Top 5 popular end stations :
#Downtown Santa Monica Expo Line Station 
#Ocean Front Walk & Navy      
#7th & Flower          
#Union Station West Portal            
#Ocean Front Walk & North Venice     



#Following link has the plots that we have generated using these functions and scripts:
# https://drive.google.com/open?id=174rK-AyEufKip4_zndvrjifEuc1t5P4n
#This link is accessible by UMD IDs only
