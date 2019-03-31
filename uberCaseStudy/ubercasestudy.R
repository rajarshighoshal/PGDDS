
library(ggplot2)
library(dplyr)
library(lubridate)

######################################################################################
######################## loading data in r environmant #############################
####################################################################################
uberData <- read.csv("Uber Request Data.csv", stringsAsFactors = F)
View(uberData)

##################################################################################
################### checking for na and duplicates ###############################
##################################################################################
#Check for duplicate values
sum(duplicated(uberData$Request.id))
" no duplicates "

#Check for NA values
sum(is.na(uberData$Request.id)) 
sum(is.na(uberData$Pickup.point))
sum(is.na(uberData$Status))
sum(is.na(uberData$Request.timestamp))
" no na in these crucial columns as well "

####################################################################################
###################### dealing with request and drop timestamp ####################
####################################################################################
## data has differnt types of date format
## replacing "/" with "-" in timestamp 
uberData$Request.timestamp <- gsub("/", "-", uberData$Request.timestamp)
uberData$Drop.timestamp <- gsub("/", "-", uberData$Drop.timestamp)
## adding :00 in second's position where it is missing
uberData$Request.timestamp <- gsub("^([0-9]+-[0-9]+-[0-9]+ [0-9]+:[0-9]+)$", "\\1:00", uberData$Request.timestamp)
uberData$Drop.timestamp <- gsub("^([0-9]+-[0-9]+-[0-9]+ [0-9]+:[0-9]+)$", "\\1:00", uberData$Drop.timestamp)

## formatting timestamp into datetime format
uberData$Request.timestamp <- as.POSIXlt(uberData$Request.timestamp, format = "%d-%m-%Y %H:%M:%S", tz = )
uberData$Drop.timestamp <- as.POSIXlt(uberData$Drop.timestamp, format = "%d-%m-%Y %H:%M:%S")

## adding time taken variable for each completed trip
# calculate difference detween request time and drop time for each request in minutes
uberData$timeTaken <- difftime(uberData$Drop.timestamp, uberData$Request.timestamp)

###################################################################################
############### getting date and time in different columns ########################
###################################################################################
## requesting info 
uberData$RequestDay <- as.Date(format(uberData$Request.timestamp, "%Y-%m-%d"))
uberData$RequestWDay <- weekdays(uberData$RequestDay) ## weekday can be useful
uberData$RequestTime <- format(uberData$Request.timestamp, "%H:%M")  ## dropping second as it is insignificant
uberData$RequestHour <- format(uberData$Request.timestamp, "%H") ## hour can be useful
## dropping info 
uberData$DropDay <- as.Date(format(uberData$Drop.timestamp, "%Y-%m-%d"))
uberData$DropWDay <- weekdays(as.Date(format(uberData$Drop.timestamp, "%d-%m-%Y"))) ## weekday can be useful
uberData$DropTime <- format(uberData$Drop.timestamp, "%H:%M")  ## dropping second as it is insignificant
uberData$DropHour <- format(uberData$Drop.timestamp, "%H") ## hour can be useful

################### ANALYSIS FOR IDENTIFYING THE PROBLEM ########################

##################################################################################
############### plots to gain insights ###########################################
##################################################################################
# groupby hour to get idea about number of requests according to hour
dataForGrouping <- uberData[ , !(names(uberData) %in% c("Request.timestamp", "Drop.timestamp"))] 
df <- dataForGrouping %>% group_by(RequestHour, Status)%>% summarise(reqCount = n())
## hourly line graph
ggplot(df,aes(x = as.numeric(RequestHour), y = reqCount, color = Status)) +
  geom_line() + xlab("Hour") + ylab("No. of requests")
" between 17 and 23 maximum no car avilable; between 3 and 10 high cancelled"
## bar plot for better assesment
ggplot(uberData ,aes(x = RequestHour,fill = Status)) + geom_bar(position = "dodge") +
    xlab("Hour") + ylab("No. of requests")
" between 00 and 03 hours one segment; between 04 and 09 another; between 10 and 16
  another; between 17 and 23 another. So four segments for proer analysis"
" segment 01 : 00 - 03 hours "
" segment 02 : 04 - 09 hours "
" segment 03 : 10 - 16 hours "
" segment 04 : 17 - 23 hours "
## plots for to and from airport by hour
df <- dataForGrouping %>% group_by(RequestHour, Pickup.point, Status) %>% summarise(reqCount = n())
## line graph (hourly)
ggplot(df,aes(x = as.numeric(RequestHour), y = reqCount, color = Pickup.point)) +
    geom_line() + xlab("Hour") + ylab("No. of requests")
" 03 to 09 high demand towards airport; 17 to 23 high demand from airport"
## bar plot for better assesment
ggplot(uberData, aes(x = as.factor(RequestHour), fill = Pickup.point)) + 
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests")
" so we can clearly visualize that between 4am and 9am there is very high 
  demand towards airport from city. And, between 5pm to 10pm there is very 
  high demand towards city from airport. So, between 4am and 9am there is 
  high demand of uber from city to airport but no demand at all from airport 
  to city and the opposite id true between 5pm to 10pm. Around  11pm there is 
  almost same demand from city and airport. "
## day wise plot
ggplot(uberData, aes(x = as.factor(RequestWDay), fill = Pickup.point)) + 
  geom_bar(position = "dodge") + xlab("Day of the week") + ylab("No. of requests")
" There are no significant changes between demand with variation of days "
## day and hour wise plot
## pickup point
ggplot(uberData, aes(x = as.factor(RequestHour), fill = Pickup.point)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~uberData$RequestWDay, nrow = 5, ncol = 1)
## status
ggplot(uberData, aes(x = as.factor(RequestHour), fill = Status)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~uberData$RequestWDay, nrow = 5, ncol = 1)
" We can see that variation of demand across hours everyday irrespective of 
  the day is almost same. So, hour of day much more important factor than 
  that of the day "

#################################################################################
###### plots to get idea about cancelled or not availablility ####################
#################################################################################
## hourly basis plot to understand when trip get calcelled or car not available
df <- dataForGrouping %>% group_by(RequestHour, Pickup.point, Status) %>% summarise(reqCount = n())
## line plot hourly basis with status and pickup location
ggplot(df,aes(x = as.numeric(RequestHour), y = reqCount, color = Status)) +
  geom_line() + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~df$Pickup.point, nrow = 2, ncol = 1)
" Now it is clear cancelled is much more common from city to airport during
  03 and 10 hours; AND not available is much frequent from airport to city during
  17 and 23 hours. "
## bar plot for more calrity
ggplot(uberData, aes(x = as.factor(RequestHour), fill = Status)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~uberData$Pickup.point, nrow = 2, ncol = 1)
" Between hour 03 to 10 there are many trips canclled from city to airport.
  Between hour 17 to 23 there are shortage of car from airport to city.
  So between 4am to 9am supply-demand gap from city to airport.
  And, between 5pm to 11pm supply-demand gap from airport to city. "
## daily basis plot for the same
ggplot(uberData, aes(x = as.factor(RequestWDay), fill = Status)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~uberData$Pickup.point, nrow = 2, ncol = 1)
" Though variation throughout days are not significant but high 
  no car avilable throughout days from airport to city is probably 
  for some reason. Also, cancelled seems quite high from city to airport
  throughout days which also might have some reasons."
#################################################################################
############ situation between 5pm and 12am hours ##################################
#################################################################################
# let's focus on hours between 5pm and 12am
subData <- subset(uberData, as.numeric(RequestHour) >= 17)
## overall view
df <- subset(dataForGrouping, as.numeric(RequestHour) >= 17) %>% group_by(RequestHour, Pickup.point, Status) %>% summarise(reqCount = n())
## line plot hourly basis with status and pickup location
ggplot(df,aes(x = as.numeric(RequestHour), y = reqCount, color = Status)) +
  geom_line() + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~df$Pickup.point, nrow = 2, ncol = 1)
" clearly airport is affected by no car available mainly "
## bar graph for more clarity
ggplot(subData, aes(x = as.factor(RequestHour), fill = Status)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~RequestWDay, nrow = 5, ncol = 1)
# only from airport
ggplot(subData[subData$Pickup.point == "Airport", ], aes(x = as.factor(RequestHour), fill = Status)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~RequestWDay, nrow = 5, ncol = 1)
" We can observe that no cars available is quite high in those hours  from airport "
# only from city
ggplot(subData[subData$Pickup.point == "City", ], aes(x = as.factor(RequestHour), fill = Status)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~RequestWDay, nrow = 5, ncol = 1)
" On the other hand number of trip completed from city is quite high"
# number of requests per hour from city and airport in those hours
ggplot(subData, aes(x = as.factor(RequestHour), fill = Pickup.point)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~RequestWDay, nrow = 5, ncol = 1)
" Number of requests from airport is higher compared to that of city"
# no. of requests per hour from airport
nrow(subData[subData$Pickup.point == "Airport",])/24-17
"69.71 per hour"
# no. of requests per hour from city
nrow(subData[subData$Pickup.point == "City",])/24-17
"14.63 per hour"
" So, there are not enough cars going from city to airport to fulfill the demand
  between 5pm and 12am. As a result not availability of car is very high. This is
  effectively increasing not availability of cars throughout the day as it seems 
  many planes are landing between 5pm and 12am in the airport "
#################################################################################
################ situation between 4am and 10am ##################################
#################################################################################
# let's focus on hours between 4am and 10am
subData <- subset(uberData, as.numeric(RequestHour) >=  4 & as.numeric(RequestHour) <= 09)
## grouping for line plot
df <- subset(dataForGrouping, as.numeric(RequestHour) >=  4 & as.numeric(RequestHour) <= 09) %>% 
    group_by(RequestHour, Pickup.point, Status) %>% summarise(reqCount = n())
## line plot hourly basis with status and pickup location
ggplot(df,aes(x = as.numeric(RequestHour), y = reqCount, color = Status)) +
  geom_line() + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~df$Pickup.point, nrow = 2, ncol = 1)
" clearly city is affected by cancelled "
## bar plot for better idea
ggplot(subData, aes(x = as.factor(RequestHour), fill = Status)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~RequestWDay, nrow = 5, ncol = 1)
# only from airport
ggplot(subData[subData$Pickup.point == "Airport", ], aes(x = as.factor(RequestHour), fill = Status)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~RequestWDay, nrow = 5, ncol = 1)
" We can observe that number of trip completed is quite high in those hours  from airport "
# only from city
ggplot(subData[subData$Pickup.point == "City", ], aes(x = as.factor(RequestHour), fill = Status)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~RequestWDay, nrow = 5, ncol = 1)
" On the other hand mant trips are getting cancelled from the city"
# number of requests per hour from city and airport in those hours
ggplot(subData, aes(x = as.factor(RequestHour), fill = Pickup.point)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~RequestWDay, nrow = 5, ncol = 1)
" Number of requests from city is quite high compared to that of the airport"
# no. of requests per hour from airport
nrow(subData[subData$Pickup.point == "Airport",])/10-04  
"45.8 per hour"
# no. of requests per hour from city
nrow(subData[subData$Pickup.point == "City",])/10-04 
"176.8 per hour"

" So, it is clear from the graphs that number of cancelled trips from city
  between 4am to 11am and the number of no car available between 5pm and 12am
  is the major problem. "
################################################################################
####### analysis between 12am and 4am plus 10am and 5pm ########################
################################################################################
##### let's focus on hours between 12am and 4am ##############################
subData <- subset(uberData, as.numeric(RequestHour) >=  0 & as.numeric(RequestHour) < 4)
## grouping for line graph
df <- subset(dataForGrouping, as.numeric(RequestHour) >=  0 & as.numeric(RequestHour) < 4) %>% 
  group_by(RequestHour, Pickup.point, Status) %>% summarise(reqCount = n())
## line plot hourly basis with status and pickup location
ggplot(df,aes(x = as.numeric(RequestHour), y = reqCount, color = Status)) +
  geom_line() + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~df$Pickup.point, nrow = 2, ncol = 1)
" no cars available is high for city and airport both "
## bar plot
ggplot(subData, aes(x = as.factor(RequestHour), fill = Status)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~RequestWDay, nrow = 5, ncol = 1)
# only from airport
ggplot(df[df$Pickup.point == "Airport", ],aes(x = as.numeric(RequestHour), y = reqCount, color = Status)) +
  geom_line() + xlab("Hour") + ylab("No. of requests")
# bar plot
ggplot(subData[subData$Pickup.point == "Airport", ], aes(x = as.factor(RequestHour), fill = Status)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~RequestWDay, nrow = 5, ncol = 1)
" We can observe that no cars avilable is quite high in those hours from airport 
  and there is not any cancellation from airport to city "
# only from city
ggplot(df[df$Pickup.point == "City", ],aes(x = as.numeric(RequestHour), y = reqCount, color = Status)) +
  geom_line() + xlab("Hour") + ylab("No. of requests")
# bar plot
ggplot(subData[subData$Pickup.point == "City", ], aes(x = as.factor(RequestHour), fill = Status)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~RequestWDay, nrow = 5, ncol = 1)
" We can observe that no cars avilable is quite high in those hours from city too 
  and cancellation is quite low."
# number of requests per hour from city and airport in those hours
ggplot(df,aes(x = as.numeric(RequestHour), y = reqCount, color = Pickup.point)) +
  geom_line() + xlab("Hour") + ylab("No. of requests")
" Number of requests from city and airport is mostly comperable "
# no. of requests per hour from airport
nrow(subData[subData$Pickup.point == "Airport",])/04-00  
"45.25 per hour"
# no. of requests per hour from city
nrow(subData[subData$Pickup.point == "City",])/04-00 
"48.5 per hour"
## comparing total no of requests and status in plots
## line
ggplot(df,aes(x = as.numeric(RequestHour), y = reqCount, color = Status)) +
  geom_line() + xlab("Hour") + ylab("No. of requests")
## bar
ggplot(subData, aes(x = as.factor(RequestHour), fill = Status)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests")
" No cars avilable is quite high in night hours; and cancellation rate
  is pretty low"
## comparing requests from airport and city
ggplot(subData, aes(x = as.factor(RequestHour), fill = Pickup.point)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests")
## day wise
ggplot(subData, aes(x = as.factor(RequestHour), fill = Pickup.point)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~RequestWDay, nrow = 5, ncol = 1)
" In the the hour of 2am city requests are comparatively high and when done 
  day wise analysis the reason is comperatively higher city requests on Wednesday,
  Thursday and Friday. "
#############################################################################
################ let's focus on hours between 10am and 5pm ##################
#############################################################################
subData <- subset(uberData, as.numeric(RequestHour) >  09 & as.numeric(RequestHour) < 17)
## grouping for line graph
df <- subset(dataForGrouping, as.numeric(RequestHour) >  09 & as.numeric(RequestHour) < 17) %>% 
  group_by(RequestHour, Pickup.point, Status) %>% summarise(reqCount = n())
## line plot hourly basis with status and pickup location
ggplot(df,aes(x = as.numeric(RequestHour), y = reqCount, color = Status)) +
  geom_line() + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~df$Pickup.point, nrow = 2, ncol = 1)
" around 10am from city to airport cancellation is still high "
# only from airport
ggplot(df[df$Pickup.point == "Airport", ],aes(x = as.numeric(RequestHour), y = reqCount, color = Status)) +
  geom_line() + xlab("Hour") + ylab("No. of requests")
# bar plot
ggplot(subData[subData$Pickup.point == "Airport", ], aes(x = as.factor(RequestHour), fill = Status)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~RequestWDay, nrow = 5, ncol = 1)
" We can see higher rate of trip completion "
# only from city
ggplot(df[df$Pickup.point == "City", ],aes(x = as.numeric(RequestHour), y = reqCount, color = Status)) +
  geom_line() + xlab("Hour") + ylab("No. of requests")
" 10 am is showing anomaly in case of city "
## bar plot
ggplot(subData[subData$Pickup.point == "City", ], aes(x = as.factor(RequestHour), fill = Status)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~RequestWDay, nrow = 5, ncol = 1)
" We can observe that except 10am trip completion rate is high as well "
# number of requests per hour from city and airport in those hours
ggplot(subData, aes(x = as.factor(RequestHour), fill = Pickup.point)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests") +
  facet_wrap(~RequestWDay, nrow = 5, ncol = 1)
" Except from 10am Number of requests from city and airport is mostly comperable "
# no. of requests per hour from airport
nrow(subData[subData$Pickup.point == "Airport",])/17-10  
"18.11 per hour"
# no. of requests per hour from city
nrow(subData[subData$Pickup.point == "City",])/17-10 
"33.88 per hour" ### this is for the anomaly of 10am
## comparing total no of requests and status in plots
ggplot(subData, aes(x = as.factor(RequestHour), fill = Status)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests")
" Trip completed status is high. At 10 am cancelled and no cars avilable is high as well "
## comparing requests from airport and city
ggplot(subData, aes(x = as.factor(RequestHour), fill = Pickup.point)) +
  geom_bar(position = "dodge") + xlab("Hour") + ylab("No. of requests")
" Around 10 am city has high margin of request. Except that other hours are 
  comperable. Though throughout the day city has higher number of request 
  compared to airport"


################################################################################
########## finding correlations ################################################
################################################################################
ggplot(data = subset(uberData, !is.na(DropHour)), aes(x = RequestHour, y = DropHour)) + geom_point()
" as usual requesthour and drophour are correlated "
# plot graphs of time taken 
min(uberData$timeTaken, na.rm = T) ## 20.83 mins
max(uberData$timeTaken, na.rm = T) ## 83 mins
mean(uberData$timeTaken, na.rm = T) ## 52.41 mins
median(uberData$timeTaken, na.rm = T) ## 52.08 mins
## data is almost evenly distributed
ggplot(uberData, aes(x = as.numeric(timeTaken))) +
    geom_histogram(breaks=seq(20, 85, by = 5), col="red", fill = "blue", na.rm = T) +
    xlab("Time taken to complete trip") + ylab("No. of trips") 
" most of the trip takes 30 to 75 minutes"
## drawing graph to understand timetaken and requesthour correlation
df <- dataForGrouping %>% group_by(RequestHour, Pickup.point) %>% summarise(avgTripTime = median(as.numeric(timeTaken), na.rm = T))
## line graph for request hour vs time taken
ggplot(df,aes(x = as.numeric(RequestHour), y = avgTripTime, color = Pickup.point)) +
  geom_line() + xlab("Request Hour") + ylab("Average trip completion time") 
## column graph for request hour vs time taken
ggplot(df,aes(x = as.numeric(RequestHour), y = avgTripTime, fill = Pickup.point)) +
  geom_col(position = "dodge", na.rm = T) + xlab("Request Hour") +
  ylab("Average trip completion time") 
" surprisingly time taken between 12am and 1:30am is sightly higher than that
  of nomal. And there not much variation of time taken throughout the hours "
## day wise
df <- dataForGrouping %>% group_by(RequestHour, Pickup.point, DropWDay) %>% summarise(avgTripTime = median(as.numeric(timeTaken), na.rm = T))
## plot
ggplot(df[!is.na(df$DropWDay), ],aes(x = as.numeric(RequestHour), y = avgTripTime, fill = Pickup.point)) +
  geom_col(position = "dodge", na.rm = T) + xlab("Request Hour") +
  ylab("Average trip completion time") + facet_wrap(~DropWDay, nrow = 5, ncol = 1)
" not much variatiion with the variation of days "

##################### ADDRESSING THE PROBLEM PROPERLY ###########################

#### making time slots for better analysis ####
# converting request hour and drop hour to numeric
uberData$RequestHour <- as.numeric(uberData$RequestHour)
uberData$DropHour <- as.numeric(uberData$DropHour)

" segment 01 : 00 - 03 hours : Early_hours "
" segment 02 : 04 - 09 hours : Morning "
" segment 03 : 10 - 16 hours : Day_Time "
" segment 04 : 17 - 23 hours : Night "

## time slots
uberData$timeSlot <- ifelse(uberData$RequestHour < 4, "Early_hours", ifelse(uberData$RequestHour < 10,"Morning",ifelse(uberData$RequestHour < 17,"Day_Time","Night")))
## finding the number of trips made in each slot
nrow(subset(uberData, uberData$timeSlot == "Early_hours"))
" 375 "
nrow(subset(uberData, uberData$timeSlot == "Morning"))
" 2306"
nrow(subset(uberData, uberData$timeSlot == "Day_Time"))
" 1224 "
nrow(subset(uberData, uberData$timeSlot == "Night"))
" 2840 "
## plotting and identifying the most critical problems before Uber
ggplot(uberData, aes(x = as.factor(timeSlot), fill= as.factor(Status))) +
    geom_bar(position = "dodge") + 
    labs(x = "Time Slot", y = "Number of Requests", fill = "Status" )
" Most critical problem is cancelled in morning hours and no cars available in night.
  Also one minor problem is no car available in early_hours as well "
######### problem 1: High cancellations in the morning #########
set1 <- subset(uberData,timeSlot == "Morning")
ggplot(set1, aes(x = as.factor(Pickup.point), fill= as.factor(Status))) +
  geom_bar() +labs(x = "Pickup Point", y = "Number of Requests", fill = "Status")
" city has higher request than airport "
ggplot(uberData, aes(x = as.factor(Pickup.point), fill= as.factor(Status))) + 
  geom_bar(position = "dodge") + 
  labs(x = "Pickup Point", y = "Number of Requests", fill = "Status" )
" airport has higher no car available; city has higher cancelled. But cancelled is
  much more severe problem than that of no cars available "
## problem situation by location
nrow(subset(set1, dataset1$Pickup.point == "Airport" & dataset1$Status == "Cancelled"))
" 25 "
nrow(subset(set1, dataset1$Pickup.point == "City" & dataset1$Status == "Cancelled"))
" 869 "
## percentagewise pie chart for city
problem1 <- subset(set1, Pickup.point %in% "City")
ggplot(problem1, aes(x = Pickup.point, fill= as.factor(Status))) + geom_bar() + coord_polar(theta = "y", start=0)+ labs( y = "Number of Requests", x = "", fill = "Status")
" very high cancellation ratio "

## Supply and Demand ##
nrow(subset(set1, set1$Pickup.point == "City" & set1$Status == "Trip Completed"))
" 514 " ## supply
nrow(subset(set1, set1$Pickup.point == "City"))
" 1808 " ## demand
####### problem 2: high no car available during night ######
set2 <- subset(uberData,timeSlot == "Night")
ggplot(set2, aes(x = as.factor(Pickup.point), fill= as.factor(Status))) + geom_bar()+labs(x = "Pickup Point", y = "Number of Requests", fill = "Status" )
" airport has higher number of requests than city "
ggplot(set2, aes(x = as.factor(Pickup.point), fill= as.factor(Status))) + geom_bar(position = "dodge")+labs(x = "Pickup Point", y = "Number of Requests", fill = "Status" )
" very high no car available from airport "
##  problem situtaion by location
nrow(subset(set2, set2$Pickup.point == "Airport" & set2$Status == "No Cars Available"))
" 1457 "
nrow(subset(set2, set2$Pickup.point == "City" & set2$Status == "No Cars Available"))
" 154 "
## percentagewise pie chart for airport
problem2 <- subset(set2, Pickup.point %in% "Airport")
ggplot(problem2, aes(x = Pickup.point, fill= as.factor(Status))) + geom_bar() + coord_polar(theta = "y", start=0) + labs( y = "Number of Requests", x = "", fill = "Status")
" Extremely high percentage of no car available "

## Supply and Demand ##
nrow(subset(set2, set2$Pickup.point == "Airport" & set2$Status == "Trip Completed"))
" 515 " ## supply
nrow(subset(set2, set2$Pickup.point == "Airport"))
" 2081 " ## demand 