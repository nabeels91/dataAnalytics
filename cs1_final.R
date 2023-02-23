library(readr)
library(tidyverse)
library(lubridate)
library(base)
library(dplyr)
library(ggplot2)
library(janitor)
library(lubridate)
library(data.table)




c202201 <- read_csv("1_year_of_cyclistic_data/202201-divvy-tripdata.csv")
c202202 <- read_csv("1_year_of_cyclistic_data/202202-divvy-tripdata.csv")
c202203 <- read_csv("1_year_of_cyclistic_data/202203-divvy-tripdata.csv")
c202204 <- read_csv("1_year_of_cyclistic_data/202204-divvy-tripdata.csv")
c202205 <- read_csv("1_year_of_cyclistic_data/202205-divvy-tripdata.csv")
c202206 <- read_csv("1_year_of_cyclistic_data/202206-divvy-tripdata.csv")
c202207 <- read_csv("1_year_of_cyclistic_data/202207-divvy-tripdata.csv")
c202208 <- read_csv("1_year_of_cyclistic_data/202208-divvy-tripdata.csv")
c202209 <- read_csv("1_year_of_cyclistic_data/202209-divvy-publictripdata.csv")
c202210 <- read_csv("1_year_of_cyclistic_data/202210-divvy-tripdata.csv")
c202211 <- read_csv("1_year_of_cyclistic_data/202211-divvy-tripdata.csv")
c202212 <- read_csv("1_year_of_cyclistic_data/202212-divvy-tripdata.csv")

november <- c202211


tripDataCombi <- rbind(c202201,c202202,c202203,c202204,c202205,c202206,c202207,c202208,c202209,c202210,c202212)
trip # this is the backup just incase its needed.

# this is where tripDataCombi goes. order it correctly for ease of use and reuse.
# 
# first thing is to split the time and date..then make sure the date and time columns are in the correct date type.then remove the non conforming rows..split time and date into 2 variables

# Date
tripDataCombi$Start_date <- as.Date(tripDataCombi$started_at)
# times
tripDataCombi$Start_time <- format(as.POSIXct(tripDataCombi$started_at), format = "%H:%M:%S")

tripDataCombi$End_time <- format(as.POSIXct(tripDataCombi$ended_at), format = "%H:%M:%S")

# Duration
tripDataCombi$Duration <- difftime(tripDataCombi$ended_at, tripDataCombi$started_at)

# Hour
tripDataCombi$Hour <- as.numeric(substr(tripDataCombi$Start_time,1,2))

# Day
tripDataCombi$Day <- format(as.Date(tripDataCombi$Start_date), "%A")

# Phase
tripDataCombi$Phase <- cut(tripDataCombi$Hour,breaks = c(0,6,12,18,24),include.lowest =TRUE,labels = c("Night","Morning","Afternoon","Evening"))

# Month
tripDataCombi$Month<- as.numeric(format(tripDataCombi$Start_date,"%m"))

# change start and end time to time format
tripDataCombi$Start_time <- as.ITime(tripDataCombi$Start_time)
tripDataCombi$End_time <- as.ITime(tripDataCombi$End_time)

# removing the na or null or blank values 
tripDataCombi_cleaned<- na.omit(tripDataCombi)

# remove all durations less than or equal to zero seconds..
#  I would ask the owner, what does ti mean to have a 5 second trip? is this a real-life use of service? do they charge by the second? 
# 43,856 removed
filter(tripDataCombi_cleaned,Duration < 0) 

tripDataCombi_cleaned <- 
  tripDataCombi_cleaned %>% filter(Duration > 0 )

# tripDataCombi end

# this is for november sorting to make it consistent with tripDataCombi
# 

november

# Start_date
november$Start_date <- as.Date(format(as.Date(november$started_at, format = "%d/%m/%Y"), "%Y-%m-%d")) 

# Start_time and End_time ::: where have you been all this time!!!!

november$started_at <- dmy_hm(november$started_at,tz=Sys.timezone())
november$ended_at <- dmy_hm(november$ended_at,tz=Sys.timezone())

# Start_time
november$Start_time <- format(as.POSIXct(november$started_at), format = "%H:%M:%S")

# End_time
november$End_time <- format(as.POSIXct(november$ended_at), format = "%H:%M:%S")

# november <- november[, c(1:14,16,17,15)]

november$Duration <- difftime(november$ended_at, november$started_at)

november$Hour <- as.numeric(substr(november$Start_time,1,2))

november$Day <- format(as.Date(november$Start_date), "%A")
n_distinct(november$Day)

# Phase
november$Phase <- cut(november$Hour,breaks = c(0,6,12,18,24),include.lowest =TRUE,labels = c("Night","Morning","Afternoon","Evening"))

november$Month<-as.numeric(format(november$Start_date,"%m"))



november$Start_time <- as.ITime(november$Start_time)
november$End_time <- as.ITime(november$End_time)



# remove na values
november_cleaned <- na.omit(november)

# find and remove durations less than 0 i.e negative durations
november_cleaned <- filter(november_cleaned,Duration > 0) 


tdcc <- rbind(tripDataCombi_cleaned,november_cleaned)


####### End november

# show all na values

colSums(!is.na(tdcc))
colSums(is.na(tdcc)) #rows with na's only 15% of dataframe


# count the number of rows in the table that is not na.. no nulls. 2 ways of doing this.
sum(!is.na(tripDataCombi)) # this is the number of na values total.. not the rows with na some rows can have multiple na's
sum(!complete.cases(tripDataCombi)) # this is for all the rows with na in them.

# 2 ways of selecting all rows with missing values
# 1st way
tripDataCombi[rowSums(!is.na(tripDataCombi)) <=0,]

..............................
# 2nd way - this is obselete.already done.
# new_df2 <-
#   tripDataCombi %>% 
#   mutate(duration= ended_at-started_at,tripDataCombi) %>% 
#   select(everything(),duration) %>% 
#   filter(!complete.cases(.)) %>% 
#   arrange(-duration)


# this is where we delete all data with nulls.. we have identified that all the missing values and nulls are in the following columns by running:
# colSums(is.na(tripRawData_duration)) -- start_station_name,start_station_id,end_station_name,end_station_id,end_lat,5858

summary(tripDataCombi)
colSums(is.na(tripDataCombi))

# removed the na values and nulls.
tripDataCombi_cleaned<- na.omit(tripDataCombi)

n_distinct(tripDataCombi_cleaned)

#all unique id's? alls good.
n_distinct(tripDataCombi_cleaned)

# 2 ways to get ride of the dates beyond our study set;
# 1- filter for the days of interest.. from x to y ... easiest way..
# 2- find where the first 2/4 characters of the date are outside the set. i want to do it both ways..

# 1st way... only show dates from to our range of interest. this is the first real data point "2022-01-01 00:00:05"
summary(tripDataCombi_cleaned) # indicates that it starts on "0001-11-20 00:25:21.00" ..make a filter


tripDataCombi_cleaned %>% 
  select(started_at,Start_date,Start_time, hour_of_day,Duration) %>% 
  subset(Start_date < "2022-01-01") %>% 
  arrange(Start_date)

filter(tripDataCombi_cleaned, Start_date < "2022-01-01") # this also works..

# done.... now second way::

# if loop doesn't work on vectors.. must use for!!

# worked...
for (i in 1:NROW(tripDataCombi_cleaned)){
  if(substr(tripDataCombi_cleaned[i,"Start_date"],1,1) < "2"){
    print(tripDataCombi_cleaned[i,"Start_date"])
  }  
}
# this worked. this is a for loop...
tripDataCombi_cleaned$Start_date[sapply(substr(tripDataCombi_cleaned$Start_date,1,1), function(x) x<2)]

# this also works: WOW! i was close!
tripDataCombi_cleaned[substr(tripDataCombi_cleaned$Start_date,1,1) != "2",]


#this is to break the time into groups: 1. 12am-6am , 2. 6am-12pm, 3. 12pm-6pm, 4. 6pm-12am

phase <- cut(tdc$hour_of_day,breaks = c(0,6,12,18,24),include.lowest =TRUE,labels = c("Night","Morning","Afternoon","Evening"))

tripDataCombi$phase <- cut(tripDataCombi$hour_of_day,breaks = c(0,6,12,18,24),include.lowest =TRUE,labels = c("Night","Morning","Afternoon","Evening"))


# tripDataCombi_duration_cleaned_days %>%
#   mutate(Catagories = cut(hour(as.Date(Start_time, breaks = seq(0, 24, 6), include.lowest = TRUE, labels = c("00:00 - 05:59", "06:00 - 11:59", "12:00 - 17:59", "18:00 - 23:59"))))) %>%
#   ungroup()
# didn't work.. the hour() function isn't working with the start_time... not compatible..
# pick the hour from the start_time colum and then put it into the correct


# tripDataCombi_duration_cleaned_days_cata <- tripDataCombi_duration_cleaned_days_cata[,!names(tripDataCombi_duration_cleaned_days_cata) %in% c("Catagories") ]
# 
# tripDataCombi_duration_cleaned_days_cata %>%
#   mutate(Catagories = if_else(Hour_of_the_day>=00 | Hour_of_the_day<=6 ~ "Night",))
# 
# typeof(tripDataCombi_duration_cleaned_days_cata$Hour_of_the_day)
# convert to number:
# tripDataCombi_duration_cleaned_days_cata$Hour_of_the_day <-as.numeric(tripDataCombi_duration_cleaned_days_cata$Hour_of_the_day)

# try again because i changed the column data type to numeric. again refuses to work!
# tripDataCombi_duration_cleaned_days_cata <-
#   tripDataCombi_duration_cleaned_days_cata %>%
#   mutate(Catagories = case_when(Hour_of_the_day>=0 | Hour_of_the_day<6 ~ "Night",
#                                 Hour_of_the_day>=6 | Hour_of_the_day<12 ~ "Morning",
#                                 Hour_of_the_day>=12 | Hour_of_the_day<18 ~ "Afternoon",
#                                 Hour_of_the_day>=18 | Hour_of_the_day<24 ~ "Evening")) 
#   n_distinct(tripDataCombi_duration_cleaned_days_cata$Catagories)

# perfect.. now to explore and query this.

tripDataCombi$month<-format(tripDataCombi$Start_date,"%m")


filter(tdcp,Start_date > "11" ) # i have just discovered that week 11 info was all delected becsuse the time format is different...  ah well..FIXED.. took 2 full days! 2 £@£#s

###########################################################################
###################### beyond this is only analysis########################
###########################################################################

table(tdcc$member_casual) # just to look at the amount of each member type
table(tdcc$rideable_type) # just to look at the amount of each member type
table(tdcc$Phase)
tabyl(tdcc$Day)


# means by the different usership.
aggregate(tdcc$Duration ~ tdcc$member_casual, FUN = mean)
aggregate(tdcc$Duration, list(tdcc$Day), FUN = mean)
aggregate(tdcc$Duration, list(tdcc$Phase,tdcc$Month), FUN = mean)
aggregate(tdcc$Duration ~ tdcc$member_casual + tdcc$Day, FUN = mean)



###########################################################################
###########################################################################
# can this be plot
aggregate(tdcc$Duration, list(tdcc$Month,tdcc$Day, tdcc$Hour), FUN= mean)

# put the weeks in order.
tdcc$Day <- ordered(tdcc$Day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# average duration for the total dataframe.
average <- mean(tdcc$Duration)
# some aggregation and checking
hourSpread <- tabyl(tdcc, Hour)
randomsCheck$percent <- randomsCheck$percent*100
sum(randomsCheck$percent)
tabyl(tdcc, Month)
str(tdcp)
summary(tdcp)
colnames(tdcp)
tail(tdcp)
head(tdcp)

summary(tdcc)
mean(tdcc$Duration)
median(tdcc$Duration)
max(tdcc$Duration)
n_distinct(tdcc$member_casual)

# save the cleaned file as a csv
# write.csv(tdcc, "/Users/nabeelel-habbash/Desktop/Data Analytics/course 8 Capstone/directed case studies/case 1/completed/tdcc.csv" , row.names = FALSE)


# Descriptive analysis of users by day of the week.(average, median, max, min )
tdcc %>%
  group_by(Day,member_casual) %>%
  summarise(Mean = mean(Duration), Median=median(Duration),Longest = max(Duration),Shortest = min(Duration))

# the type of bike users prefer
  tdcc%>% 
  group_by(rideable_type) %>% 
  # summarise(nabeel=n())%>% 
  # mutate(percent = nabeel*100/sum(nabeel)) %>% 
  ggplot(data = tdcc,mapping= aes(x= rideable_type, fill=rideable_type)) +geom_bar() + labs(title="Bike Types used")


  
  
# HERE

# average used by day of the week per phase of day
tdcc %>%
  group_by(Day,member_casual,Phase) %>%
  summarise(Mean = mean(Duration), longest = max(Duration), shortest = min(Duration), median=median(Duration))

ggplot(data= tdcc, mapping =aes(x = Day, fill = member_casual)) +geom_bar(position = "dodge")

# vs




# average used by day of the week per hour
tdcc %>%
  group_by(Day,member_casual,Hour) %>%
  summarise(Mean = mean(Duration), longest = max(Duration), shortest = min(Duration), mean= mean(Duration), median=median(Duration)) %>% 
  ggplot(data = tdcc, mapping = aes(x=Day,y=mean(Duration), fill=member_casual)) +geom_col(position = "dodge")



# very nice.. the type of bike used per rider.
User_type<-

ggplot(data = as.data.frame(tdcc%>%group_by(member_casual,rideable_type) %>% 
                              summarise(n=n())),
       mapping= aes(x= member_casual, y=n, fill =rideable_type)) +
  geom_bar(stat = 'identity') + labs(title="Type of bike used by users",
                                     y ="Duration in seconds",x="Month", caption = "By: Nabeel el Habbash")

# This is for the number of riders per day in a facet_wrap

  ggplot(data = tdcc) + geom_bar(mapping = aes(x = Day )) +facet_wrap(~tdcc$member_casual)+ 
     labs(title = "Usertype by Day ", subtitle = ("use of bikes by day by the different users"), 
          caption = "By: Nabeel el Habbash")

  # This is for the number of riders per day side by side
  ggplot(data= as.data.frame(tdcc %>% group_by(member_casual, Day) %>%  arrange(member_casual, Day) %>% 
                               summarise(number_of_rides = n())),  
         mapping= aes(x = Day, y = number_of_rides , fill = member_casual)) +
  geom_col(position = "dodge")+ labs(title = "Rides per day by each user ", subtitle = ("The number of rides by the different users"),
                                     y ="Number of rides",x="Day", caption = "By: Nabeel el Habbash") 



Duration_Per_Month<-
  tdcc %>% 
  group_by(member_casual, Month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(Duration)) %>% 
  arrange(member_casual, Month)  %>% 
  ggplot(aes(x = Month, y = average_duration , fill = member_casual)) +
  geom_col(position = "dodge")+ labs(title = "Analytic Nabeeling", subtitle = ("The average ride length(Duration) by the different Users per month"),
                                     y ="Duration in seconds",x="Month", caption = "By: Nabeel el Habbash") +scale_x_continuous(breaks=seq(1,12,1))

Duration_Per_day<-
  tdcc %>% 
  group_by(member_casual, Day) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(Duration)) %>% 
  arrange(member_casual, Day)  %>% 
  ggplot(aes(x = Day, y = average_duration , fill = member_casual)) +
  geom_col(position = "dodge")+ labs(title = "Analytic Nabeeling", subtitle = ("The average ride length(Duration) by the different Users per day"),
                                     y ="Duration in seconds",x="Day", caption = "By: Nabeel el Habbash") 


Rides_Per_Month <-
  tdcc %>% 
  group_by(member_casual, Month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, Month)  %>% 
  ggplot(aes(x = Month, y = number_of_rides , fill = member_casual)) +
  geom_col(position = "dodge")+ labs(title = "Rides per month by user group", subtitle = ("The number of rides by the different Users per month"),
                                     y ="Number of rides",x="Month", caption = "By: Nabeel el Habbash") +scale_x_continuous(breaks=seq(1,12,1))

tabyl(tdcc,Month,Day,member_casual) # much nicer than the count.. this gives a table. with the variables as listed.
count(tdcc, member_casual)
count(tdcc, Month,member_casual)

aggregate(tdcc$Duration ~ tdcc$member_casual, FUN = mean)


###########################################################################
# 1 plots i want: then im finished: 
# 1- plot the 4 phases on a chart, month on the x-axis. i want 2 facets... 
# one for members and one for casuals.
# 2- all days comparing member vs casuals.
###########################################################################


# i want to group all the users per membership and per month
  tdcc%>% 
  group_by(Month,Day,Phase,member_casual) %>% 
  summarise(no_riders=n())%>% 
  
  ggplot(data=tdcc ,aes(Month,Phase, fill= Phase )) + geom_line()+
  geom_point() + labs(title="members per month by usership") +  scale_x_continuous(breaks=seq(1,12,1))

# ggsave("user_per_month_catagorised_ggsave.png")
###########################################################################
###########################################################################



# GOT IT!!!
tdcc_final_G<- count(tdcc, Phase, Month, member_casual) # looking promising!

ggplot(data=tdcc_final_G, mapping = aes(x= Month, y= n, fill=Phase, color= Phase))+
  scale_x_continuous(breaks=seq(1,12,1))+
  geom_point()+geom_line(linewidth=1)+ facet_grid(~member_casual) + 
  labs(title = "Phase by users per month" , caption="By: Nabeel el Habbash")




###########################################################################
###########################################################################

# this is the breakdown of rides by the day.
ggplot(data = tdcc) + geom_bar(mapping = aes(x = Phase)) +
    facet_wrap(~tdcc$Day)+ labs(title = "nabeels doing analytics", subtitle = ("use of bikes by time of day by all"), 
                                caption = "By: Nabeel el Habbash")


# this is the phase used by both user types.
ggplot(data = tdcc) + geom_bar(mapping = aes(x = Phase)) +
    facet_wrap(~tdcc$Day)+facet_wrap(~tdcc$member_casual)+ labs(title = "nabeels doing analytics", 
                                                                subtitle = ("use of bikes by time of day by all"), 
                                                                caption = "By: Nabeel el Habbash")


