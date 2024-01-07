### Case Study Analysis on Cyclistic trip data of 2019 ###

# The purpose of the analysis is to merge downloaded data into a single dataframe, clean and
# format the data in a useful way, then conduct full analysis to help answer the question how
# do annual members and casual riders use Cyclistic bikes differently, and finally share my
# findings through descriptive visualisations in order for my stakeholders to take act and 
# adjust their marketing approach respectively

### install required packages ###
library(tidyverse)
library(dplyr)
library(skimr)
library(ggplot2)

### IMPORT DATA ###

q1_2019 <- read.csv("Divvy_Trips_2019_Q1.csv")
q2_2019 <- read.csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read.csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read.csv("Divvy_Trips_2019_Q4.csv")

### CREATE ONE SINGLE DATAFRAME TO WORK WITH ###

# CHECK COLUMNS IN ORDER TO MERGE INTO ONE SINGLE FILE
colnames(q1_2019)
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
# q2_2019 columns are inconsistent 

# Rename q2_2019 attributes to match the columns' names of the other datasets
q2_2019 <- q2_2019 %>% 
  rename(trip_id = X01...Rental.Details.Rental.ID) %>% 
  rename(start_time = X01...Rental.Details.Local.Start.Time) %>% 
  rename(end_time = X01...Rental.Details.Local.End.Time) %>% 
  rename(bikeid = X01...Rental.Details.Bike.ID) %>% 
  rename(tripduration = X01...Rental.Details.Duration.In.Seconds.Uncapped) %>% 
  rename(from_station_id = X03...Rental.Start.Station.ID) %>% 
  rename(from_station_name = X03...Rental.Start.Station.Name) %>% 
  rename(to_station_id = X02...Rental.End.Station.ID) %>% 
  rename(to_station_name = X02...Rental.End.Station.Name) %>% 
  rename(usertype = User.Type) %>% 
  rename(gender = Member.Gender) %>% 
  rename(birthyear = X05...Member.Details.Member.Birthday.Year)

# Check for other inconsistencies among the columns such as data types
str(q1_2019)
str(q2_2019)
str(q3_2019)
str(q4_2019)
# Νο inconsistencies found having q1_2019 as a point of interest

# Merge 4 quarter dataframes into a single dataframe
total_trips_2019 <- bind_rows(q1_2019, q2_2019, q3_2019, q4_2019)

# Create a copy of the original dataframe to work with
trips_test <- total_trips_2019

##### Clean And Format Data ###

# Check dataframe functions
colnames(trips_test)
nrow(trips_test)
head(trips_test)
str(trips_test)
summary(trips_test)
skim_without_charts(trips_test)
# no na values found

# Remove gender,birthyear columns as we cannot use personal identifiable information
trips_test <- trips_test %>% 
  select(-gender,-birthyear)

# replace columns start_at,end_at,trip_duration with
# started_at, ended_at, trip_length which are in a more
# useful format for calculations
trips_test$started_at <- ymd_hms(trips_test$start_time)
trips_test$ended_at <- ymd_hms(trips_test$end_time)
trips_test$trip_length <- difftime(trips_test$ended_at,trips_test$started_at, units=c("secs"))
trips_test <- trips_test %>% 
  select(-start_time,-end_time,-tripduration)

# Check the trip_length column for in-bound values
min(trips_test$trip_length)
# found rows with negative numbers so they must be excluded
trips_test <- trips_test %>% 
  filter(trips_test$trip_length > 0)

# Remove columns that I will not use in my analysis
# I want to keep just some id to play the role of the primary key of the observations
# My analysis will focus on the usertype and aspects of the columns started_at, ended_at
# and trip_length
trips_test <- trips_test %>% 
  select(-from_station_id,-from_station_name,-to_station_id,-to_station_name)

# Check the usertype column for its info
# We want only 2 values, Subscriber & Customer
# After that we calculate the count of Subscribers & Customers respectively
unique(trips_test$usertype)
num_of_observations <- trips_test %>% 
  count(usertype)
num_of_customers <- num_of_observations[1,2]
num_of_subscribers <- num_of_observations[2,2]
total_members = num_of_customers + num_of_subscribers

# Add columns that list the day, month and date of each trip in order to
# discover patterns

trips_test$date <- as.Date(trips_test$started_at)
trips_test$day <- format(as.Date(trips_test$date), "%d")
trips_test$month <- format(as.Date(trips_test$date), "%m")
trips_test$day_of_week <- format(as.Date(trips_test$date), "%A")

# wanting days of week in order by beginning from Sunday
trips_test$day_of_week <- ordered(trips_test$day_of_week, levels=c("Sunday", "Monday",
                                  "Tuesday","Wednesday","Thursday","Friday","Saturday"))

### CREATE DESCRIPTIVE ANALYSIS ###

# observations about trip_length (in seconds)
mean(trips_test$trip_length)
median(trips_test$trip_length)
min(trips_test$trip_length)
max(trips_test$trip_length)

# observations about trip_length for subscribers and customers respectively
aggregate(trips_test$trip_length ~ trips_test$usertype, FUN=mean)
aggregate(trips_test$trip_length ~ trips_test$usertype, FUN=median)
aggregate(trips_test$trip_length ~ trips_test$usertype, FUN=min)
aggregate(trips_test$trip_length ~ trips_test$usertype, FUN=max)

# average ride time by day for subscribers and customers respectively
aggregate(trips_test$trip_length ~ trips_test$usertype + trips_test$day_of_week,
          FUN=mean)

# average ride time by month for subscribers and customers respectively
aggregate(trips_test$trip_length ~ trips_test$usertype + trips_test$month,
          FUN=mean)

# summarization of score, number_of_trips and average_duration by day_of_week
# for subscribers and customers respectively
# score is a new metric that combine number_of_trips and average_duration
usertype_day_summarization <- trips_test %>% 
  group_by(usertype,day_of_week) %>% 
  summarize(number_of_trips =n(),
            average_duration = mean(trip_length),
            score = number_of_trips * average_duration) %>% 
  arrange(usertype,day_of_week)

# summarization of number_of_trips and average_duration by month
# for subscribers and customers respectively
usertype_month_summarization <- trips_test %>% 
  group_by(usertype,month) %>% 
  summarize(number_of_trips =n(),
            average_duration = mean(trip_length),
            score = number_of_trips * average_duration) %>% 
  arrange(usertype,month)

### VISUALIZATIONS ###

# viz about day_of_week and number_of_trips
# for subscribers and customers respectively
trips_test %>% 
  group_by(usertype,day_of_week) %>% 
  summarize(number_of_trips=n()) %>% 
  arrange(usertype,day_of_week) %>%
  ggplot(aes(x = day_of_week, y = number_of_trips, fill = usertype)) +
  geom_col(position="dodge")

# viz about day_of_week and average_duration
# for subscribers and customers respectively
trips_test %>% 
  group_by(usertype,day_of_week) %>% 
  summarize(average_duration = mean(trip_length)) %>% 
  arrange(usertype,day_of_week) %>%
  ggplot(aes(x = day_of_week, y = average_duration, fill = usertype)) +
  geom_col(position="dodge")

# viz about day_of_week and score
# for subscribers and customers respectively
trips_test %>% 
  group_by(usertype,day_of_week) %>% 
  summarize(score = (number_of_trips=n()) * (average_duration = mean(trip_length))) %>% 
  arrange(usertype,day_of_week) %>%
  ggplot(aes(x = day_of_week, y = score, fill = usertype)) +
  geom_col(position="dodge")

# viz about month and number_of_trips
# for subscribers and customers respectively 
trips_test %>% 
  group_by(usertype,month) %>% 
  summarize(number_of_trips=n()) %>% 
  arrange(usertype,month) %>%
  ggplot(aes(x = month, y = number_of_trips, fill = usertype)) +
  geom_col(position="dodge")

# viz about month and average_duration
# for subscribers and customers respectively
trips_test %>% 
  group_by(usertype,month) %>% 
  summarize(average_duration = mean(trip_length)) %>% 
  arrange(usertype,month) %>%
  ggplot(aes(x = month, y = average_duration, fill = usertype)) +
  geom_col(position="dodge")

# viz about month and score
# for subscribers and customers respectively
trips_test %>% 
  group_by(usertype,month) %>% 
  summarize(score = (number_of_trips=n()) * (average_duration = mean(trip_length))) %>% 
  arrange(usertype,month) %>%
  ggplot(aes(x = month, y = score, fill = usertype)) +
  geom_col(position="dodge")
