#---------------project----------------
library("dplyr")
library("lubridate")
library(ggplot2)

washington <- read.csv2("C:/Users/Soo/Desktop/R/udacity/washington.csv", sep = ",")
newyork <- read.csv2("C:/Users/Soo/Desktop/R/udacity/new-york-city.csv", sep = ",")
chicago <- read.csv2("C:/Users/Soo/Desktop/R/udacity/chicago.csv", sep = ",")

str(chicago)

washington$Start.Time2 <- as.Date(washington$Start.Time)
newyork$Start.Time2 <- as.Date(newyork$Start.Time)
chicago$Start.Time2 <- as.Date(chicago$Start.Time)

washington$End.Time2 <- as.Date(washington$End.Time)
newyork$End.Time2 <- as.Date(newyork$End.Time)
chicago$End.Time2 <- as.Date(chicago$End.Time)

#1 Popular times of travel (i.e., occurs most often in the start time)
#   What is the most common month in Washington?
ggplot(data = washington, aes(x = floor_date(Start.Time2, "month")))+
  geom_histogram(fill = "dodgerblue1")+
  scale_x_date(date_labels = "%b %y", date_breaks = "1 month")+
  ylim(c(0, 80000))+
  stat_bin(geom = "text", aes(label = ..count..), vjust = -0.5)
  labs(title = "Popular times of travel", 
      subtitle = "What is the most common month?", 
      x = "Month",
      y = "Travel count")
# Answer: The most common month in Washington is June with 68,339 total count of use. 
#         I'd appreciate if you could tell me how to remove the zeros between the months.
#         And in general, if you have the solutions for all the listed questions in Project Details,
#         I'd really appreciate if you could provide me the sample solutions
#         because I really want to be able to handle data with dates. Thank you in advance.
  
# What is the most common month in all 3 cities?
washstart <- washington$Start.Time2
nystart <- newyork$Start.Time2
chicstart <- chicago$Start.Time2
startall <- data.frame(floor_date(washstart, "month"), 
                       floor_date(nystart, "month"), 
                       floor_date(chicstart, "month"))
names(startall) <- c("Washington", "Newyork", "Chicago")
str(startall)

washcount <- table(startall$Washington)
nycount <- table(startall$Newyork)
chiccount <- table(startall$Chicago)
countall <- data.frame(washcount, nycount, chiccount)
countall <- countall[,-3]
countall <- countall[,-4]
countall$sum <- rowSums(countall[,-1])
names(countall) <- c("Month", "Washington", "Newyork", "Chicago", "Sum")
countall <- countall[order(-countall$Sum),]
head(countall, 1)
# Answer: The most common month in all 3 cities is also June with 242,442 total count.

# 2 Popular stations and trip
#   What is the most common trip from start to end (i.e., most frequent combination of start station and end station)?
washington$Trip <- paste(washington$Start.Station, washington$End.Station, sep = " - ")
counttrip <- as.data.frame(table(washington$Trip))
colnames(counttrip) <- c("trip", "freq")
counttrip <- counttrip[order(-counttrip$freq),]
head(counttrip, 1)
# Answer: The most common trip from start to end in Washington is 
#         a round trip from Jefferson Dr $ 14th St Sw with 673 total count.

# 3 Trip duration
#   What is the average travel time for users in different cities?
str(washington$Trip.Duration)
washington$Trip.Duration <- as.numeric(washington$Trip.Duration)
ttime <- c(mean(washington$Trip.Duration)/60, 
           mean(newyork$Trip.Duration)/60, 
           mean(chicago$Trip.Duration)/60)
tname <- c("Washington", "Newyork", "Chicago")
traveltime <- cbind(tname, ttime)
traveltime <- as.data.frame(traveltime)
View(traveltime)

ggplot(data=traveltime, aes(x=tname, y=ttime))+
  geom_bar(stat="identity", fill="hotpink")+
  labs(title="Trip duration", 
       subtitle="What is the average travel time for users in different cities?",
       x="Cities",
       y="Travel duration in minutes")
# Answer: The average travel time for users in different cities is about 15min. in Chicago and New York.
#         In Washington, the average travel time is slightly higher than 20min.

# 4 User info
#   What are the counts of each user type?
usertype <- as.data.frame(washington$User.Type)
usertype$newyork <- newyork$User.Type
usertype$chicago <- chicago$User.Type
names(usertype)[1] <- "washington"

ggplot(data=usertype, aes(x=washington))+
  geom_bar(fill="skyblue")+
  labs(title="What are the counts of each user type in Washington?",
       x="Washington",
       y="Count of user")

ggplot(data=usertype, aes(x=newyork))+
  geom_bar(fill="pink")+
  labs(title="What are the counts of each user type in New York?",
       x="New York",
       y="Count of user")+
  xlim(c("Customer", "Subscriber"))

ggplot(data=usertype, aes(x=chicago))+
  geom_bar(fill="darkgreen")+
  labs(title="What are the counts of each user type in Chicago?",
       x="Chicago",
       y="Count of user")+
  xlim(c("Customer", "Subscriber"))
# question to the reviewer: How can I show all three cities in one diagram just next to each other?
#                           And how can I label the bars with the number of count?

table(usertype$washington)
table(usertype$newyork)
table(usertype$chicago)
# Answer: There are 79214 Customers in Washington, 30159 in New York and 61110 in Chicago.
#         The number of Subscribers is 220786 in Washington, 269149 in New York, 238889 in Chicago. 
#         To wrap up, the number of subscribers is much higher than the number of customers. 
#         And we have 692 empty data inputs in New York.

#   What are the counts of each gender (only available for NYC and Chicago)?
n <- as.data.frame(newyork$Gender)
names(n)[1] <- "m.w"
n$city <- rep("newyork", 300000)

c <- as.data.frame(chicago$Gender)
names(c)[1] <- "m.w"
c$city <- rep("chicago", 300000)
gender <- rbind(n, c)

ggplot(data=gender, aes(x=m.w))+
  geom_bar(fill="orange")+
  facet_wrap(~city)+
  xlim(c("Female", "Male"))+
  stat_count(geom = "text", colour = "white", size = 3.5,
           aes(label = ..count..),position=position_stack(vjust=0.5))+
  labs(title="Whar are the counts of each gener?",
       x="Gender",
       y="Count of gender")
# Answer: In Chicago, we have 57758 female user and 181190 male user.
#         In New York we have 66783 female user and 204008 male user. 
#         In both city, we have more than 3 times male user.