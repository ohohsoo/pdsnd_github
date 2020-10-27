y <- seq(1, 100, 1)
x <- subset(y, y%%4 ==0)

### Imagine you want to create a variable x that holds all of the values in y that are divisible by 4.###
y <- seq(1, 100, 1)
x = NULL #빈 방 하나 만들어주고
idx = 1 #빈 방에 index 한개씩 넣어줘야 하니까 idx 만들어주고
for (num in y){
  if (num %% 4 == 0){
    x[idx] = num #여기 idx 만든거를 x 안에 넣어서 출력값을 zuweisen 해주고
    idx = idx + 1 #idx에 1씩 더해서 출력값이 하나씩 차들어가게 만드는듯...
  }
}
x

### Given a number z, can you find the factorial of z? For example z = 3, then 3! = 3*2*1 = 6 ###
z =  3
factorial.result = 1 #1이란 값을 만들어서 곱하게 해줄 것을 만드는듯..?
z.array = seq(1, z, 1) #이 array 덕분에 1부터 z까지 값을 나열할 수 있음
for (num in z.array){
  factorial.result = num*factorial.result #factorial.result를 1로 설정해놓은 덕분에 array의 값을 차례대로 곱할 수 있는듯...
}
factorial.result

### 위의 factorial 값 구하기를 function으로 만들기
get.factorial = function(z){
  z.array = seq(1, z, 1)
  factorial.result = 1
  for(i in z.array){
    factorial.result = factorial.result*i
  }
  return(factorial.result)
}
get.factorial(5)


#################### histogram and boxplot ########################################
### Pseudo Facebook EDA (exploratory data analysis) ###
pf <- read.csv2('C:/Users/Soo/Desktop/R/udacity/pseudo_facebook.tsv', sep = '\t')
View(head(pf))

# install.packages('ggthemes')
# library(ggthemes)
# library(ggplot2)

### How to use ggplot or qplot and start EDA
qplot(x = dob_day, data = pf)+
  scale_x_continuous(breaks=1:31)+
  facet_wrap(~dob_month, ncol=3)

ggplot(data=pf, aes(dob_day))+
  geom_histogram(binwidth = 1)+
  scale_x_continuous(breaks=1:31)+
  facet_wrap(~dob_month, ncol=3)

### Now, how is the distribution of birthdays for different genders? (facetting)
ggplot(data=pf, aes(x=dob_day, y=..density..)) +
  geom_histogram(binwidth = 1)+
  scale_x_continuous(breaks = 1:31) +
  facet_grid(dob_month~gender)

### Avoiding long tail data with xlim=c() or scale_x_continuous(limits=c())
### and setting binwidth (x-achse가 나타내는 값을 설정. binwidth=5로 설정하면 x값을 5개씩 묶어서 나타냄)
### and breaks (seq(x,y,z)와 함께 사용해서 x-achse의 값이 몇개 단위로 표시되도록 설정함.
### 예를 들어 breaks = seq(1,100,10)으로 설정하면 x-achse가 1에서 100까지 10단위씩 표시됨.
### and check how is the friend count distribution for men and women with facet_grid or facet_wrap
### facet_wrap(~variable_to_split_on), facet_grid(rows_variable~columns_variable)
fc <- pf$friend_count
View(fc)

ggplot(data=pf, aes(friend_count))+
  geom_histogram()+
  scale_x_continuous(limits = c(0, 1000))

ggplot(data=pf, aes(friend_count))+
  geom_histogram(binwidth = 25)+
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))+
  facet_wrap(~gender) #NA 값도 함께 나옴

qplot(x=friend_count, data=pf, xlim = c(0,1000), binwidth=25)

qplot(x=friend_count, data=pf, binwidth=25)+
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))+
  facet_wrap(~gender) #NA 값도 함께 나옴

#transponieren
qplot(data=pf, x=friend_count)+
  facet_grid(gender~.)

#NA 값을 없애려면?? subset!
ggplot(data=subset(pf, !is.na(gender)), aes(friend_count))+ #이렇게 하면 gender에서 na값을 빼고 계산하게됨.
  geom_histogram(binwidth = 25)+ #만약 data 전체에서 na값이 있는 row를 빼버리려면 data=na.omit(pf)를 사용.
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))+
  facet_wrap(~gender)

qplot(x=friend_count, data=subset(pf, !is.na(gender)), binwidth=25)+
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))+
  facet_wrap(~gender)

###이제 부수적인 제목 등으로 diagram을 꾸며보자
ggplot(data=subset(pf, !is.na(gender)), aes(friend_count))+ 
  geom_histogram(binwidth = 25,color ="black", fill = "hotpink")+ 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))+
  facet_wrap(~gender)+
  ggtitle("Histogram of number of friends per user")+
  labs(x="number of friends", y="number of user")

qplot(x=friend_count, data=subset(pf, !is.na(gender)), binwidth=25,color = I("black"), fill = I("hotpink"))+
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))+ #I()는 as.is라는 function
  facet_wrap(~gender)

#남자랑 여자 중 누가 평균적으로 친구가 더 많은지 살펴보기
table(pf$gender)
by(pf$friend_count, pf$gender, summary) #엄청난 function이다!! by(object, object를 나누는 variable, 사용할 function)

#alternative 1: by(pf$friend_count, pf$gender, mean)
#alternative 2: women.friend <- subset(pf$friend_count, pf$gender == "female")
#             men.friend <- subset(pf$friend_count, pf$gender != "female")
#             mean(women.friend)
#             mean(men.friend)


###practice
ggplot(data=pf, aes(tenure/365))+
  geom_histogram(binwidth = 0.25, fill="hotpink", color="black")+
  scale_x_continuous(limits=c(0,7),breaks = seq(1,7,1))+
  ggtitle("tenure")+
  labs(x = "number of years using facebook",y = "number of users")
#oder xlab("number of years using facebook")+ylab("number of users")

qplot(data=pf, x=tenure/365, binwidth=0.25, color=I("black"), fill=I("hotpink"), 
      xlab="number of zears using facebook", ylab="number of users")+
  scale_x_continuous(limits=c(0,7),breaks=seq(1,7,1))+
  ggtitle("tenure")

#What is the www_like count for men? Which gender has more www_likes?
by(pf$www_likes, pf$gender, sum)

#boxplot
ggplot(data=subset(pf, !is.na(pf$gender)), aes(x=gender, y=friend_count))+
  geom_boxplot()+
  coord_cartesian(ylim = c(0, 1000)) #여기 scale_y_continuous(limits = c(0, 1000))를 쓰면
  #boxplot의 값이 생략된 2949 rows의 값만큼 달라지게 됨!! 그래서 생략하지 않고 ausblenden만 하려면
  #coord_cartesian()를 써야함!!

qplot(data=subset(pf, !is.na(pf$gender)), x=gender, y=friend_count, geom="boxplot", ylim= c(0, 1000))
#여기도 ylim이 아니라 +coord_cartesian()를 쓸것.

#who made more friend request in average?
by(pf$friendships_initiated, pf$gender, mean)

qplot(geom="boxplot", data=subset(pf, !is.na(gender)), x=gender, y=friendships_initiated)+
  coord_cartesian(ylim=c(0, 150))

ggplot(data=subset(pf,!is.na(gender)), aes(x=gender, y=friendships_initiated))+
  geom_boxplot()+
  coord_cartesian(ylim = c(0, 150))


######################### scatterplot ########################################
summary(pf$age)

qplot(age, friend_count, data=pf)

ggplot(aes(x=age, y=friend_count), data=pf)+
  geom_point()+
  xlim(13, 90)
  
ggplot(aes(x=age, y=friend_count), data=pf)+
  geom_jitter(alpha=1/20, color="hotpink")+ #alpha=()를 지정하면 point의 채워진 정도를 설정할 수 있음.
  xlim(13, 90) #geom_gitter()를 사용하면 정수에 맞추지 않고 achse를 continuous 하게 나타낼 수 있음. 

ggplot(data=pf, aes(x=age, y=friend_count))+
  geom_point(alpha=1/20, 
             color="hotpink",
             position=position_jitter(h=0))+
  coord_trans(y="sqrt")+
  geom_line(stat = "summary", fun = mean)+ #mean을 보여주는 sekundaerachse를 추가시킴.
  geom_line(stat="summary", fun = quantile, 
            fun.args = list(probs = .1), linetype=2, color="blue")+
  geom_line(stat="summary", fun = quantile, 
            fun.args = list(probs = .5), linetype=2, color="blue")+
  geom_line(stat="summary", fun = quantile, 
            fun.args = list(probs = .9), linetype=2, color="blue")+
  #첫번째 quantile를 보여주는 sekundaerachse를 추가시킴. linetype=2는 점선. probs=0.1은 quantile 설정.
  coord_cartesian(xlim=c(13, 90), ylim = c(0, 3000))
#coord_cartesian을 쓸려면 위의 coord_trans()를 지워야함.


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

##################################SAMPLE ANSWER BELOW################################
  library(ggplot2)
  # new york data
  ny$Start.Time <- strptime(ny$Start.Time, format="%Y-%m-%d %H:%M:%S")
  ny$weekday <- weekdays(ny$Start.Time)
  ny$hour <- ny$Start.Time$hour
  ny$month <- months(ny$Start.Time)
  
  # chicago data
  chi$Start.Time <- strptime(chi$Start.Time, format="%Y-%m-%d %H:%M:%S")
  chi$weekday <- weekdays(chi$Start.Time)
  chi$hour <- chi$Start.Time$hour
  chi$month <- months(chi$Start.Time)
  
  # washington data
  wash$Start.Time <- strptime(wash$Start.Time, format="%Y-%m-%d %H:%M:%S")
  wash$weekday <- weekdays(wash$Start.Time)
  wash$hour <- wash$Start.Time$hour
  wash$month <- months(wash$Start.Time)
  
  # Custom function to handle mode calculation.
  Mode <- function(x, na.rm = FALSE) {
    if(na.rm){
      x = x[!is.na(x)]
    }
    ux <- unique(x)
    return(ux[which.max(tabulate(match(x, ux)))])
  }
  
  
  ## most common month, weekday and hour in NYC
  
  print(Mode(ny$month))
  print(Mode(ny$weekday))
  print(Mode(ny$hour))
  
  
  ##  Plots for most common month, weekday and hour in NYC
  qplot(x=ny$weekday, data = ny)
  + labs(title = "Most common day of week",x = "Days", y = "Count")

  qplot(x=ny$month, data = ny)
  + labs(title = "Most common month", x = "Month", y = "Count")
  
  qplot(x=ny$hour, data = ny)+ xlim(13,18) 
  + labs(title = "Most common hour of day", x = "Hour", y = "Count")

##########################SAMPLE ANSWER ABOVE####################################
  
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