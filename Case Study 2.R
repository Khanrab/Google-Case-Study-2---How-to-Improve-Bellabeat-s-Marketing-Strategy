## PART I. Some basic steps, calculations, reading and understanding the data, etc.
## PART II. Dealing with my research questions and trying to find proofs for the hypotheses.
## Research question:
## --> a) Is there a relationship between the number of steps taken and the amount of
##        calories burned among FitBit users?
## --> b) Is there a difference in the number of steps taken and the amount of calories burned
##        between weekdays and weekends among FitBit users?
## --> c) How does the relationship between the number of steps taken and the amount of calories
##        burned differ between users of different weight groups?
## Hypothesis:
## --> a) There is a positive correlation between the number of steps taken and the amount
##        of calories burned among FitBit users.
## --> b) FitBit users will take more steps weekdays than on weekends, but the difference is not
##        significant.
## --> c) Users with a higher weight will burn more calories when taking the same number of
##        steps compared to users with lower weight.

###########
##PART I.##
###########

## 1. Install+read potentially necessary packages

install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("readxl")
install.packages("dplyr")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readxl)
library(dplyr)

## 2. setting up the library for simplier codes for reading the data

getwd()
setwd("Desired_Destination/Fitabase Data 4.12.16-5.12.16")

## 3. Reading of the data
dailyActivity <- read.csv("dailyActivity_merged.csv")
sleepDay <- read.csv("sleepDay_merged.csv")
weightLogInfo <- read.csv("weightLogInfo_merged.csv")

## 4. A quick look into the table and check the colnames

head(dailyActivity)
head(sleepDay)
colnames(weightLogInfo)

## 5. Checking the number of participants, observations, understanding the data
n_distinct(dailyActivity$Id)
n_distinct(sleepDay$Id)
n_distinct(weightLogInfo$Id)

#This step is rather useless as it can be well read at the top right box
#nrow(dailyActivity_merged)
#nrow(sleepDay_merged)
#nrow(weightLogInfo)

dailyActivity %>%
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes) %>%
  summary()

sleepDay %>%  
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()

## 6. Data cleansing and merging
## (with all the participants but empty cells and with
## the participants only present in both daatabases)

#this keeps all the participants
merged1 <- merge(sleepDay, dailyActivity, by="Id", all = TRUE)
#but this is more relevant for analization purposes as we have the participants present only
#in both databases
merged2 <- merge(sleepDay, dailyActivity, by="Id")
n_distinct(merged1$Id) #whichever we chose, we can double check the number of the participants
n_distinct(merged2$Id)

## 7. Plotting

# Comparing total steps and sedentary minutes
ggplot(data=dailyActivity, aes(x=TotalSteps, y=SedentaryMinutes)) + geom_point()
# There is no strict correlation between total steps and sedentary minutes, however, until
# 10000 steps there is a group of people who sits more if he/she walks less. Although the
# visualized data doesn't let us presuppose any correlation, we can understand that most of
# the people sit more and walk less. Fitbit products could help to turn this bad habbit of
# people.

# The relation between minutes asleep and time in bed
ggplot(data=sleepDay, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + geom_point()
#There is a strong correlation between total minutes asleep and total time in bed
# There is only a few unexpected trends for those between 200 and 400 and between
# around 600 and more minutes sleeping, who tends to spend more time in bed. 

# Fitbit products target people to improve people's bad sleeping habbits and spent time in bed.
# Fitbit could also use these data, extend it and use it for a more sophisticated way of
# understanding health norms and conditions to make their products more effective.

############
##PART II.##
############

## a) Q1: Is there a relationship between the number of steps taken and the amount of
##    calories burned among FitBit users?
##    H1: There is a positive correlation between the number of steps taken and the amount
##    of calories burned among FitBit users.

#calculating the correlation coefficient:
correlation <- cor(dailyActivity$TotalSteps, dailyActivity$Calories)
#creating our linear regression between the total steps taken and the ammount of calories burned
ggplot(dailyActivity, aes(x = TotalSteps, y = Calories)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Total Steps", y = "Calories Burned", 
       title = paste("Correlation:", round(correlation, 2)))

# We can see that there is a positive correlation between the number of steps and the amount of
# calories. Our first hypothesis is supported.


## b) Q2: Is there a difference in the number of steps taken and the amount of calories burned
##    between weekdays and weekends among FitBit users?
##    H2: FitBit users will take more steps weekdays than on weekends, but the difference is not
##    significant.

# new df with the necessary variables + adding a weekend-weekday variable
weekday <- dailyActivity %>% 
  select(TotalSteps, Calories, ActivityDate) %>% 
  mutate(weekday = ifelse(weekdays(as.Date(ActivityDate)) %in% c("Saturday", "Sunday"), "weekend", "weekday"))  

weekday_mean <- weekday %>% 
  group_by(weekday) %>% 
  summarize(steps_mean = mean(TotalSteps), calories_mean = mean(Calories))

ggplot(weekday_mean, aes(x = weekday, y = steps_mean, fill = weekday)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Day of the week", y = "Mean number of steps taken",
       title = "Mean number of steps taken by weekday/weekend") +
  scale_fill_manual(values = c("#FFA500", "#0072B2"))
# Now the weekdays are just slightly higher than weekends, which positively answers the research
# question. However, the result is not significant, due to the slight difference. For this
# and similar cases, where we may not evaluate the significance ourselves, we can run a
# t-test to see whether our result is significant or not.
t.test(weekday$TotalSteps ~ weekday$weekday)
# After the t-test we can see that our p-value is 84,7% whereas the maximum accepted level is 5%,
# (in some cases 10%) meaning that there is no significant difference in the mean number of steps
# taken between weekdays and weekends among FitBit users.


## c) Q1: How does the relationship between the number of steps taken and the amount of calories
##        burned differ between users of different weight groups?
##    H2: Users with a higher weight will burn more calories when taking the same number of
##        steps compared to users with lower weight.

##Approach 1

merged_weight <- merge(weightLogInfo, dailyActivity, by="Id")

#lets make it more narrow
weight_question <- merged_weight %>% 
  select(Id, WeightKg, TotalSteps, Calories) %>% 
  mutate(weight_category = cut(WeightKg, c(0, 50, 100, 150, 200), labels = c("underweight", "normal", "overweight", "obese")))

#group by weight groups (approach 1)
weight_mean <- weight_question %>% 
  group_by(weight_category) %>% 
  summarize(steps_mean = mean(TotalSteps), calories_mean = mean(Calories))

ggplot(weight_mean, aes(x = steps_mean, y = calories_mean, color = weight_category)) +
  geom_point(size = 3) +
  xlab("Mean Number of Steps") +
  ylab("Mean Number of Calories Burned") +
  ggtitle("Relationship between Steps and Calories Burned by Weight Category")
# So far this does not help us answer the research question.

# First, lets check the correlation coefficient between steps and calories
correlation <- cor(weight_question$TotalSteps, weight_question$Calories)
# and a correlation test with significant p-value
cor_test <- cor.test(weight_question$TotalSteps, weight_question$Calories)
# and finally, a plot
ggplot(weight_question, aes(x=TotalSteps, y=Calories, color=weight_category)) +
  geom_point() +
  labs(title="Relationship between Steps and Calories Burned by Weight",
       x="Steps", y="Calories Burned", color="Weight (kg)") +
  theme(plot.title = element_text(hjust = 0.5))
# our visualized data shows that the users with a higher weight will burn more
# calories when taking the same number of steps compared to users with lower weight.
# However, this seems only true until 5000 steps as there is a deviant group above 5000
# steps. As there are only 2 groups available of 4, the low sample size or wrong weight
# groups could both be problems.


## Approach 2
## We can try something else ang  group by the users and mean weight, steps and calories so that
## one user will take only one dot.

weight_question_groupped <- weight_question %>%
  group_by(Id) %>%
  summarize(steps_mean = mean(TotalSteps), calories_mean = mean(Calories), kilos_mean = mean(WeightKg))

ggplot(weight_question_groupped, aes(x = steps_mean, y = calories_mean, color = kilos_mean)) +
  geom_point(size = 3) +
  xlab("Mean Number of Steps") +
  ylab("Mean Number of Calories Burned") +
  ggtitle("Relationship between Steps and Calories Burned by Weight Category")
# Although this seems to be too narrow, as we get rid of many valuable information and trends

correlation2 <- cor(weight_question_groupped$steps_mean, weight_question_groupped$calories_mean)
# and a t-test again. But in this approach p-value is too high, therefore we cannot use this method
cor_test2 <- cor.test(weight_question_groupped$steps_mean, weight_question_groupped$calories_mean)
# Anyways, just for analytical purposes without validation, here comes the plot
ggplot(weight_question_groupped, aes(x=steps_mean, y=calories_mean, color=kilos_mean)) +
  geom_point() +
  labs(title="Relationship between Steps and Calories Burned by Weight",
       x="Steps", y="Calories Burned", color="Weight (kg)") +
  theme(plot.title = element_text(hjust = 0.5))
# none of the graphs of the second approach seems to be useful. The second graph of the first
# approach is the most accurate so far, partially proving the hypothesis.


## Approach 3
## However, since the lightest user is 53 kg and the heaviest is 134 kg in this database,
## we could approach reorganizing the weight groups, which can help us to get a better visualized
## data.

weight_attempt3 <- merged_weight %>% 
  select(Id, WeightKg, TotalSteps, Calories) %>% 
  mutate(weight_category = cut(WeightKg, c(52, 70, 84, 120, 150), labels = c("Flyweight-Lightweight", "Welterweight-Middleweight", "Light Heavyweight-Heavyweight", "Super Heavyweight")))
# First, lets check the correlation coefficient between steps and calories
correlation3 <- cor(weight_attempt3$TotalSteps, weight_attempt3$Calories)
# and a correlation test
cor_test3 <- cor.test(weight_attempt3$TotalSteps, weight_attempt3$Calories)
# and the plot
ggplot(weight_attempt3, aes(x=TotalSteps, y=Calories, color=weight_category)) +
  geom_point() +
  labs(title="Relationship between Steps and Calories Burned by Weight",
       x="Steps", y="Calories Burned", color="Weight (kg)") +
  theme(plot.title = element_text(hjust = 0.5))

# or we can just ignore the weight categories and use the available data of weight
# the trend is even more visible
ggplot(weight_attempt3, aes(x=TotalSteps, y=Calories, color=WeightKg)) +
  geom_point() +
  labs(title="Relationship between Steps and Calories Burned by Weight",
       x="Steps", y="Calories Burned", color="Weight (kg)") +
  theme(plot.title = element_text(hjust = 0.5))

# I used the MMA weight categories and narrowed it down to four elements in this attempt.
# The purple and blue dots are the heavyweight and super heavyweight FitBit users, while the
# green and pink dots are the Flyweight-Middleweight users.
# The data shows that the pink group (lightweight) is burning the least calories with a given
# amount of steps,while the green group tends to burn more calories and the purple and blue
# (heavyweight) users burn the most amount of calories.
# As an example, a lightweight user might walk around 10000 steps, and they would burn
# around 2000 calories in a day. For heavyweight users this number is 4000 calories,
# twice as much as for lightweight users.

## In conclusion there is a correlation between the number of steps taken and the amount
## of calories burned among FitBit users of different weight groups.