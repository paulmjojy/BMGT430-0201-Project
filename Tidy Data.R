#Install once
#install.packages("stringr")
#install.packages("dplyr")
#install.packages("car")

library(stringr)
library(dplyr)
library(car)

###Tidying up the data

kickstarter <- read.csv("./kickstarter_data_with_features.csv", header = TRUE, stringsAsFactors = FALSE)

#Conversion of Units
#The units for goal and pledged are in US dollars and properly converted based on the static_usd_rate column
<<<<<<< HEAD
reduced_dataset <- kickstarter %>%
  select(goal, pledged, state, static_usd_rate, backers_count, created_at_weekday, launched_at_weekday, staff_pick, category, country, launch_to_deadline, create_to_launch, launched_at_month, launched_at_day, launched_at_yr, deadline_month, deadline_day, deadline_yr) %>%
  mutate(goal = goal*static_usd_rate) %>%
  mutate(pledged = pledged*static_usd_rate) %>%
  select(goal, pledged, state, static_usd_rate, backers_count, staff_pick, launch_to_deadline, create_to_launch,launched_at_weekday, launched_at_month, launched_at_day, launched_at_yr, deadline_month, deadline_day, deadline_yr)

write.csv(reduced_dataset, "./reduced_dataset.csv", row.names = FALSE) #Create this and store on your local file system but don't commit this to github please.

attach(reduced_dataset)
#Paul and Anthony Section
#This is to calculate the duration, in days, of the campaign
deadline_date <- as.Date(with(reduced_dataset, paste(deadline_month, deadline_day, deadline_yr, sep="/")), format="%m/%d/%Y")
launch_date <- as.Date(with(reduced_dataset, paste(launched_at_month, launched_at_day, launched_at_yr, sep="/")), format="%m/%d/%Y")
reduced_dataset<-reduced_dataset[,-c(7:12)]
reduced_dataset$duration<-deadline_date-launch_date

#this is to turn launched_at_weekday into a binary variable. 1=weekday, 0 = weekend
weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekend <- c("Saturday", "Sunday")
reduced_dataset$launched_at_weekday[reduced_dataset$launched_at_weekday %in% weekday] <- 1
reduced_dataset$launched_at_weekday[reduced_dataset$launched_at_weekday %in% weekend] <- 0

#Convert campaign success into a binary variable, 1=success, 0 = fail
logistic_data<-reduced_dataset[state != "live",]
detach(reduced_dataset)
attach(logistic_data)
fail<- c("canceled","failed", "suspended")
success<-c("successful")
logistic_data$state[logistic_data$state %in% fail] <- 0
logistic_data$state[logistic_data$state %in% success] <- 1
logistic_data$state <- factor(state)
logistic_data$launched_at_weekday <- factor(launched_at_weekday)
=======
kickstarter <- kickstarter %>%
  mutate(goal = goal*static_usd_rate) %>%
  mutate(pledged = pledged*static_usd_rate)

##Paul Section
#This is to calculate the duration, in days, of the campaign
logistic_model <- kickstarter %>%  
  select(goal, pledged, state, static_usd_rate, backers_count, 
         launched_at_weekday, launched_at_month, launched_at_day, 
         launched_at_yr, deadline_month, deadline_day, deadline_yr)
attach(logistic_model)
logistic_model$deadline_date <- as.Date(with(logistic_model, paste(deadline_month, deadline_day, deadline_yr, sep="/")), format="%m/%d/%Y")
logistic_model$launch_date <- as.Date(with(logistic_model, paste(launched_at_month, launched_at_day, launched_at_yr, sep="/")), format="%m/%d/%Y")
logistic_model<-logistic_model[,-c(7:12)]

#Just an fyi, if you don't make another attach statement here and
#you were to just call deadline_date-launch_date, R will throw an error
#So that's why I had to call logistic_model$deadline_date-logistic_model$launch_date
logistic_model$duration<-logistic_model$deadline_date-logistic_model$launch_date

#this is to turn launched_at_weekday into a binary variable. 1=weekday, 0 = weekend
weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekend <- c("Saturday", "Sunday")
logistic_model$launched_at_weekday[logistic_model$launched_at_weekday %in% weekday] <- 1
logistic_model$launched_at_weekday[logistic_model$launched_at_weekday %in% weekend] <- 0

#Convert campaign success into a binary variable, 1=success, 0 = fail
logistic_model<-logistic_model[state != "live",]
fail<- c("canceled","failed", "suspended")
success<-c("successful")
logistic_model$state[logistic_model$state %in% fail] <- 0
logistic_model$state[logistic_model$state %in% success] <- 1
logistic_model$state <- factor(logistic_model$state)
logistic_model$launched_at_weekday <- factor(logistic_model$launched_at_weekday)

m<-glm(state~goal+launched_at_weekday+duration, data=logistic_model, family = binomial())
summary(m)



##Anthonys Section
detach(logistic_model)
logistic_model_two <- kickstarter %>%  
  select(goal, pledged, state, static_usd_rate, 
         backers_count, staff_pick, launch_to_deadline, 
         create_to_launch, launched_at_weekday, launched_at_month, 
         launched_at_day, launched_at_yr, deadline_month, 
         deadline_day, deadline_yr)
#Remember that if you alter the number of rows or columns you'll need to reattach
#Also if you're doubtful just do logistic_model_two$[Whatever this variable is]
attach(logistic_model_two)

>>>>>>> 448845c6fc9599e52d1fff844b91cad1d05e9bdd

m<-glm(state~goal+launched_at_weekday+duration, data=logistic_data, family = binomial())
summary(m)

##Jordans Section
<<<<<<< HEAD
attach(reduced_dataset)
detach(logistic_data)
=======
jordan_dataset <- kickstarter %>%
  mutate(goal = goal*static_usd_rate) %>%
  mutate(pledged = pledged*static_usd_rate) %>%
  mutate(launch_to_deadline_days = as.integer(str_extract(kickstarter$launch_to_deadline, "([0-9]+)"))) %>%
  mutate(create_to_launch_days = as.integer(str_extract(kickstarter$create_to_launch, "([0-9]+)"))) %>%
  select(goal, pledged, state, backers_count, created_at_weekday, staff_pick, category, country, launch_to_deadline_days, create_to_launch_days)

attach(jordan_dataset)

>>>>>>> 448845c6fc9599e52d1fff844b91cad1d05e9bdd
avglog_goal = log(goal + 1) - mean(log(goal + 1))
avglog_backer = log(backers_count + 1) - mean(log(backers_count + 1))
fit <- lm(log(pledged + 1)~avglog_goal + I(avglog_goal^2) + I(avglog_goal^3))
vif(fit) # OK
fit2 <- lm(log(pledged + 1)~avglog_goal + I(avglog_goal^2) + I(avglog_goal^3)+avglog_backer+I(avglog_backer^2)+staff_pick+launch_to_deadline+create_to_launch)
vif(fit2) # Also OK
summary(fit)
summary(fit2)