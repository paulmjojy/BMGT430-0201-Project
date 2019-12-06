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
kickstarter <- kickstarter %>%
  mutate(goal = goal*static_usd_rate) %>%
  mutate(pledged = pledged*static_usd_rate)

#Paul and Anthony Section

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
anova(m, test = "Chisq")
#Test if the model is significant
mred<-glm(state~1, data=logistic_model, family = binomial())
anova(mred,m,test = "Chisq")
#P-value was small so model is significant
#Transformation to fix equal spread and linearity assumptions
m1<-glm(state~I(log(goal))+launched_at_weekday+duration,data=logistic_model,family = binomial())
summary(m1)

##Jordans Section
detach(logistic_model)
jordan_dataset <- kickstarter %>%
  mutate(goal = goal*static_usd_rate) %>%
  mutate(pledged = pledged*static_usd_rate) %>%
  mutate(launch_to_deadline_days = as.integer(str_extract(kickstarter$launch_to_deadline, "([0-9]+)"))) %>%
  mutate(create_to_launch_days = as.integer(str_extract(kickstarter$create_to_launch, "([0-9]+)"))) %>%
  select(goal, pledged, state, backers_count, created_at_weekday, staff_pick, category, country, launch_to_deadline_days, create_to_launch_days)

attach(jordan_dataset)

avglog_goal = log(goal + 1) - mean(log(goal + 1))
avglog_backer = log(backers_count + 1) - mean(log(backers_count + 1))
fit <- lm(log(pledged + 1)~avglog_goal + I(avglog_goal^2) + I(avglog_goal^3))
vif(fit) # OK
fit2 <- lm(log(pledged + 1)~avglog_goal + I(avglog_goal^2) + I(avglog_goal^3)+avglog_backer+I(avglog_backer^2)+staff_pick+launch_to_deadline_days+create_to_launch_days)
vif(fit2) # Also OK
summary(fit)
