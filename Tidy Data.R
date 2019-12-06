#Install once
install.packages("stringr")
install.packages("dplyr")
install.packages("car")
install.packages("olsrr") #This is for backwards elimination

library(stringr)
library(dplyr)
library(car)
library(olsrr)

###Tidying up the data and fit variables
#The units for goal and pledged are in US dollars and properly converted based on the static_usd_rate column
#Filtered out any goal amounts where the goal is 0
kickstarter <- read.csv("./kickstarter_data_with_features.csv", header = TRUE) %>%
  filter(goal, goal > 0) %>%
  mutate(goal = goal*static_usd_rate) %>% 
  mutate(pledged = pledged*static_usd_rate)

#Whittle down the columns to the variables we're interested in
#weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
#weekend <- c("Saturday", "Sunday")
reduced_model <- kickstarter %>%
  mutate(launch_to_deadline_days = as.integer(str_extract(kickstarter$launch_to_deadline, "([0-9]+)"))) %>%
  mutate(create_to_launch_days = as.integer(str_extract(kickstarter$create_to_launch, "([0-9]+)"))) %>%
  select(goal, pledged, state, backers_count, created_at_weekday, staff_pick, category, country, launch_to_deadline_days, create_to_launch_days)
attach(reduced_model)



#Create a linear regression model based on this reduced model
default_model <- lm(pledged~., data=reduced_model)

###Basic Analysis of Data, Check for Linearity
summary(default_model)
#R^2 = 0.5981
#Adjusted R^2 = 0.597
#P value overall is low but the a majority of the predictors are just... terrible
plot(default_model)
#Residuals vs Fitted: Linearity and Equal Spread are violated
#Normal QQ: Normality is violated, data needs to be transformed
#Standardized Residuals vs. Fitted: Linearity and Equal Spread are violated
#It's a mess





###Transform Data and remove outliers ???







###Perform backwards, forwards and stepwise on the reduced model in order to achieve a more parsimonious model

#Perform backwards elimination
ols_step_backward_p(default_model, details=TRUE)

#Results from backwards elimination
#It said our model would fit better if we removed create_to_launch_days and country

#Perform forwards elimination
ols_step_forward_p(default_model, details=TRUE)

#Results from forwards elimination
#It said our model would fit better if we fit backers_count, staff_pick, category, state, launch_to_deadline_days,
#goal and created_at_weekday. I.e. remove create_to_launch_days, country and launch_to_deadline_days

#Perform (dub)stepwise elimination
ols_step_both_p(default_model, details=TRUE)

#Results from forwards elimination
#It said our model would fit best if we fit backers_count, staff_pick, category,
#state, launch_to_deadline_days, and goal. I.e. remove created_at_weekday, country,
#and create_to_launch_days




###Backwards Reduction Anaysis
#This fits the whole model minus these two predictors
backwards_model <- lm(pledged~. -create_to_launch_days -country, data=reduced_model)





###Jordan's analysis based on what he found doing things by hand
#We need to find the outliers.
avglog_goal = log(goal + 1) - mean(log(goal + 1))
avglog_backer = log(backers_count + 1) - mean(log(backers_count + 1))
fit <- lm(log(pledged + 1)~avglog_goal + I(avglog_goal^2) + I(avglog_goal^3))
vif(fit) # OK
fit2 <- lm(log(pledged + 1)~avglog_goal + I(avglog_goal^2) + I(avglog_goal^3)+avglog_backer+I(avglog_backer^2)+staff_pick+launch_to_deadline_days+create_to_launch_days)
vif(fit2) # Also OK
summary(fit)


