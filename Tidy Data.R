#Install once
install.packages("stringr")
install.packages("dplyr")
install.packages("car")

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

##Jordans Section
reduced_model <- kickstarter %>%
  mutate(goal = goal*static_usd_rate) %>%
  mutate(pledged = pledged*static_usd_rate) %>%
  mutate(launch_to_deadline_days = as.integer(str_extract(kickstarter$launch_to_deadline, "([0-9]+)"))) %>%
  mutate(create_to_launch_days = as.integer(str_extract(kickstarter$create_to_launch, "([0-9]+)"))) %>%
  select(goal, pledged, state, backers_count, created_at_weekday, staff_pick, category, country, launch_to_deadline_days, create_to_launch_days)

attach(reduced_model)

avglog_goal = log(goal + 1) - mean(log(goal + 1))
avglog_backer = log(backers_count + 1) - mean(log(backers_count + 1))
fit <- lm(log(pledged + 1)~avglog_goal + I(avglog_goal^2) + I(avglog_goal^3))
vif(fit) # OK
fit2 <- lm(log(pledged + 1)~avglog_goal + I(avglog_goal^2) + I(avglog_goal^3)+avglog_backer+I(avglog_backer^2)+staff_pick+launch_to_deadline_days+create_to_launch_days)
vif(fit2) # Also OK
summary(fit)
