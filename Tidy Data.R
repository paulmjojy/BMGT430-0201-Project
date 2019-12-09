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
kickstarter <- read.csv("./kickstarter_data_with_features.csv", header = TRUE) 

#Whittle down the columns to the variables we're interested in
weekday <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday") #Sunday is the baseline
status <- c("failed", "canceled", "suspended", "live", "successful") #Failed is the baseline
kickstarter_filtered <- kickstarter %>%
  mutate(goal = goal*static_usd_rate) %>% 
  mutate(pledged = pledged*static_usd_rate) %>%
  mutate(launch_to_deadline_days = as.integer(str_extract(launch_to_deadline, "([0-9]+)"))) %>%
  mutate(create_to_launch_days = as.integer(str_extract(create_to_launch, "([0-9]+)"))) %>%
  mutate(state = factor(state, ordered = FALSE, labels = status, levels = status)) %>% #Made the state an ordered factor
  mutate(created_at_weekday = factor(created_at_weekday, ordered = FALSE, labels = weekday, levels = weekday)) %>%
  select(pledged, goal, backers_count, launch_to_deadline_days, create_to_launch_days, staff_pick, state, created_at_weekday, category, country) %>%
  filter(category != "") #After manipulating all the data we need to remove the rows where there is no category
summary(kickstarter_filtered)
attach(kickstarter_filtered)


###Basic Analysis of Data, Check for Linearity
#Create a linear regression model with modified predictors and response variables due to range of data
default_model <- lm(pledged~., data=kickstarter_filtered)
summary(default_model)
#R^2 = 0.5875
#Adjusted R^2 = 0.5862
#P value overall is low but the a majority of the predictors are just... terrible

plot(default_model)
plot(default_model$terms) #Plot each predictor
#Residuals vs Fitted: Linearity and Equal Spread are violated
#Normal QQ: Normality is violated, data needs to be transformed
#Standardized Residuals vs. Fitted: Linearity and Equal Spread are violated
#A few outliers based on residuals vs leverage
#It's a mess

vif(default_model)
#Nothing wrong with the VIF, all of them hover around 1


###Transform Data and remove outliers
#Transformed the Model to take the log of the response and two predictors
transformed_model<- lm(log(pledged+1)~log(goal) + log(backers_count+1) + launch_to_deadline_days + 
                       create_to_launch_days + staff_pick + state + created_at_weekday +
                       category + country, data=kickstarter_filtered)
plot(transformed_model$terms) #plot all the predictors


#Add Interaction Variables
interaction_terms_model <- lm(pledged~goal + backers_count + launch_to_deadline_days + 
                                create_to_launch_days + staff_pick + state + created_at_weekday +
                                category + country + goal*staff_pick + goal*state + 
                                backers_count*created_at_weekday + 
                                backers_count*country, data=kickstarter_filtered)
summary(interaction_terms_model)
plot(interaction_terms_model)
#None of them are significant

interaction_terms_model_with_log <- lm(log(pledged+1)~log(goal) + log(backers_count+1) + launch_to_deadline_days + 
                                create_to_launch_days + staff_pick + state + created_at_weekday +
                                category + country + log(goal)*staff_pick + log(goal)*state + 
                                log(backers_count+1)*created_at_weekday + 
                                log(backers_count+1)*country, data=kickstarter_filtered)
summary(interaction_terms_model_with_log)
plot(interaction_terms_model_with_log)


#Polynomial Fitting
#First fitted model using some higher power polynomial transformations
fit <- lm(log(pledged+1)~log(goal) + I(log(goal)^2) + I(log(goal)^3) + 
          log(backers_count + 1) + I(log(backers_count + 1)^2) + launch_to_deadline_days + 
          create_to_launch_days + staff_pick + state + created_at_weekday +
          category + country, data=kickstarter_filtered)
summary(fit)
vif(fit)
plot(fit)

#Fit a second time, centering the variables
avglog_goal = log(goal + 1) - mean(log(goal + 1))
avglog_backer = log(backers_count + 1) - mean(log(backers_count + 1))
fit2 <- lm(log(pledged + 1)~avglog_goal + I(avglog_goal^2) + I(avglog_goal^3)+
             avglog_backer+I(avglog_backer^2) + launch_to_deadline_days + 
             create_to_launch_days + staff_pick + state + created_at_weekday +
             category + country, data=kickstarter_filtered)
vif(fit2) # Also OK
summary(fit2)
plot(fit2) #Looks more normal, a few weird observations/trends

#clean up fit2
#This process just indiscriminately removes any points that have a cookes distance
#greater than 4/n. I don't know if you wanna examine it further but...
cookes_distance_fit2 <- cooks.distance(fit2)
values_to_remove_cook_fit2 <- cookes_distance_fit2 > (4/nrow(kickstarter_filtered)) #4/n is apparently a good metric to determine if we should remove or not...
to_remove_cooke_fit2 <- as.numeric(names(cookes_distance_fit2)[values_to_remove_cook_fit2])
plot(cookes_distance_fit2)
kickstarter_filtered_no_outliers <- kickstarter_filtered[-to_remove_cooke_fit2,]

#Remember to detach and reattach the new dataset because we took out some points
#so there will be a discrepancy in rows
detach(kickstarter_filtered)
attach(kickstarter_filtered_no_outliers)

#Recalculate because some data points are missing from the original model
avglog_goal = log(goal + 1) - mean(log(goal + 1))
avglog_backer = log(backers_count + 1) - mean(log(backers_count + 1))
fit3 <- lm(log(pledged + 1)~avglog_goal + I(avglog_goal^2) + I(avglog_goal^3)+
                            avglog_backer+I(avglog_backer^2)+staff_pick+launch_to_deadline_days+
                            create_to_launch_days, data=kickstarter_filtered_no_outliers)
summary(fit3)
plot(fit3)

#Perform backwards elimination on the original model
detach(kickstarter_filtered_no_outliers)
attach(kickstarter_filtered)
ols_step_backward_p(fit3, details=TRUE) #Nothing removed, we're good for this part then.



#This is just something extra
ols_step_both_p(fit3, details=TRUE) #a lot interesting here
stepwise_model <- lm(log(pledged + 1)~avglog_goal + I(avglog_goal^3) +
                       avglog_backer + I(avglog_backer^2)+staff_pick +
                       create_to_launch_days, data=kickstarter_filtered_no_outliers)
summary(stepwise_model)
plot(stepwise_model)
anova(stepwise_model, fit3)