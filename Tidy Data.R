library(dplyr)

###Tidying up the data

kickstarter <- read.csv("./kickstarter_data_with_features.csv")

#filter out based on kickstarters with US currency ONLY
us_kickstarters <- kickstarter %>%
  filter(kickstarter$currency == "USD")

#Filter based on kickstarters without US currency
non_us_kickstarters <- kickstarter %>%
  filter(kickstarter$currency != "USD")

#Filter based on successful kickstarters
successful_kickstarters <- kickstarter %>%
  filter(kickstarter$state == "successful")

#Filter full dataset down to the columns for goal, pledged, state, number of backers, and launch date
#The units for goal and pledged are in US dollars and properly converted based on the static_usd_rate column
reduced_dataset <- kickstarter %>%
  mutate(goal = goal*static_usd_rate) %>%
  mutate(pledged = pledged*static_usd_rate) %>%
  select(goal, pledged, state, static_usd_rate, backers_count, launched_at_weekday)
  
#Hi Anthony

write.csv(reduced_dataset, "./reduced_dataset.csv", row.names = FALSE) #Create this and store on your local file system but don't commit this to github please.