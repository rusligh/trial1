install.packages("tidyverse")
library(tidyverse)

## 1
data <- read_csv("~/Desktop/DSCI/PSET1/MLB Batting Data.csv")
sum(data$batting_avg >= 0.3)
count <- length(which(data$batting_avg >=0.3))
print(count)
# 106 players in this dataset can expect to make the hall of fame

## 2
data$hit_at_least_once<-ifelse (data$b_hit_by_pitch >=1, 1, 0)
print(sum(data$hit_at_least_once ==0))
# 29 players were not hit at least once a season

## 3
plot(data$exit_velocity_avg, data$b_home_run)
exit_velocity_avg_on_home_run <- lm (data$b_home_run ~ data$exit_velocity_avg, data =data)
summary(exit_velocity_avg_on_home_run)
abline(exit_velocity_avg_on_home_run, col="blue", lwd=2)
## 
# The conventional wisdom is correct because the linear regression summary shown 
# the estimate change in the number of homerun is increased by 2.7042 as the 
# exit_velocity_avg increases by 1

## 4
plot(data$player_age,data$batting_avg)
player_age_on_batting_avg <- lm (data$batting_avg ~ data$player_age, data =data)
summary(player_age_on_batting_avg)
abline(lm(data$batting_avg ~ data$player_age), col="red")
##
# Age doesn't seem to play an important factor on batting performance
# Although the slope is negative, but it is very small (-0.0004381)
# And P value is 0.0995 which is greater than 0.05, thus not statistically significant
# Multiple R-squared value is 0.003729, which means that player_age can explain 
# only ~0.3% of variation in batting_avg

### 4a
data$overthirty <-ifelse (data$player_age >= 30, 1, 0)
table(data$overthirty)
player_overthirty_on_batting_avg <- lm (data$batting_avg ~ data$overthirty, data = data)
summary (player_overthirty_on_batting_avg)
##
# The batting average of over thirthy is lower by 0.004 as compare to below thirty
# The P values is 0.0355 which is statistically significant
# However Multiple R-squared is 0.00607 which means the impact of over thirty is only 0.6% in 
# batting_avg which is very small

## 5
slugging_percentage <- (data$b_single+(2*data$b_double)+(3*data$b_triple)+(4*data$b_home_run))/data$b_ab
summary(slugging_percentage)
#print (slugging_percentage)
sum(slugging_percentage > 0.5)
##
# From the summary, the median average of slugging percentage is 0.4513
# And 183 players have slugging percentage above .500

## 6
#data_2019 <- subset(data,year==2019, select = c(last_name, year))

##6a subset data from 2019 season only
data_2019 <- subset(data,year==2019)
print(data_2019)
total_player_2019= nrow(data_2019)
print(total_player_2019)
# Total number of player is 156

##6b b_total_pa average
avg_total_pa=(mean(data_2019$b_total_pa))
print(avg_total_pa)
#Average number of plate appearance taken per player in 2019 is  588.4359

##6c Most number of b_home_run by a player
summary(data_2019$b_home_run)
most_homeruns= max(data_2019$b_home_run)
print(most_homeruns)
# Most number of homeruns hit by a player in 2019 is 53
first_name_hr <- data_2019$first_name[data_2019$b_home_run==max(data_2019$b_home_run)]
last_name_hr <- data_2019$last_name[data_2019$b_home_run==max(data_2019$b_home_run)]
print(first_name_hr)
print(last_name_hr)
# They player that hit those homeruns is Pete Alonso
