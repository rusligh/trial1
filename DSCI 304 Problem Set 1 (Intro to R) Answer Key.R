
#############################
# DSCI 304                  #
# Problem Set 1 Answer Key  #
# Prof. Steven Perry        #
#############################


#Clear Workspace
dev.off()
cat("\014")
rm(list=ls())
set.seed(18552)


#Load in Data
setwd("U:/Rice TA/DSCI 304/Assignments")
MLBdata<-read.csv("MLB Batting Data.csv")



#### Problem #1 #####

#1) There is a longstanding saying in baseball that having a career batting average (batting_avg) of 0.300 (that is, earning a hit on at least 30% of your at bats) will earn you a place in the Baseball Hall of Fame. If that saying is correct, how many of the players in this dataset can expect to make the hall of fame? 

#We can solve this using the length command with an [] subset: 
length(MLBdata$batting_avg[MLBdata$batting_avg>=.3])

#We can also solve this using a ifelse command to create a new variable
MLBdata$FutureFamer<-ifelse(MLBdata$batting_avg>=.3, 1, 0)
table(MLBdata$FutureFamer)

#Advanced Solution: We can also use the dplyr package to solve the problem
library(tidyverse)
MLBdata %>%
  count(batting_avg>=.3)

#Answer: If the saying is true, 106 players can expect to make the Hall of Fame.

#Optional Extension: What if we did not assume that each player-year is independent? How many players would make the Hall of Fame? 
MLBdata2<- MLBdata%>%
  group_by(last_name, first_name)%>%
  summarise(all.average=mean(batting_avg), futureHOF=ifelse(all.average>=.3, 1, 0))
table(MLBdata2$futureHOF)
  #Using all of a players stats from 2015-2019, only 28 players can expect to make the Hall of Fame. 



#### Problem #2 #####

#2)	When up to bat, players are sometimes hit by a pitch. Using the hit by pitch variable (b_hit_by_pitch), create a new variable that is a 0 if the player was never hit by a pitch in a given season, and a 1 if the player was hit at least once. Report a table of your new variable. How many players were not hit at least once a season?

summary(MLBdata$b_hit_by_pitch)

#Option 1: Using NA and selective replacement
MLBdata$neverhit<-NA
MLBdata$neverhit[MLBdata$b_hit_by_pitch==0]<-0
MLBdata$neverhit[MLBdata$b_hit_by_pitch>0]<-1
table(MLBdata$neverhit)

#Option 2: Using ifelse statement
MLBdata$neverhit2<-ifelse(MLBdata$b_hit_by_pitch==0, 1, 0)
table(MLBdata$neverhit2)

#Answer:29 players were never hit by a pitch in a given season. 



#### Problem #3 #####

#3)	It is commonly thought that the harder you hit a baseball, the more likely you are to score a homerun. Assess the validity of that claim: Run a linear regression examining the effect of a players average exit velocity (exit_velocity_avg) on the number of homeruns they hit (b_home_run). What do you find? Is the conventional wisdom correct? 

hr.velocity.mod<-lm(MLBdata$b_home_run~MLBdata$exit_velocity_avg)
summary(hr.velocity.mod)

#Answer:Conventional wisdom seems to be correct, as a player's average ball exit velocity is positive and statistically correlated with the number of homeruns a player hits in a given season. Specially, as a player's average exit velocity increases by 1mph, the number of homeruns they are estimated to hit in a season increases by 2.7.  



#### Problem #4 #####

#4)	Age can be an important limitation for a hitter; it is often thought that older hitters perform at lower levels than their younger teammates. Assess that claim: Run a linear regression examining the effect of a player's age (player_age) on their batting average (batting_avg). Discuss what you find. Does age seem to play an important factor? 

age.mod<-lm(MLBdata$batting_avg~MLBdata$player_age)
summary(age.mod)

#Answer:It does not appear than a player's age has a substantive effect on their batting performance. While the effect of age is negative, the coefficient estimate is quite small and does not reach conventional levels of statistical significance. Moreover, our R-squared statistic is quite small, indicating that our model explains relatively little of the variance in player's batting performance. As a result, we cannot conclude that age has an effect on batting performance. 


#### Problem #4A #####

#a.	It may be the case that age really only matters after important milestones. Using the player_age variable, create a new variable that is a 0 if the player is less than 30 years old, and a 1 if the player is at least 30 years of age. Repreform your previous analysis using your new variable instead of the player_age variable. What do you find? Does this variable change the results or your interpretation of the importance of age? 

MLBdata$overthirity-NA
MLBdata$overthirity[MLBdata$player_age<30]<-0
MLBdata$overthirity[MLBdata$player_age>=30]<-1
table(MLBdata$overthirity)

age.mod2<-lm(MLBdata$batting_avg~MLBdata$overthirity)
summary(age.mod2)

#Answer: This analysis shows that age has slightly more of an impact on batting performance. On average, batters that are 30 years of age or older have batting averages approximately 0.004 points lower than their younger colleagues. While this effect does reach conventional levels of statistical significance, the overall effect size is small, and explains little of the overall variance in batter performance. As a result, we can infer that while age has a small affect on batter performance, it is not one of the primary factors in determining a batter's batting average. 


#### Problem #5 #####

#5)	As an alternative to the traditional batting average stats, Major League Baseball has a separate stat known as the Slugging Percentage, which is a weighted average that gives more weight to extra base hits (hits where the batter reaches more than first base). Unfortunately, this statistic is not included in our data set. Using the formula below, and the variables for single base hits (b_single), double base hits (b_double), triple base hits (b_triple), homeruns (b_home_run), and at bats (b_ab), calculate each players Slugging Percentage. What is the average Sluggging Percentage in the dataset? How many players have slugging percentages above .500? 


#First, calculate slugging percentage
slug<- (MLBdata$b_single+(2*MLBdata$b_double)+(3*MLBdata$b_triple)+(4*MLBdata$b_home_run))/MLBdata$b_ab

#Average slugging percentage
summary(slug)

#Number of players with slugging percentages above .500
length(slug[slug>.500])

#Answer: On average, player slugging percentage is approximately 0.4568. In total, 183 player-year dyads had slugging percentages above .500. 


#### Problem #6 #####

#6)	Finally, subset the full dataset into a new data frame that only includes observations from the 2019 season. 
  #a.	How many players (ie rows) from 2019 are included in the data? 
  #b.	What is the average number of plate appearances (b_total_pa) taken per player in 2019? 
  #c.	What was the most number of homeruns (b_home_run) hit by a player in 2019? What player hit those homeruns? 
  

#First, subset the data to only include observations from the 2019 season:
MLB2019<-subset(MLBdata, year==2019)

#Number of Players in 2019: 
length(MLB2019$last_name)

#Average number of plate appearances
mean(MLB2019$b_total_pa)

#Who had the most home runs? 
summary(MLB2019$b_home_run)
MLB2019$last_name[MLB2019$b_home_run==max(MLB2019$b_home_run)]
MLB2019$first_name[MLB2019$b_home_run==max(MLB2019$b_home_run)]

#Answer: We have data on 156 players from the 2019 season. On average, these players took 588.4 at-bats per player. Of those players, Pete Alonso hit the most homeruns with 53. 


