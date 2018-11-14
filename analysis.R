#Script for in-app advertising project survey analysis
#Yixin is amazing, Abraham sucks

#load necessary libraries
library(ggplot2)
library(gridExtra)
library(grid)
library(reshape2)
library(lemon)
library(gmodels)

#Clean the global environment
rm(list=ls())

#Read the csv file
setwd("C:\\Users\\yzou15\\Google Drive\\UMSI\\Research\\In-app advertising\\Data")
data <- read.csv("data_numeric.csv",header = TRUE)

#Factor the variables first
#Size of the company
#Create a new category for company size >20
data$Q23[data$Q23==5|data$Q23==6]<-4
data$size_company.f <- factor(data$Q23,levels=c(1:4),
          labels=c("1-4","4-9","10-19",">20"))
#Size of the development team
#Create a new category for development team size >20
data$Q24[data$Q24==5|data$Q24==6]<-4
data$size_team.f <- factor(data$Q24,levels=c(1:4),
          labels=c("1-4","4-9","10-19",">20"))
#Perceived importance of app user privacy
#Code "N/A" value as "Not at all important"
data$Q32_3[is.na(data$Q32_3)==TRUE]<-1
data$user_privacy.f <- factor(data$Q32_3, levels=c(1:5),
          labels=c("Not at all important","Slightly important","Moderately important",
                   "Very important","Extremely important"))

#Chi-square test between size of company and concern for app-user privacy
#Result: p<.05
#How to write it up in the paper: "There was a significant association between the type of training and whether or not cats learned to dance ??2(1, N = 200) = 25.36, p< .001.Based on the odds ratio, the odds of cats dancing were 6.65 times higher if they were trained with food than if trained with affection."
CrossTable(data$size_company.f, data$user_privacy.f, chisq = TRUE, fisher = TRUE, 
           expected = TRUE, sresid = TRUE, format = "SPSS")

#Generate a heatmap
ggplot(data = data, aes(x=size_company.f, y=user_privacy.f)) + 
  geom_tile()
?ggplot

#Chi-square test between size of development team and concern for app-user privacy
CrossTable(data$size_team.f, data$user_privacy.f, chisq = TRUE, fisher = TRUE, 
           expected = TRUE, sresid = TRUE, format = "SPSS")



