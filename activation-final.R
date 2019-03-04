#We use repeated measure on each participant (denoted by ID)
#We are interested in knowing if the number of attempts (DV) differs across different conditions
#For the gaze condition, may also explore the effect of angle and orientation

#Set working directory
# setwd("D:\\Google Drive\\UMSI\\Research\\Smart Speaker Evaluation (Alexa Study)\\Functionality Evaluation\\Analysis")
setwd("C:\\Users\\hslzy\\Documents\\Alexa")

#Clean up the global environment
rm(list=ls())

#Import the data
#Raw data are constructed in long format
data <- read.csv(file="activation_data.csv", header=TRUE, sep=",",stringsAsFactors = FALSE)

#Factor variables (condition group, distance, and orientation)
#Condition group
data$conditionf_mute <- factor(data$Condition, levels = c("Mute","Voice","Gaze","EchoDot"))
data$conditionf_echo <- factor(data$Condition, levels = c("EchoDot","Voice","Gaze","Mute"))
data$conditionf_voice <- factor(data$Condition, levels = c("Voice","Gaze","Mute","EchoDot"))
#Distance
data$distancef <- factor(data$Distance, levels = c(1:3),labels = c("1.5m","3m","4.5m"))
#Angle (for gaze)
data$gaze_angle <- factor(data$Angle,levels = c("0","-20","20",NA))
#Body orientation (for gaze)
data$gaze_orientation <- factor(data$Orientation, levels = c("Forward","Left","Right",NA))

library(pastecs)
options(scipen=999)
stat.desc(data$Attempts)
table(data$Attempts)

library(car)
leveneTest(Attempts ~ conditionf_mute, data=data)

#Filter the dataset to do comparison between conditions
#Only keep data points with orientation == forward and angle  == 0 for gaze condition
library(dplyr) 
data_cleaned <- data %>% 
  filter((Condition.Group != "Gaze") | 
          (Condition.Group == "Gaze") & (Angle == 0) & (Orientation == 'Forward') )
data_cleaned

#Run linear mixed models, since Friedman's test have less power to detect the differences in the groups
library("lme4")
library("lmerTest")
#The following codes test a single model for all distances. 
#It assumes that the number of times required is a linear function of the distance
model_mute <- lmer(Attempts ~ 1 + distancef + conditionf_mute + (1 | ï..ID), data = data_cleaned)
summary(model_mute)

model_echo <- lmer(Attempts ~ 1 + distancef + conditionf_echo + (1 | ï..ID), data = data_cleaned)
summary(model_echo)

model_voice <- lmer(Attempts ~ 1 + distancef + conditionf_voice + (1 | ï..ID), data = data_cleaned)
summary(model_voice)

#Syntax for calculating the effect size for the model
#For each predictor, the effect size would be the estimate coefficient
# r2.corr.mer <- function(m) {
#   lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
#   summary(lmfit)$r.squared
# }
# 
# r2.corr.mer(model_mute)

#Detect power for the comparison between mute, gaze, and voice
library(simr)
#Power to detect significant differences between mute and voice
power_mute_voice <- powerSim(model_mute, test=fixed("conditionf_muteVoice",method="z"))
print(power_mute_voice)
#Power to detect significant differences between mute and gaze
power_mute_gaze <- powerSim(model_mute, test=fixed("conditionf_muteGaze",method="z"))
print(power_mute_gaze)
#Power to detect significant differences between voice and gaze
power_voice_gaze <- powerSim(model_voice, test=fixed("conditionf_voiceGaze",method="z"))
print(power_voice_gaze)

#Move on to the examination on how angle and orientation impact performance in the gaze condition
#Filter out the dataset to only include data points in gaze
data_gaze <- data %>% 
  filter(Condition.Group == "Gaze")
data_gaze

model_gaze <- lmer(Attempts ~ 1 + distancef + gaze_angle + gaze_orientation + (1 | ï..ID), 
                   data = data_gaze)
summary(model_gaze)

#Detect power for the comparison between different angles and orientations within gaze
power_angle_negative <- powerSim(model_gaze, test=fixed("gaze_angle-20",method="z"))
print(power_angle_negative)
power_angle_positive <- powerSim(model_gaze, test=fixed("gaze_angle20",method="z"))
print(power_angle_positive)
power_orientation_right <- powerSim(model_gaze, test=fixed("gaze_orientationRight",method="z"))
print(power_orientation_right)
power_orientation_left <- powerSim(model_gaze, test=fixed("gaze_orientationLeft",method="z"))
print(power_orientation_left)
