library(ggplot2)
library(plyr)
library(dplyr)

### Set File Path for Window Environment
setwd('C:/Users/LeeSooHwan/Desktop/github/DataVisualization-ZoomProj')
### Set File Path for Mac Environment
setwd("/Users/Soohwan/Desktop/DataVisualization-ZoomProj")


### Read Raw Data
userBackground <- read.csv(file = "./data/userEvaluation/revisedData/userBackground.csv", header=T, fileEncoding="UTF-8-BOM")


### Set factor for user background
userBackground$measure <- as.factor(userBackground$measure)


### Set variables for experimental group
offlineExperienceExperimental = subset(userBackground$value, userBackground$measure == "offlineExperience" & userBackground$group == "experimental" )
offlineActivenessExperimental = subset(userBackground$value, userBackground$measure == "offlineActiveness" & userBackground$group == "experimental")
onelineExperienceExperimental = subset(userBackground$value, userBackground$measure == "onlineExperience" & userBackground$group == "experimental")
onlineActivenessExperimental = subset(userBackground$value, userBackground$measure == "onlineActiveness" & userBackground$group == "experimental")
importanceNVOffExperimental = subset(userBackground$value, userBackground$measure == "importanceNVOff" & userBackground$group == "experimental")
FrequencyNVOffExperimental = subset(userBackground$value, userBackground$measure == "FrequencyNVOff" & userBackground$group == "experimental")
usefulnessNVoffExperimental = subset(userBackground$value, userBackground$measure == "usefulnessNVoff" & userBackground$group == "experimental")
importanceNVOnExperimental = subset(userBackground$value, userBackground$measure == "importanceNVOn" & userBackground$group == "experimental")
frequencyNVOnExperimental = subset(userBackground$value, userBackground$measure == "frequencyNVOn" & userBackground$group == "experimental")
usefulnessNVOnExperimental = subset(userBackground$value, userBackground$measure == "usefulnessNVOn" & userBackground$group == "experimental")
importanceEOnExperimental = subset(userBackground$value, userBackground$measure == "importanceEOn" & userBackground$group == "experimental")
frequencyEOnExperimental = subset(userBackground$value, userBackground$measure == "frequencyEOn" & userBackground$group == "experimental")
usefulnessEOnExperimental = subset(userBackground$value, userBackground$measure == "usefulnessEOn" & userBackground$group == "experimental")


### Set variables for control group
offlineExperienceControl = subset(userBackground$value, userBackground$measure == "offlineExperience" & userBackground$group == "control" )
offlineActivenessControl = subset(userBackground$value, userBackground$measure == "offlineActiveness" & userBackground$group == "control")
onelineExperienceControl = subset(userBackground$value, userBackground$measure == "onlineExperience" & userBackground$group == "control")
onlineActivenessControl = subset(userBackground$value, userBackground$measure == "onlineActiveness" & userBackground$group == "control")
importanceNVOffControl = subset(userBackground$value, userBackground$measure == "importanceNVOff" & userBackground$group == "control")
FrequencyNVOffControl = subset(userBackground$value, userBackground$measure == "FrequencyNVOff" & userBackground$group == "control")
usefulnessNVoffControl = subset(userBackground$value, userBackground$measure == "usefulnessNVoff" & userBackground$group == "control")
importanceNVOnControl = subset(userBackground$value, userBackground$measure == "importanceNVOn" & userBackground$group == "control")
frequencyNVOnControl = subset(userBackground$value, userBackground$measure == "frequencyNVOn" & userBackground$group == "control")
usefulnessNVOnControl = subset(userBackground$value, userBackground$measure == "usefulnessNVOn" & userBackground$group == "control")
importanceEOnControl = subset(userBackground$value, userBackground$measure == "importanceEOn" & userBackground$group == "control")
frequencyEOnControl = subset(userBackground$value, userBackground$measure == "frequencyEOn" & userBackground$group == "control")
usefulnessEOnControl = subset(userBackground$value, userBackground$measure == "usefulnessEOn" & userBackground$group == "control")


### Normality test for experimental group
shapiro.test(offlineExperienceExperimental)
shapiro.test(offlineActivenessExperimental)
shapiro.test(onelineExperienceExperimental)
shapiro.test(onlineActivenessExperimental)
shapiro.test(importanceNVOffExperimental)
shapiro.test(FrequencyNVOffExperimental)
shapiro.test(usefulnessNVoffExperimental)
shapiro.test(importanceNVOnExperimental)
shapiro.test(frequencyNVOnExperimental)
shapiro.test(usefulnessNVOnExperimental)
shapiro.test(importanceEOnExperimental)
shapiro.test(frequencyEOnExperimental)
shapiro.test(usefulnessEOnExperimental)


### Normality test for control group
shapiro.test(offlineExperienceControl)
shapiro.test(offlineActivenessControl)
shapiro.test(onelineExperienceControl)
shapiro.test(onlineActivenessControl)
shapiro.test(importanceNVOffControl)
shapiro.test(FrequencyNVOffControl)
shapiro.test(usefulnessNVoffControl)
shapiro.test(importanceNVOnControl)
shapiro.test(frequencyNVOnControl)
shapiro.test(usefulnessNVOnControl)
shapiro.test(importanceEOnControl)
shapiro.test(frequencyEOnControl)
shapiro.test(usefulnessEOnControl)



### Wilcoxon rank sum test between experimental & control group
wilcox.test(offlineExperienceExperimental, offlineExperienceControl)
wilcox.test(offlineActivenessExperimental, offlineActivenessControl)
wilcox.test(onelineExperienceExperimental, onelineExperienceControl)
wilcox.test(onlineActivenessExperimental, onlineActivenessControl)
wilcox.test(importanceNVOffExperimental, importanceNVOffControl)
wilcox.test(FrequencyNVOffExperimental, FrequencyNVOffControl)
wilcox.test(usefulnessNVoffExperimental, usefulnessNVoffControl)
wilcox.test(importanceNVOnExperimental, importanceNVOnControl)
wilcox.test(frequencyNVOnExperimental, frequencyNVOnControl)
wilcox.test(usefulnessNVOnExperimental, usefulnessNVOnControl)
wilcox.test(importanceEOnExperimental, importanceEOnControl)
wilcox.test(frequencyEOnExperimental, frequencyEOnControl)
wilcox.test(usefulnessEOnExperimental, usefulnessEOnControl)

