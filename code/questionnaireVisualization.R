#install.package("")
install.packages("ggplot2")
install.packages("pylr")
install.packages("dplyr")
library(ggplot2)
library(plyr)
library(dplyr)

### Set File Path for Window
setwd('C:/Users/LeeSooHwan/Desktop/DataVisualization-ZoomProj')

### Read Raw Data
data <- read.csv(file = "./rawData/questionnaire/rawData.csv", header=T, fileEncoding="UTF-8-BOM")


### Age of Participants
meanAge = mean(data$age)
variationAge = var(data$age)
stdAge = sd(data$age)


### Normality Test
# Previous Experience
shapiro.test(data$offlineExperience)
shapiro.test(data$offlineAggressivity)
shapiro.test(data$onlineExperience)
shapiro.test(data$onlineAggressivity)
# Online Experience - Participation Change
shapiro.test(data$onlineMeetingParticipationChange)
shapiro.test(data$onlineClassParticipationChange)
# Online Experience - Overal Satisfaction
shapiro.test(data$onlineSatisfaction)
shapiro.test(data$onlineVerbalSatisfaction)
shapiro.test(data$onlineNonverbalSatisfaction)


### Data Preprocessing Function (summary data)
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}


### Previous Experience for On/Offline Meeting
# Data Preprocessing
previousExperience <- read.csv(file = "./rawData/questionnaire/previousExperience.csv", header=T, fileEncoding="UTF-8-BOM")
previousExperience$kind <- as.factor(previousExperience$kind)
pe_summary <- data_summary(ToothGrowth, varname="len", groupnames=c("supp", "dose"))
# Convert dose to a factor variable
df2$dose=as.factor(df2$dose)
head(df2)

# Data Visualization

# t-test (?)


### Online Experience - Participation Change
# Elicit dataframe form main data
onlineParticipationChange = subset(data, select = c(onlineMeetingParticipationChange, onlineClassParticipationChange))
# Data Visualization


### Online Experience - Overal Satisfaction
# Elicit dataframe form main data
onlineSatisfaction = subset(data, select = c(onlineSatisfaction,onlineVerbalSatisfaction,onlineNonverbalSatisfaction))
# Data Visualization

