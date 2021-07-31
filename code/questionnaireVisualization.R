#install.package("")
install.packages("ggplot2")
install.packages("pylr")
install.packages("dplyr")
library(ggplot2)
library(plyr)
library(dplyr)

### Set File Path for Window Environment
setwd('C:/Users/LeeSooHwan/Desktop/DataVisualization-ZoomProj')
### Set File Path for Mac Environment
setwd("/Users/Soohwan/Desktop/DataVisualization-ZoomProj")


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
    c(mean = mean(x[[col]], na.rm=TRUE), sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}


### Previous Experience for On/Offline Meeting
# Data Preprocessing
previousExperience <- read.csv(file = "./rawData/questionnaire/previousExperience.csv", header=T, fileEncoding="UTF-8-BOM")
head(previousExperience)
m <- c("offline", "online")
k <- c("experience", "aggressivity")
pe_summary <- data.frame(matrix(ncol = 4))
colnames(pe_summary) <- c("meeting", "kind", "meanVal", "sdVal")
for (i in 1:length(m)) {
  for (j in 1:length(k)) {
    meanVal <- (mean(subset(previousExperience, previousExperience$meeting == m[i] & previousExperience$kind == k[j])$value))
    sdVal <- (sd(subset(previousExperience, previousExperience$meeting == m[i] & previousExperience$kind == k[j])$value))
    pe_summary <- rbind(pe_summary, c(m[i], k[j], round(meanVal,2), round(sdVal,2)))
  } 
} 
pe_summary <- pe_summary[-1 , ]
row.names(pe_summary) = NULL
# Change the Mean & Std to Numeric Value
pe_summary$meanVal = as.numeric(pe_summary$meanVal)
pe_summary$sdVal = as.numeric(pe_summary$sdVal)
# Default bar plot
p <- ggplot(pe_summary, aes(x=meeting, y=meanVal, fill=kind)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=meanVal-sdVal, ymax=meanVal+sdVal), width=.2,position=position_dodge(.9))
print(p)
# Finished bar plot
p+labs(title="Tooth length per dose", x="Dose (mg)", y = "Length")+
  theme_classic() +
  scale_fill_manual(values=c('#999999','#E69F00'))
print(p)
# t-test (?)



### Online Experience - Participation Change
# Elicit dataframe form main data
onlineParticipationChange = subset(data, select = c(onlineMeetingParticipationChange, onlineClassParticipationChange))
# Data Visualization


### Online Experience - Overal Satisfaction
# Elicit dataframe form main data
onlineSatisfaction = subset(data, select = c(onlineSatisfaction,onlineVerbalSatisfaction,onlineNonverbalSatisfaction))
# Data Visualization

