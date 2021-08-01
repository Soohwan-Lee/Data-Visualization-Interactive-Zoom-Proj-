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
k <- c("experience", "participation")
pe_summary <- data.frame(matrix(ncol = 6))
colnames(pe_summary) <- c("meeting", "kind", "meanVal", "sdVal", "seVal", "ciVal")
for (i in 1:length(m)) {
  for (j in 1:length(k)) {
    meanVal <- (mean(subset(previousExperience, previousExperience$meeting == m[i] & previousExperience$kind == k[j])$value))
    sdVal <- (sd(subset(previousExperience, previousExperience$meeting == m[i] & previousExperience$kind == k[j])$value))
    seVal <- sdVal/sqrt(38)
    ciVal <- 1.96*seVal
    pe_summary <- rbind(pe_summary, c(m[i], k[j], round(meanVal,2), round(sdVal,2), round(seVal,2), round(ciVal,2)))
  } 
} 
pe_summary <- pe_summary[-1 , ]
row.names(pe_summary) = NULL

# Change the Mean & Std to Numeric Value
pe_summary$meanVal = as.numeric(pe_summary$meanVal)
pe_summary$sdVal = as.numeric(pe_summary$sdVal)
pe_summary$seVal = as.numeric(pe_summary$seVal)
pe_summary$ciVal = as.numeric(pe_summary$ciVal)

# Default bar plot with 95% confidence level error bar
p <- ggplot(pe_summary, aes(x=kind, y=meanVal, fill=meeting)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=meanVal-ciVal, ymax=meanVal+ciVal), width=.2,position=position_dodge(.9))
print(p)
# Finished bar plot
p+labs(title="Previous Experience for On/Offline Meeting", x="Previous Expreience", y = "Score", fill = "Meeting")+theme_classic()

# paried t-test for experience
t.test(subset(previousExperience, meeting == "offline" & kind == "experience")$value 
       - subset(previousExperience, meeting =="online" & kind == "experience")$value)
# paired t-test for participation
t.test(subset(previousExperience, meeting == "offline" & kind == "participation")$value 
       - subset(previousExperience, meeting =="online" & kind == "participation")$value)



### Online Experience - Participation Change ###
# Elicit dataframe form main data
onlineParticipationChange = subset(data, select = c(onlineMeetingParticipationChange, onlineClassParticipationChange))
# Data Visualization





### Online Experience - Overal Satisfaction
# Elicit dataframe form main data
onlineSatisfaction = subset(data, select = c(onlineSatisfaction,onlineVerbalSatisfaction,onlineNonverbalSatisfaction))
# Data Visualization

