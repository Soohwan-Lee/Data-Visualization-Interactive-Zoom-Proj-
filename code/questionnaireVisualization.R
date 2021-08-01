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
data <- read.csv(file = "./data/questionnaire/rawData/rawData.csv", header=T, fileEncoding="UTF-8-BOM")



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



### Previous Experience for On/Offline Meeting ###
# Data Preprocessing
previousExperience <- read.csv(file = "./data/questionnaire/revisedData/previousExperience.csv", header=T, fileEncoding="UTF-8-BOM")
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
pe_summary$kind <- factor(pe_summary$kind, level = c("experience", "participation"))
p <- ggplot(pe_summary, aes(x=kind, y=meanVal, fill=meeting)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=meanVal-ciVal, ymax=meanVal+ciVal), width=.2,position=position_dodge(.9)) +
  scale_y_continuous(limits = c(0,7))
print(p)
# Finished bar plot
p+labs(title="Previous Experience for On/Offline Meeting", x="Previous Expreience", y = "Score", fill = "Meeting")

# paried t-test for experience
t.test(subset(previousExperience, meeting == "offline" & kind == "experience")$value 
       - subset(previousExperience, meeting =="online" & kind == "experience")$value)
# paired t-test for participation
t.test(subset(previousExperience, meeting == "offline" & kind == "participation")$value 
       - subset(previousExperience, meeting =="online" & kind == "participation")$value)



### Online Experience - Participation Change ###
# Elicit dataframe form main data
onlineParticipationChange <- read.csv(file = "./data/questionnaire/revisedData/onlineParticipationChange.csv", header=T, fileEncoding="UTF-8-BOM")
head(onlineParticipationChange)
purpose <- c("meeting", "class")
opc_summary <- data.frame(matrix(ncol = 5))
colnames(opc_summary) <- c("purpose", "meanVal", "sdVal", "seVal", "ciVal")
for (i in 1:length(purpose)) {
  meanVal <- (mean(subset(onlineParticipationChange, onlineParticipationChange$participation == purpose[i])$value))
  sdVal <- (sd(subset(onlineParticipationChange, onlineParticipationChange$participation == purpose[i])$value))
  seVal <- sdVal/sqrt(38)
  ciVal <- 1.96*seVal
  opc_summary <- rbind(opc_summary, c(purpose[i], round(meanVal,2), round(sdVal,2), round(seVal,2), round(ciVal,2)))
} 
opc_summary <- opc_summary[-1 , ]
row.names(opc_summary) = NULL

# Change the Mean & Std to Numeric Value
opc_summary$meanVal = as.numeric(opc_summary$meanVal)
opc_summary$sdVal = as.numeric(opc_summary$sdVal)
opc_summary$seVal = as.numeric(opc_summary$seVal)
opc_summary$ciVal = as.numeric(opc_summary$ciVal)

# Default bar plot with 95% confidence level error bar
opc_summary$purpose <- factor(opc_summary$purpose, level = c("class", "meeting"))
p <- ggplot(opc_summary, aes(x=purpose, y=meanVal)) + 
  geom_bar(stat="identity", color="BLACK", fill = "#F8766D", position=position_dodge()) +
  geom_errorbar(aes(ymin=meanVal-ciVal, ymax=meanVal+ciVal), width=.2,position=position_dodge(.9)) +
  scale_y_continuous(limits = c(0,7))
print(p)
# Finished bar plot
p+labs(title="Participation Change depend on the Meeting", x="Online Meeting Purpose", y = "Score") + theme(plot.title = element_text(hjust = 0.5))




### Online Experience - Overall Satisfaction ###
# Elicit dataframe form main data
onlineSatisfaction <- read.csv(file = "./data/questionnaire/revisedData/onlineSatisfaction.csv", header=T, fileEncoding="UTF-8-BOM")
head(onlineSatisfaction)
s <- c("overall", "verbal", "nonVerbal")
os_summary <- data.frame(matrix(ncol = 5))
colnames(os_summary) <- c("satisfaction", "meanVal", "sdVal", "seVal", "ciVal")
for (i in 1:length(s)) {
  meanVal <- (mean(subset(onlineSatisfaction, onlineSatisfaction$satisfaction == s[i])$value))
  sdVal <- (sd(subset(onlineSatisfaction, onlineSatisfaction$satisfaction == s[i])$value))
  seVal <- sdVal/sqrt(38)
  ciVal <- 1.96*seVal
  os_summary <- rbind(os_summary, c(s[i], round(meanVal,2), round(sdVal,2), round(seVal,2), round(ciVal,2)))
} 
os_summary <- os_summary[-1 , ]
row.names(os_summary) = NULL

# Change the Mean & Std to Numeric Value
os_summary$meanVal = as.numeric(os_summary$meanVal)
os_summary$sdVal = as.numeric(os_summary$sdVal)
os_summary$seVal = as.numeric(os_summary$seVal)
os_summary$ciVal = as.numeric(os_summary$ciVal)

# Default bar plot with 95% confidence level error bar
os_summary$satisfaction <- factor(os_summary$satisfaction, level = c("overall", "verbal", "nonVerbal"))
p <- ggplot(os_summary, aes(x=satisfaction, y=meanVal, fill = osPosition)) + 
  geom_bar(stat="identity", color="BLACK", position=position_dodge()) +
  geom_errorbar(aes(ymin=meanVal-ciVal, ymax=meanVal+ciVal), width=.2,position=position_dodge(.9)) +
  scale_y_continuous(limits = c(0,7)) + scale_fill_manual(values = c("overall" = "#F8766D","verbal" = "#01BFC4", 'nonVerbal' = '#01BFC4'))
print(p)
# Finished bar plot
p+labs(title="Satisfaction of Online Meeting", x="", y = "Score") + theme(legend.position="none", plot.title = element_text(hjust = 0.5))

# paired t-test
t.test(subset(onlineSatisfaction, satisfaction == "verbal")$value 
       - subset(onlineSatisfaction, satisfaction =="nonVerbal")$value)
