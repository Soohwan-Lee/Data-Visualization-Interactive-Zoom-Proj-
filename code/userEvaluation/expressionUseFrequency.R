library(ggplot2)
library(plyr)
library(dplyr)

# --------------- #
# RED: "#F8766D"  #
# BLUE: "#01BFC4" #
# --------------- #


### Set File Path for Window Environment
setwd('C:/Users/LeeSooHwan/Desktop/github/DataVisualization-ZoomProj')
### Set File Path for Mac Environment
setwd("/Users/Soohwan/Desktop/DataVisualization-ZoomProj")



### Read Raw Data
rawData <- read.csv(file = "./data/userEvaluation/revisedData/expressionUseFrequencyRaw.csv", header=T, fileEncoding="UTF-8-BOM")
summaryData <- read.csv(file = "./data/userEvaluation/revisedData/expressionUseFrequency.csv", header=T, fileEncoding="UTF-8-BOM")


### Start Y axis from n
require(scales)
my_trans <- function(from=0) 
{
  trans <- function(x) x-from
  inv <- function(x) x+from
  trans_new("myscale", trans, inv, 
            domain = c(from, Inf))
}


### Draw bar plot
summaryData$expression <- factor(summaryData$expression, level = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine"))
summaryData$kind <- factor(summaryData$kind, level = c("hit", "falseAlarm", "miss"))
summaryData$value <- as.numeric(summaryData$value)

p <- ggplot(summaryData, aes(x=expression, y=value, fill=kind, label = value)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(name = "Signal Detection", labels = c("Hit", "False Alarm", "Miss"), values = c("hit" = "#00ba38","falseAlarm" = "#619cff", 'miss' = '#f8766d')) +
  geom_text(size = 4, position=position_dodge(0.9), vjust=-0.25) +
  labs(title="Frequency of use for each emoji", x="", y = "Number") + theme(legend.position="left", plot.title = element_text(hjust = 0.5), text=element_text(size=15))
p


### Kruskal-wallis test
rawData$kind <- as.factor(rawData$kind)
rawData$expression <- factor(rawData$expression, levels = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine"))
rawData$value <- as.numeric(rawData$value)

hit <- subset(rawData, rawData$kind == 'hit')
hit$expression <- factor(hit$expression, levels = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine"))
hit$value <- as.numeric(hit$value)

falseAlarm <- subset(rawData, rawData$kind == 'falseAlarm')
falseAlarm$expression <- factor(falseAlarm$expression, levels = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine"))
falseAlarm$value <- as.numeric(falseAlarm$value)

miss <- subset(rawData, rawData$kind == 'miss')
miss$expression <- factor(miss$expression, levels = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine"))
miss$value <- as.numeric(miss$value)

# kruskal test
kruskal.test(value ~ expression, data = hit)
kruskal.test(value ~ expression, data = falseAlarm)
kruskal.test(value ~ expression, data = miss)

# pairwise.wilcox test
pairwise.wilcox.test(hit$value, hit$expression, p.adj="bonferroni")
pairwise.wilcox.test(falseAlarm$value, falseAlarm$expression, p.adj="bonferroni")
pairwise.wilcox.test(miss$value, miss$expression, p.adj="bonferroni")

# wilcox.test with exact
install.packages("exactRankTests")
library(exactRankTests)
exactRankTests::wilcox.exact(subset(hit$value, hit$expression == "one"), subset(hit$value, hit$expression == "two"), p.adj="bonferroni")

# Tukey
avHit = aov(hit$value ~ hit$expression)
TukeyHSD(avHit)
avFalseAlarm = aov(falseAlarm$value ~ falseAlarm$expression)
TukeyHSD(avFalseAlarm)
avMiss = aov(miss$value ~ miss$expression)
TukeyHSD(avMiss)
