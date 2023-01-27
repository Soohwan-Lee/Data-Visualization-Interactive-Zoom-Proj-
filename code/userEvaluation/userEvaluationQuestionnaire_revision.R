library(ggpattern)       
library(ggplot2)
library(plyr)
library(dplyr)

# --------------- #
# RED: "#F8766D"  #
# BLUE: "#01BFC4" #
# --------------- #

### Set File Path for Window Environment
setwd('C:/Users/LeeSooHwan/Desktop/github/NEASdataVisualization')
### Set File Path for Mac Environment
setwd("/Users/Soohwan/Desktop/Github/NEASdataVisualization")


### Read Raw Data
questionnaireControl <- read.csv(file = "./data/userEvaluation/revisedData/questionnaireControl.csv", header=T, fileEncoding="UTF-8-BOM")
questionnaireExperimental <- read.csv(file = "./data/userEvaluation/revisedData/questionnaireExperimental.csv", header=T, fileEncoding="UTF-8-BOM")
questionnaireMerge <- read.csv(file = "./data/userEvaluation/revisedData/questionnaireMerge.csv", header=T, fileEncoding="UTF-8-BOM")
experimentalMeetingExperience <- read.csv(file = "./data/userEvaluation/revisedData/experimentalMeetingExperience.csv", header=T, fileEncoding="UTF-8-BOM")
experimentalSystemEvaluation <- read.csv(file = "./data/userEvaluation/revisedData/experimentalSystemEvaluation.csv", header=T, fileEncoding="UTF-8-BOM")


### Start Y axis from n
require(scales)
my_trans <- function(from=0) 
{
  trans <- function(x) x-from
  inv <- function(x) x+from
  trans_new("myscale", trans, inv, 
            domain = c(from, Inf))
}