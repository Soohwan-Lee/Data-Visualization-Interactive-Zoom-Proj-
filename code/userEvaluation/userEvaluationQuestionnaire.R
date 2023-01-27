#install.package("")
install.packages("ggplot2")
install.packages("pylr")
install.packages("dplyr")
install.packages("remotes")                    # Install remotes package
remotes::install_github("coolbutuseless/ggpattern")
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


### Set factor for questionnaireControl & questionnaireExperimental
questionnaireControl$measure <- as.factor(questionnaireControl$measure)
questionnaireExperimental$measure <- as.factor(questionnaireExperimental$measure)
questionnaireMerge$measure <- as.factor(questionnaireMerge$measure)
experimentalMeetingExperience$measure <- as.factor(experimentalMeetingExperience$measure)
experimentalSystemEvaluation$measure <- as.factor(experimentalSystemEvaluation$measure)
questionnaireControl$order <- as.factor(questionnaireControl$order)
questionnaireExperimental$order <- as.factor(questionnaireExperimental$order)
questionnaireMerge$order <- as.factor(questionnaireMerge$order)
experimentalMeetingExperience$order <- as.factor(experimentalMeetingExperience$order)
experimentalSystemEvaluation$order <- as.factor(experimentalSystemEvaluation$order)



### Set variables for questionnaireControl
controlSO = subset(questionnaireControl$value, questionnaireControl$measure == 'SO')
controlSP1 = subset(questionnaireControl$value, questionnaireControl$measure == 'SP1')
controlSP2 = subset(questionnaireControl$value, questionnaireControl$measure == 'SP2')
controlPNGA = subset(questionnaireControl$value, questionnaireControl$measure == 'PNGA')
controlprogress1 = subset(questionnaireControl$value, questionnaireControl$measure == 'progress1')
controlprogress2 = subset(questionnaireControl$value, questionnaireControl$measure == 'progress2')
controlparticipation = subset(questionnaireControl$value, questionnaireControl$measure == 'participation')
controlfeedback1 = subset(questionnaireControl$value, questionnaireControl$measure == 'feedback1')
controlfeedback2 = subset(questionnaireControl$value, questionnaireControl$measure == 'feedback2')
controlsocialPresence = subset(questionnaireControl$value, questionnaireControl$measure == 'socialPresence')


### Set variables for questionnaireControl_First Group
controlSOFirst = subset(questionnaireControl$value, questionnaireControl$measure == 'SO' & questionnaireControl$order == 'first')
controlSP1First = subset(questionnaireControl$value, questionnaireControl$measure == 'SP1' & questionnaireControl$order == 'first')
controlSP2First = subset(questionnaireControl$value, questionnaireControl$measure == 'SP2' & questionnaireControl$order == 'first')
controlPNGAFirst = subset(questionnaireControl$value, questionnaireControl$measure == 'PNGA' & questionnaireControl$order == 'first')
controlprogress1First = subset(questionnaireControl$value, questionnaireControl$measure == 'progress1' & questionnaireControl$order == 'first')
controlprogress2First = subset(questionnaireControl$value, questionnaireControl$measure == 'progress2' & questionnaireControl$order == 'first')
controlparticipationFirst = subset(questionnaireControl$value, questionnaireControl$measure == 'participation' & questionnaireControl$order == 'first')
controlfeedback1First = subset(questionnaireControl$value, questionnaireControl$measure == 'feedback1' & questionnaireControl$order == 'first')
controlfeedback2First = subset(questionnaireControl$value, questionnaireControl$measure == 'feedback2' & questionnaireControl$order == 'first')
controlsocialPresenceFirst = subset(questionnaireControl$value, questionnaireControl$measure == 'socialPresence' & questionnaireControl$order == 'first')


### Set variables for questionnaireControl_Second Group
controlSOSecond = subset(questionnaireControl$value, questionnaireControl$measure == 'SO' & questionnaireControl$order == 'second')
controlSP1Second = subset(questionnaireControl$value, questionnaireControl$measure == 'SP1' & questionnaireControl$order == 'second')
controlSP2Second = subset(questionnaireControl$value, questionnaireControl$measure == 'SP2' & questionnaireControl$order == 'second')
controlPNGASecond = subset(questionnaireControl$value, questionnaireControl$measure == 'PNGA' & questionnaireControl$order == 'second')
controlprogress1Second = subset(questionnaireControl$value, questionnaireControl$measure == 'progress1' & questionnaireControl$order == 'second')
controlprogress2Second = subset(questionnaireControl$value, questionnaireControl$measure == 'progress2' & questionnaireControl$order == 'second')
controlparticipationSecond = subset(questionnaireControl$value, questionnaireControl$measure == 'participation' & questionnaireControl$order == 'second')
controlfeedback1Second = subset(questionnaireControl$value, questionnaireControl$measure == 'feedback1' & questionnaireControl$order == 'second')
controlfeedback2Second = subset(questionnaireControl$value, questionnaireControl$measure == 'feedback2' & questionnaireControl$order == 'second')
controlsocialPresenceSecond = subset(questionnaireControl$value, questionnaireControl$measure == 'socialPresence' & questionnaireControl$order == 'second')


### Set variables for questionnaireExperimental
experimentalSO = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'SO')
experimentalSP1 = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'SP1')
experimentalSP2 = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'SP2')
experimentalPNGA = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'PNGA')
experimentalprogress1 = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'progress1')
experimentalprogress2 = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'progress2')
experimentalparticipation = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'participation')
experimentalfeedback1 = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'feedback1')
experimentalfeedback2 = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'feedback2')
experimentalsocialPresence = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'socialPresence')
experimentalsystemNovelty = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'systemNovelty')
experimentalbodyNovelty = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'bodyNovelty')
experimentalfaceNovelty = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'faceNovelty')
experimentalimmediacy = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'immediacy')
experimentalaccuracy = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'accuracy')
experimentallearnability = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'learnability')
experimentalfrustration = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'frustration')


### Set variables for questionnaireExperimental_First Group
experimentalSOFirst = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'SO' & questionnaireExperimental$order == 'first')
experimentalSP1First = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'SP1' & questionnaireExperimental$order == 'first')
experimentalSP2First = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'SP2' & questionnaireExperimental$order == 'first')
experimentalPNGAFirst = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'PNGA' & questionnaireExperimental$order == 'first')
experimentalprogress1First = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'progress1' & questionnaireExperimental$order == 'first')
experimentalprogress2First = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'progress2' & questionnaireExperimental$order == 'first')
experimentalparticipationFirst = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'participation' & questionnaireExperimental$order == 'first')
experimentalfeedback1First = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'feedback1' & questionnaireExperimental$order == 'first')
experimentalfeedback2First = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'feedback2' & questionnaireExperimental$order == 'first')
experimentalsocialPresenceFirst = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'socialPresence' & questionnaireExperimental$order == 'first')
experimentalsystemNoveltyFirst = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'systemNovelty' & questionnaireExperimental$order == 'first')
experimentalbodyNoveltyFirst = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'bodyNovelty' & questionnaireExperimental$order == 'first')
experimentalfaceNoveltyFirst = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'faceNovelty' & questionnaireExperimental$order == 'first')
experimentalimmediacyFirst = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'immediacy' & questionnaireExperimental$order == 'first')
experimentalaccuracyFirst = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'accuracy' & questionnaireExperimental$order == 'first')
experimentallearnabilityFirst = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'learnability' & questionnaireExperimental$order == 'first')
experimentalfrustrationFirst = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'frustration' & questionnaireExperimental$order == 'first')



### Set variables for questionnaireExperimental_Second Group
experimentalSOSecond = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'SO' & questionnaireExperimental$order == 'second')
experimentalSP1Second = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'SP1' & questionnaireExperimental$order == 'second')
experimentalSP2Second = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'SP2' & questionnaireExperimental$order == 'second')
experimentalPNGASecond = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'PNGA' & questionnaireExperimental$order == 'second')
experimentalprogress1Second = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'progress1' & questionnaireExperimental$order == 'second')
experimentalprogress2Second = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'progress2' & questionnaireExperimental$order == 'second')
experimentalparticipationSecond = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'participation' & questionnaireExperimental$order == 'second')
experimentalfeedback1Second = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'feedback1' & questionnaireExperimental$order == 'second')
experimentalfeedback2Second = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'feedback2' & questionnaireExperimental$order == 'second')
experimentalsocialPresenceSecond = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'socialPresence' & questionnaireExperimental$order == 'second')
experimentalsystemNoveltySecond = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'systemNovelty' & questionnaireExperimental$order == 'second')
experimentalbodyNoveltySecond = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'bodyNovelty' & questionnaireExperimental$order == 'second')
experimentalfaceNoveltySecond = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'faceNovelty' & questionnaireExperimental$order == 'second')
experimentalimmediacySecond = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'immediacy' & questionnaireExperimental$order == 'second')
experimentalaccuracySecond = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'accuracy' & questionnaireExperimental$order == 'second')
experimentallearnabilitySecond = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'learnability' & questionnaireExperimental$order == 'second')
experimentalfrustrationSecond = subset(questionnaireExperimental$value, questionnaireExperimental$measure == 'frustration' & questionnaireExperimental$order == 'second')



### Normality Test for questionnaireControl
shapiro.test(controlSO)
shapiro.test(controlSP1)
shapiro.test(controlSP2)
shapiro.test(controlPNGA)
shapiro.test(controlprogress1)
shapiro.test(controlprogress2)
shapiro.test(controlparticipation)
shapiro.test(controlfeedback1)
shapiro.test(controlfeedback2)
shapiro.test(controlsocialPresence)


### Normality Test for questionnaireExperimental
shapiro.test(experimentalSO)
shapiro.test(experimentalSP1)
shapiro.test(experimentalSP2)
shapiro.test(experimentalPNGA)
shapiro.test(experimentalprogress1)
shapiro.test(experimentalprogress2)
shapiro.test(experimentalparticipation)
shapiro.test(experimentalfeedback1)
shapiro.test(experimentalfeedback2)
shapiro.test(experimentalsocialPresence)
shapiro.test(experimentalsystemNovelty)
shapiro.test(experimentalbodyNovelty)
shapiro.test(experimentalfaceNovelty)
shapiro.test(experimentalimmediacy)
shapiro.test(experimentalaccuracy)
shapiro.test(experimentallearnability)
shapiro.test(experimentalfrustration)


#### Wilcoxon rank sum test between control group & experimental group
wilcox.test(controlSO, experimentalSO)
wilcox.test(controlSP1, experimentalSP1)
wilcox.test(controlSP2, experimentalSP2)
wilcox.test(controlPNGA, experimentalPNGA)
wilcox.test(controlprogress1, experimentalprogress1)
wilcox.test(controlprogress2, experimentalprogress2)
wilcox.test(controlparticipation, experimentalparticipation)
wilcox.test(controlfeedback1, experimentalfeedback1)
wilcox.test(controlfeedback2, experimentalfeedback2)
wilcox.test(controlsocialPresence, experimentalsocialPresence)


### Wilcoxon rank sum test betwen first and second group of control group
wilcox.test(controlSOFirst, controlSOSecond)
wilcox.test(controlSP1First, controlSP1Second)
wilcox.test(controlSP1First, controlSP1Second)
wilcox.test(controlPNGAFirst, controlPNGASecond)
wilcox.test(controlprogress1First, controlprogress1Second)
wilcox.test(controlprogress2First, controlprogress2Second)
wilcox.test(controlparticipationFirst, controlparticipationSecond)
wilcox.test(controlfeedback1First, controlfeedback1Second)
wilcox.test(controlfeedback2First, controlfeedback2Second)
wilcox.test(controlsocialPresenceFirst, controlsocialPresenceSecond)


### Wilcoxon rank sum test betwen first and second group of experimental group
wilcox.test(experimentalSOFirst, experimentalSOSecond)
wilcox.test(experimentalSP1First, experimentalSP1Second)
wilcox.test(experimentalSP2First, experimentalSP2Second)
wilcox.test(experimentalPNGAFirst, experimentalPNGASecond)
wilcox.test(experimentalprogress1First, experimentalprogress1Second)
wilcox.test(experimentalprogress2First, experimentalprogress2Second)
wilcox.test(experimentalparticipationFirst, experimentalparticipationSecond)
wilcox.test(experimentalfeedback1First, experimentalfeedback1Second)
wilcox.test(experimentalfeedback2First, experimentalfeedback2Second)
wilcox.test(experimentalsocialPresenceFirst, experimentalsocialPresenceSecond)
wilcox.test(experimentalsystemNoveltyFirst, experimentalsystemNoveltySecond)
wilcox.test(experimentalfaceNoveltyFirst, experimentalfaceNoveltySecond)
wilcox.test(experimentalbodyNoveltyFirst, experimentalbodyNoveltySecond)
wilcox.test(experimentalimmediacyFirst, experimentalimmediacySecond)
wilcox.test(experimentalaccuracyFirst, experimentalaccuracySecond)
wilcox.test(experimentallearnabilityFirst, experimentallearnabilitySecond)
wilcox.test(experimentalfrustrationFirst, experimentalfrustrationSecond)


### Box plot of control group & experimental group (SO~socialPresence)
# Define each variables
questionnaireMerge$value <- as.numeric(questionnaireMerge$value)
questionnaireMerge$group <- as.factor(questionnaireMerge$group)
questionnaireMerge$measure <- factor(questionnaireMerge$measure, level=c("progress2", "progress1", "SP2", "participation", "SO", "SP1", "PNGA", "socialPresence", "feedback1", "feedback2"))

mergeFile <- ggplot(questionnaireMerge, aes(x=measure, y=value, fill=group)) + geom_boxplot(alpha=0.4, outlier.color = 'black',outlier.shape = 2) + 
  #scale_x_discrete(labels=c("SO", "SP1", "SP2", "PNGA", "P", "PE", "A", "F1", "F2", "SPS")) +
  scale_x_discrete(labels=c("Positive\nexperience","Progress speed", "Activeness\nin meeting", "Concentration\nin meeting",
                            "Satisfaction\nwith outcome", "Satsfaction\nwith process", "Perceived net goal\nattainment",
                            "Social presence", "Other people's\nfeedback", "Feedback\nto other people")) +
  scale_fill_discrete(labels=c("Control group", "Experimental group")) +
  scale_y_continuous(trans = my_trans( from=1),breaks = c(1,2,3,4,5,6,7)) + 
  coord_cartesian(ylim = c(1, 7)) +
  geom_point(aes(colour = group), position=position_jitterdodge(), show.legend = F) +
  labs(title="Quantitative user evaluation of meeting experience through questionnaire", x="", y = "Score", fill = "Group") + 
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=15),legend.position = "left")
mergeFile


 ### Box plot of experimental group (systemNovelty ~ frustration)
experimentalSystemEvaluation$measure <- factor(experimentalSystemEvaluation$measure, level=c('systemNovelty', 'bodyNovelty', 'faceNovelty', 'immediacy', 'accuracy', 'learnability', 'frustration'))
experimentalSystemEvaluation$value <- as.numeric(experimentalSystemEvaluation$value)
systemEvaluation <- ggplot(experimentalSystemEvaluation, aes(x=measure, y=value)) + geom_boxplot(fill = "#01BFC4", alpha=0.4, outlier.color = 'black',outlier.shape = 2) +
  scale_x_discrete(labels=c("Novelty of system", "Novelty of body gesture", "Novelty of facial expression", "Recognition immediacy", "Recognition accuracy", "System learnability", "Frustration")) +
  scale_y_continuous(trans = my_trans( from=1),breaks = c(1,2,3,4,5,6,7)) + 
  coord_cartesian(ylim = c(1, 7)) +
  geom_point(aes(color = 'measure'), position=position_jitterdodge(), show.legend = F) +
  scale_color_manual(values = c("#01BFC4", "#01BFC4", "#01BFC4", "#01BFC4", "#01BFC4", "#01BFC4", "#01BFC4")) +
  labs(title="Quantitative concept and system evaluation of experimental group", x="", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=15),legend.position = "left")
systemEvaluation


### Control group Box plot (First vs Second)
questionnaireControl$value <- as.numeric(questionnaireControl$value)
questionnaireControl$measure <- factor(questionnaireControl$measure, level=c("progress2", "progress1", "SP2", "participation", "SO", "SP1", "PNGA", "socialPresence", "feedback1", "feedback2"))
questionnaireControl$order <- factor(questionnaireControl$order)
questionnaireControlCompare <- ggplot(questionnaireControl, aes(x=measure, y=value, pattern = order)) + 
  scale_x_discrete(labels=c("Positive\nexperience","Progress speed", "Activeness\nin meeting", "Concentration\nin meeting",
                            "Satisfaction\nwith outcome", "Satsfaction\nwith process", "Perceived net goal\nattainment",
                            "Social presence", "Other people's\nfeedback", "Feedback\nto other people")) +
  geom_boxplot_pattern(pattern_fill = "Black", pattern_spacing = 0.03, alpha = 0, aes(pattern = order), color =  "#F8766D") +
  scale_y_continuous(trans = my_trans( from=1), breaks = c(1,2,3,4,5,6,7)) + 
  coord_cartesian(ylim = c(1, 7)) +
  geom_point(aes(colour = order), position=position_jitterdodge(), show.legend = F) +
  guides(color="none") +
  scale_color_manual(values = c("#F8766D", "#F8766D")) +
  scale_pattern_discrete(labels = c("First group", "Second group")) +
  labs(title="Questionnaire result(meeting experience) of the first and second group of control group", x="", y = "Score", pattern = "Control group") + 
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=15),legend.position = "left")
questionnaireControlCompare



### Experimental group Box plot - meeting experience (First vs Second)
experimentalMeetingExperience$value <- as.numeric(experimentalMeetingExperience$value)
experimentalMeetingExperience$measure <- factor(experimentalMeetingExperience$measure, level=c("progress2", "progress1", "SP2", "participation", "SO", "SP1", "PNGA", "socialPresence", "feedback1", "feedback2"))
experimentalMeetingExperience$order <- factor(experimentalMeetingExperience$order)
experimentalMeetingExperienceCompare <- ggplot(experimentalMeetingExperience, aes(x=measure, y=value, pattern = order)) + 
  scale_x_discrete(labels=c("Positive\nexperience","Progress speed", "Activeness\nin meeting", "Concentration\nin meeting",
                            "Satisfaction\nwith outcome", "Satsfaction\nwith process", "Perceived net goal\nattainment",
                            "Social presence", "Other people's\nfeedback", "Feedback\nto other people")) +
  geom_boxplot_pattern(pattern_fill = "Black", pattern_spacing = 0.03, alpha = 0, aes(pattern = order), color =  "#01BFC4") +
  scale_y_continuous(trans = my_trans( from=1), breaks = c(1,2,3,4,5,6,7)) + 
  coord_cartesian(ylim = c(1, 7)) +
  geom_point(aes(colour = order), position=position_jitterdodge(), show.legend = F) +
  guides(color="none") +
  scale_color_manual(values = c("#01BFC4", "#01BFC4")) +
  scale_pattern_discrete(labels = c("First group", "Second group")) +
  labs(title="Questionnaire result(meeting experience) of the first and second group of experimental group", x="", y = "Score", pattern = "Experimental group") + 
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=15),legend.position = "left")
experimentalMeetingExperienceCompare


### Experimental group Box plot - system evaluation (First vs Second)
experimentalSystemEvaluation$value <- as.numeric(experimentalSystemEvaluation$value)
experimentalSystemEvaluation$measure <- factor(experimentalSystemEvaluation$measure, level=c('systemNovelty', 'bodyNovelty', 'faceNovelty', 'immediacy', 'accuracy', 'learnability', 'frustration'))
experimentalSystemEvaluation$order <- factor(experimentalSystemEvaluation$order)
experimentalSystemEvaluationCompare <- ggplot(experimentalSystemEvaluation, aes(x=measure, y=value, pattern = order)) + 
  scale_x_discrete(labels=c("Novelty of system", "Novelty of body gesture", "Novelty of facial expression", "Recognition immediacy", "Recognition accuracy", "System learnability", "Frustration")) +
  geom_boxplot_pattern(pattern_fill = "Black", pattern_spacing = 0.03, alpha = 0, aes(pattern = order), color =  "#01BFC4") +
  scale_y_continuous(trans = my_trans( from=1), breaks = c(1,2,3,4,5,6,7)) + 
  coord_cartesian(ylim = c(1, 7)) +
  geom_point(aes(colour = order), position=position_jitterdodge(), show.legend = F) +
  guides(color="none") +
  scale_color_manual(values = c("#01BFC4", "#01BFC4")) +
  scale_pattern_discrete(labels = c("First group", "Second group")) +
  labs(title="Questionnaire result(system evaluation) of the first and second group of experimental group", x="", y = "Score", pattern = "Experimental group") + 
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=15),legend.position = "left")
experimentalSystemEvaluationCompare

