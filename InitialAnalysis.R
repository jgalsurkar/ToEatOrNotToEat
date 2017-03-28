rm(list=ls())
library(ggplot2)
library(viridis)
library("dplyr")

ViolationsData <- read.csv(file="DOHMH_New_York_City_Restaurant_Inspection_Results.csv", 
                           header=TRUE, sep=",",as.is=TRUE)
ViolationsData<-ViolationsData %>%
  mutate(INSPECTION.DATE= as.Date(INSPECTION.DATE, format= "%m/%d/%Y"))%>%
  mutate(GRADE.DATE= as.Date(GRADE.DATE, format= "%m/%d/%Y"))%>%
  mutate(RECORD.DATE= as.Date(RECORD.DATE, format= "%m/%d/%Y"))
ViolationsData<-ViolationsData %>%
  mutate(CUISINE.DESCRIPTION= as.factor(CUISINE.DESCRIPTION))
##Plot Boroughs
boroughPlot <- ggplot(ViolationsData, aes(factor(BORO)))
boroughPlot + geom_bar()
##plot Violations
violationTypePlot <- ggplot(ViolationsData, aes(factor(CRITICAL.FLAG)))
violationTypePlot + geom_bar()
##plot ViolationsYears
violationYearsPlot <- ggplot(ViolationsData, aes(factor(as.numeric( format(INSPECTION.DATE , '%Y')))))
violationYearsPlot + geom_bar()+coord_flip()
#
qplot(factor(BORO), data=ViolationsData, geom="bar", fill=factor(as.numeric( format(INSPECTION.DATE , '%Y'))))+coord_flip()
##cuisine Violations
cuisinePlot <- ggplot(ViolationsData, aes(factor(CUISINE.DESCRIPTION)))
cuisinePlot + geom_bar()
##plot ViolationCuisine
ViolationCuisinePlot <- ggplot(ViolationsData, aes(factor(CUISINE.DESCRIPTION)))
ViolationCuisinePlot + geom_bar()+coord_flip()
#qplot of violation
qplot(factor(CUISINE.DESCRIPTION), data=na.omit(ViolationsData), geom="bar", fill=factor(GRADE))+coord_flip()+
  facet_wrap(~ BORO)
library(plyr)
cusinecount<- count(ViolationsData, 'CUISINE.DESCRIPTION')
plot<-ggplot(data=cusinecount, aes(x=reorder(CUISINE.DESCRIPTION,freq), y=freq,fill=CUISINE.DESCRIPTION)) + geom_bar(stat="identity")+coord_flip()
plot+ theme(legend.position="none")#+geom_text(aes(label = sprintf("%.2f%%", freq/sum(freq) * 100)), vjust = .5,position=position_stack(vjust=0.5))
###
cusinegradecount<- count(ViolationsData, c('CUISINE.DESCRIPTION','GRADE'))
ggplot(cusinegradecount, aes(CUISINE.DESCRIPTION, freq, fill = GRADE)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

############
uniqv <-function(x) length(unique(x)) < 100
vcs <-names(ViolationsData)[sapply(ViolationsData, uniqv)]
