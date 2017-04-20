rm(list=ls())
library(ggplot2)
library(viridis)
library("dplyr")

ViolationsData <- read.csv(file="/Users/lakshyagarg/Documents/Columbia/Spring2017/ExplDataAndViz/Project/DOHMH_New_York_City_Restaurant_Inspection_Results.csv", 
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

###########
ViolationsData <- read.csv(file="/Users/lakshyagarg/Documents/Columbia/Spring2017/ExplDataAndViz/Project/DOHMH_New_York_City_Restaurant_Inspection_Results.csv", 
                           header=TRUE, sep=",",as.is=TRUE)
library(zipcode)
data(zipcode)
library(dplyr)
zipsNYC<-zipcode %>% filter(state=='NY')
joined<-merge(zipsNYC, ViolationsData, by.x='zip', by.y='ZIPCODE')#by = c('zip' = 'ZIPCODE'))
g <- ggplot(data=joined) + geom_point(aes(x=longitude, y=latitude, colour=GRADE))
g <- g + theme_bw() + scale_x_continuous(limits = c(-75,-73), breaks = NULL)
g <- g + scale_y_continuous(limits = c(40,41), breaks = NULL)
g
# don't need axis labels
g = g + labs(x=NULL, y=NULL)
library(ggmap)
map<-get_map(location='united states', zoom=4, maptype='roadmap')
ggmap(map)+geom_point(aes(x=longitude, y=latitude), data=joined, alpha=.5)

insec_data <- state_feature %>% select(lat, long, group, order, region, abb, FOODINSEC_00_02, FOODINSEC_07_09, FOODINSEC_10_12) %>% gather("Year","Value", -lat, -long, -group, -order, -region, -abb)

ggplot(insec_data, aes(x = long, y = lat, fill = Value, group = group)) + 
  geom_polygon(color = "white") +
  scale_fill_continuous(low = "#fee5d9", high = "#a50f15", guide="colorbar")+
  coord_fixed(1.3)+
  theme_bw() +
  facet_wrap(~Year) +
  ggtitle("Food Insecurity in USA (2000-2012)") +
  labs(x="",y="")

joined1 <- joined %>% 
  mutate(GRADENEW = ifelse(GRADE=="", "missing", GRADE))
joined1<-joined1 %>% select(latitude, longitude,GRADENEW)
sapply(joined1, function(x) sum(is.na(x)))
theme_set(theme_bw(16))
NYCMap <- qmap("NY", zoom = 10, color = "bw", legend = "topleft")
NYCMap +
  geom_point(aes(x = as.numeric(as.character(longitude)), y = as.numeric(as.character(latitude)), colour = GRADENEW),
             data = joined1)

qmap("Queens", zoom = 10, color = "bw", legend = "topleft")
#+qmap("bronx", zoom = 12, color = "bw", legend = "topleft")+


lat <- c(-75,-73)                
lon <- c(40,41)   

map <- get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 14,
               maptype = "satellite", source = "google")

### When you draw a figure, you limit lon and lat.      
foo <- ggmap(map)#+
  scale_x_continuous(limits = c(11.33, 11.36), expand = c(0, 0)) +
  scale_y_continuous(limits = c(44.49, 44.5), expand = c(0, 0))

foo
counties <- map_data("county")
ny_county <- subset(counties, region == "new york")

foo

library(ggmap)
cap  <- geocode("Bronx")
wash <- geocode("brooklyn")
loc  <- unlist((cap+wash)/2)
ggmap(get_map(location=loc,zoom=15))+coord_fixed(ylim=loc[2]+.005*c(-1,+1))
##CHECKOUT
##https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/ggmap/ggmapCheatsheet.pdf

## works
mmap <- get_map(location = c(-74.5,40,-73,41),source="google")
ggmap(mmap) #+ coord_fixed(ylim = c(-74,-73))
