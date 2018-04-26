rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
data<-stl<-read.csv("C:/Users/zoeja/OneDrive/Documents/Spring2018/R/PS7/March2018.csv", header=T)

stl<-as_tibble(stl)
class(stl)
#Clean Description
n_distinct(stl$Description) #there are 181 unique entires
#I will condense the crimes into the following categories:
#homicide; rape; robery; assault; burglary; larceny; theft; 
#arson; forgery, fraud, embezzlement; weapons; sex offense; drugs; 
#disorderly conduct, obstruct govern, loitering, or public disorder; other
#===================

#extract day for grouping
as.character(stl$DateOccur)
stl$day<-substr(stl$DateOccur, 4,5) #get just the day as a column in the tibble
#create a new logical column for each category===================
stl$rape <- as.numeric(grepl(pattern="RAPE", x=stl$Description, ignore.case = T)) #these rows are the rape cases; need to be relabeled as rape
stl$homicide <- as.numeric(grepl(pattern="homicide", x=stl$Description, ignore.case = T)) #these rows are the rape cases; need to be relabeled as rape
stl$robery<-as.numeric(grepl(pattern="robery", x=stl$Description, ignore.case = T))
stl$assault<-as.numeric(grepl(pattern="assault", x=stl$Description, ignore.case = T))
stl$burglary<-as.numeric(grepl(pattern="burglary", x=stl$Description, ignore.case = T))
stl$larceny<-as.numeric(grepl(pattern="larceny", x=stl$Description, ignore.case = T))
stl$theft<-as.numeric(grepl(pattern="theft", x=stl$Description, ignore.case = T))
stl$arson<-as.numeric(grepl(pattern="arson", x=stl$Description, ignore.case = T))
stl$weapons<-as.numeric(grepl(pattern="weapons", x=stl$Description, ignore.case = T))
stl$sex<-as.numeric(grepl(pattern="sex", x=stl$Description, ignore.case = T))
stl$drugs<-as.numeric(grepl(pattern="drug", x=stl$Description, ignore.case = T))
stl$forg<-as.numeric(grepl(pattern="forg", x=stl$Description, ignore.case = T))
stl$fraud<-as.numeric(grepl(pattern="fraud", x=stl$Description, ignore.case = T))
stl$embezz<-as.numeric(grepl(pattern="embezzlement", x=stl$Description, ignore.case = T))
stl$disorder<-as.numeric(grepl(pattern="disorder", x=stl$Description, ignore.case = T))
stl$obstruct<-as.numeric(grepl(pattern="obstruct", x=stl$Description, ignore.case = T))
stl$loit<-as.numeric(grepl(pattern="loit", x=stl$Description, ignore.case = T))
#--------------
stl2<-temp <- stl %>% #create new tibble with a column that labels the crime type by a more general label
 gather('rape', 'homicide', 'robery', 'assault','burglary', 
        'larceny', 'theft', 'arson','weapons', 'sex', 'drugs',
        'forg','fraud','embezz', 'disorder', 'obstruct','loit',
        key="crimetype", value="count") 





#2 Crime by day by type=======================
stl2<-temp <- stl2 %>%
  filter(count==1)
DayByType<-temp <- stl2 %>% #create a new tibble of crime by day and type
  group_by(crimetype, day)%>%
  summarise(count=n())
which.max(temp$count)
temp[215,] #Larceny on 3/15



#3 Crime by day by neighborhood=================

DayByHood<-temp <- temp %>% #create a new tibble with crime type by neighborhood
  group_by(crimetype,Neighborhood)%>%
  summarise(count=n())
which.max(temp$count)
temp[36,] #arson in neighborhood 36

#4 proportion of robery by district===============
temp <- stl %>% #create a new tibble of proportion robbery by district
  group_by(robery, District)%>% #robery by district
  summarise(count=n()) %>%
  mutate(RobDist=count/sum(count)) # new column "RobDist" with proportion of robery over all crime

#5==================================
#Crime over TIme
library(ggplot2)
data$DateOccur<-as.Date(data$DateOccur,"%m/%d/%Y") #make date of class "Date" and format
data_date<-arrange(data, data$DateOccur)
date <- data %>% #create date variable
  filter(DateOccur > as.Date("2018-1-1")) %>%
  group_by(DateOccur) %>%
  summarise(count=n()) 

ggplot(date, aes(x=DateOccur, y=count)) + #date is the data, plot date of x axis, occurrence on y
  geom_line() + 
  labs(
    y="Number of crimes", 
    x="Date", 
    title="Crimes by dates")
#6=====================
#Crime by district over time
ggplot(date, aes(x=DateOccur, y=count,group=(District))) + 
  geom_line(aes(color=factor(District)))+
  scale_color_manual(name="District",values=c('purple', 'red','blue','green','yellow','pink','black'))+
  labs(
    y="Number of crimes", 
    x="Date", 
    title="Crimes by dates and District")
