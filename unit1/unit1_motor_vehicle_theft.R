library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(ggplot2)

# unit 1
setwd("c:/users/stephen/desktop/r/mit_edx/unit1")
list.files()

# load mvtweek1, motor vehicle theft in chicago
mvt <- read_csv("mvtWeek1.csv")
glimpse(mvt)
dim(mvt)
max(mvt$ID)
head(mvt)
min(mvt$Beat)
length(which(mvt$Arrest == TRUE))
length(which(mvt$LocationDescription == "ALLEY"))


DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
summary(mvt)
median(mvt$Date)
dates <- mdy_hm(mvt$Date)
median(dates)

mvt$Month = months(DateConvert)

mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

mvt %>% group_by(Year, Month) %>% summarize(mvt_count = sum(Arrest)) %>% arrange(mvt_count) %>% data.frame(.)
table(mvt$Month)
mvt %>% group_by(Month) %>% summarize(obs_count = n()) %>% filter(obs_count == min(obs_count))
mvt %>% group_by(Weekday) %>% summarize(obs_count = n()) %>% filter(obs_count == max(obs_count))
mvt %>% group_by(Month) %>% summarize(mvt_count = sum(Arrest)) %>% arrange(desc(mvt_count)) %>% data.frame(.)


hist(mvt$Date, breaks=100)
mvt %>% mutate(period = case_when(.$Year < 2007 ~ 1, .$Year > 2006 ~ 2)) %>% 
        filter(Arrest == TRUE) %>% ggplot(aes(x = period, y = Arrest)) + geom_boxplot()
mvt %>% filter(Year < 2007) %>% 
        summarize(thefts = n(), arrests = sum(Arrest), pct_arrests = arrests / thefts)

mvt %>% filter(Year > 2006) %>% 
        summarize(thefts = n(), arrests = sum(Arrest), pct_arrests = arrests / thefts)

mvt %>% filter(Year == 2012) %>% 
        summarize(thefts = n(), arrests = sum(Arrest), pct_arrests = arrests / thefts)


names(mvt)
head(mvt)
mvt %>% group_by(LocationDescription) %>% summarize(count = n()) %>% arrange(desc(count)) %>% 
        filter(!LocationDescription == "OTHER") %>% slice(1:5)

top5 <- mvt %>% filter(LocationDescription %in% c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", 
                "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL"))
unique(top5$LocationDescription)
dim(top5)
top5 %>% group_by(LocationDescription) %>% summarize(thefts = n(), arrests = sum(Arrest), 
                                                     pct_arrests = arrests / thefts)
top5 %>% filter(LocationDescription == "GAS STATION") %>% group_by(Weekday) %>% summarize(count = n())
top5 %>% filter(LocationDescription == "DRIVEWAY - RESIDENTIAL") %>% group_by(Weekday) %>%
        summarize(count = n()) %>% filter(count == min(count))