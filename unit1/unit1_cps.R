library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(ggplot2)
library(reshape2)

# unit 1 - stock prices
setwd("c:/users/stephen/desktop/r/mit_edx/unit1")
list.files()

# read in data
cps <- read.csv("CPSData.csv", stringsAsFactors = FALSE)
dim(cps)
head(cps)
names(cps)
str(cps)

cps %>% group_by(Industry) %>% summarize(count = n()) %>% filter(!is.na(Industry)) %>% 
        filter(count == max(count))

cps %>% group_by(State) %>% summarize(count = n()) %>% filter(count == min(count))
cps %>% group_by(State) %>% summarize(count = n()) %>% filter(count == max(count))

citizens <- nrow(filter(cps, Citizenship %in% c("Citizen, Native", "Citizen, Naturalized")))
citizens / nrow(cps)

cps %>% group_by(Race) %>% filter(Hispanic == 1) %>% summarize(count = n())

apply(cps, 2, function(x) sum(is.na(x)))

cps %>% group_by(Region) %>% summarize(na_pct = sum(is.na(Married)) / length(Married))
cps %>% group_by(Sex) %>% summarize(na_pct = sum(is.na(Married)) / length(Married))
cps %>% group_by(Age) %>% summarize(na_pct = sum(is.na(Married)) / length(Married)) %>% data.frame(.)
cps %>% group_by(Age) %>% summarize(na_pct = sum(is.na(Married))) %>% data.frame(.)

cps %>% group_by(Citizenship) %>% summarize(na_pct = sum(is.na(Married)) / length(Married)) %>% data.frame(.)

cps %>% group_by(State) %>% summarize(metro_na_pct = sum(is.na(MetroAreaCode)) / length(MetroAreaCode)) %>%
        filter(metro_na_pct == max(metro_na_pct))
cps %>% group_by(State) %>% summarize(metro_na_pct = sum(!is.na(MetroAreaCode)) / length(MetroAreaCode)) %>%
        filter(metro_na_pct == max(metro_na_pct))

cps %>% group_by(Region) %>% summarize(pct_non_metro = sum(is.na(MetroAreaCode)) / length(MetroAreaCode))
cps %>% group_by(State) %>% summarize(pct_non_metro = sum(is.na(MetroAreaCode)) / length(MetroAreaCode)) %>%
        filter(pct_non_metro > .25, pct_non_metro < .49)
cps %>% group_by(State) %>% summarize(pct_non_metro_30 = abs(.3 - (sum(is.na(MetroAreaCode)) / length(MetroAreaCode)))) %>%
        filter(pct_non_metro_30 == min(pct_non_metro_30))

cps %>% group_by(State) %>% filter(!sum(is.na(MetroAreaCode)) == 0) %>% 
        summarize(pct_non_metro = sum(is.na(MetroAreaCode)) / length(MetroAreaCode)) %>%
        filter(pct_non_metro == min(pct_non_metro))

cps %>% group_by(State) %>% summarize(metro_na_pct = sum(is.na(MetroAreaCode)) / length(MetroAreaCode)) %>%
        arrange(desc(metro_na_pct))

metro_area_codes <- read.csv("MetroAreaCodes.csv")
country_codes <- read.csv("CountryCodes.csv")

dim(metro_area_codes)
dim(country_codes)

cps2 <- left_join(cps, metro_area_codes, by = c("MetroAreaCode" = "Code"))
cps2 %>% summarize(na_metro = sum(is.na(MetroArea)))
cps2 %>% group_by(MetroArea) %>% summarize(count = n()) %>% 
        filter(MetroArea %in% c("Atlanta-Sandy Springs-Marietta, GA", "Baltimore-Towson, MD",
                                "Boston-Cambridge-Quincy, MA-NH", "San Francisco-Oakland-Fremont, CA"))

cps2 %>% group_by(MetroArea) %>% summarize(pct_hispanic = sum(Hispanic) / length(Hispanic)) %>%
        filter(pct_hispanic == max(pct_hispanic))

cps2 %>% group_by(MetroArea) %>% summarize(pct_asian = sum(Race == "Asian") / length(Race)) %>%
        filter(pct_asian >= .2) %>% nrow(.)

cps2 %>% group_by(MetroArea) %>% filter(!is.na(Education)) %>% 
        summarize(pct_no_hs = sum(Education == "No high school diploma") / length(Education)) %>% 
        filter(pct_no_hs == min(pct_no_hs))

country_codes$Code <- as.character(country_codes$Code)
cps2$CountryOfBirthCode <- as.character(cps2$CountryOfBirthCode)
cps2 <- left_join(cps2, country_codes, by = c("CountryOfBirthCode" = "Code"))
cps2 %>% summarize(na_count = sum(is.na(Country)))

cps2 %>% group_by(Country) %>% summarize(count = n()) %>% arrange(desc(count)) %>% head(.)

cps2 %>% filter(MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA") %>%
        summarize(pct_foreign = sum(Country != "United States", na.rm = TRUE) / length(Country))

cps2 %>% group_by(MetroArea) %>% summarize(count_india = sum(Country == "India", na.rm = TRUE)) %>% 
        arrange(desc(count_india)) %>% head(.)

cps2 %>% group_by(MetroArea) %>% summarize(count_brazil = sum(Country == "Brazil", na.rm = TRUE)) %>% 
        arrange(desc(count_brazil)) %>% head(.)
cps2 %>% group_by(MetroArea) %>% summarize(count_somalia = sum(Country == "Somalia", na.rm = TRUE)) %>% 
        arrange(desc(count_somalia)) %>% head(.)

 
        
