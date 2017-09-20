library(stringr)
library(dplyr)
library(readr)
library(lubridate)
library(ggpairs)
library(ggplot2)
library(zoo)

# edx MIT unit 2 - linear regression - detecting flu epidemics
setwd("C:/Users/Stephen/Desktop/R/mit_edx/unit2")
list.files()

# read in data
flu_train <- read_csv("FluTrain.csv")
glimpse(flu_train)

# which week has highest percentage of ILI visits?
flu_train %>% filter(ILI == max(ILI))

# histogram of ILI
ggplot(flu_train, aes(x = ILI)) + geom_histogram()

# create log(ILI)
flu_train$log_ILI <- log(flu_train$ILI)

# plot log_ILI against queries
ggplot(flu_train, aes(x = Queries, y = log_ILI)) + geom_point()

# build model1
m1 <- lm(log(ILI) ~ Queries, flu_train)
summary(m1)

# explore relationship btw correlation and R2 in two-variable OLS
cor(flu_train$Queries, log(flu_train$ILI))
cor(flu_train$Queries, log(flu_train$ILI))^2

# predict on flu_train
list.files()
flu_test <- read_csv("FluTest.csv")
head(flu_test)
flu_test_pred <- exp(predict(m1, flu_test))
flu_test$pred <- flu_test_pred

# find predicted ILI for week of 2012-03-11
flu_test %>% filter(grepl("2012-03-11", Week, ignore.case = TRUE))

# find relative error for prediction of week of 2012-03-11
flu_test %>% filter(grepl("2012-03-11", Week, ignore.case = TRUE)) %>% 
        summarize(rel_error = (ILI - pred) / ILI)

# find rmse on flu_test
sqrt(mean((flu_test$pred - flu_test$ILI)^2))

# create ili_lag2
ILI_lag2 <- lag(zoo(flu_train$ILI), 2, na.pad = TRUE)
head(ILI_lag2)
head(flu_train$ILI)
str(ILI_lag2)
str(coredata(ILI_lag2))
flu_train$ILI_lag2 <- coredata(ILI_lag2)
sum(is.na(flu_train$ILI_lag2))

# plot relationship btw logs of ILI and ILI_lag2
ggplot(flu_train, aes(x = log(ILI_lag2), y = log(ILI))) + geom_point()

# create model2 
m2 <- lm(log(ILI) ~ Queries + log(ILI_lag2), flu_train)
summary(m2)
AIC(m2)

# add ILI_lag2 to flu_test
flu_test$ILI_lag2 <- lag(zoo(flu_test$ILI), 2, na.pad = TRUE)
flu_test_pred2 <- exp(predict(m2, flu_test))
flu_test$pred2 <- flu_test_pred2
sum(is.na(flu_test$pred2))

# add NA values of flu_test based on ending values of flu_train
dim(flu_train)
flu_test$ILI_lag2[1] <- flu_train$ILI[416]
flu_test$ILI_lag2[2] <- flu_train$ILI[417]
flu_test$ILI_lag2[1:2]
flu_train$ILI[416:417]

# predict model2 on flu_test
flu_test_pred2_2 <- exp(predict(m2, flu_test))
flu_test$pred_2_2 <- flu_test_pred2_2

# create rmse for pred_2_2
sqrt(mean((flu_test$ILI - flu_test$pred_2_2)^2))








