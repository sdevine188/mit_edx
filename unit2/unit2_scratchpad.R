library(stringr)
library(dplyr)
library(readr)
library(corrplot)
library(lubridate)
library(zoo)
library(ggpairs)

# edx MIT unit 2
setwd("C:/Users/Stephen/Desktop/R/mit_edx/unit2")
list.files()

# read in wine data
wine <- read_csv("wine.csv")
dim(wine)
head(wine)

m1 <- lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(m1)


##################################################


# read in flu data
flu <- read_csv("FluTrain.csv")
dim(flu)
head(flu)
str(flu)

# split week variable into two date variables
flu$week_start <- ymd(flu$week_start)
flu$week_end <- ymd(flu$week_end)
flu$week_start <- sapply(flu$Week, function(x) unlist(str_split(x, " - ")[[1]][1]))
flu$week_end <- sapply(flu$Week, function(x) unlist(str_split(x, " - ")[[1]][2]))
min(flu$week_start)
max(flu$week_end)

# which week has highest percentage of ili visits
flu %>% filter(ILI == max(ILI))
flu %>% filter(Queries == max(Queries))

ggplot(flu, aes(x = ILI)) + geom_histogram()

ggplot(flu, aes(y = log(ILI), x = Queries)) + geom_point()

m1 <- lm(log(ILI) ~ Queries, data = flu)
summary(m1)
flu_cor <- cor(select(flu, ILI, Queries))
corrplot(flu_cor)

flu_test <- read_csv("FluTest.csv")
flu_test$pred <- predict(m1, flu_test)
flu_test$exp_pred <- exp(flu_test$pred)
filter(flu_test, grepl("2012-03-11", Week))
flu_test$rel_error <- (flu_test$ILI - flu_test$exp_pred) / flu_test$ILI
flu_test$rmse <- sqrt(mean((flu_test$ILI - flu_test$exp_pred)^2))

ici_lag2 <- lag(zoo(flu$ILI), -2, na.pad=TRUE)
flu$ili_lag2 <- lag(flu$ILI, 2)
sum(is.na(flu$ili_lag2))

ggplot(flu, aes(x = log(ili_lag2), y = log(ILI))) + geom_point()

m2 <- lm(log(ILI) ~ Queries + log(ili_lag2), flu)
summary(m2)

flu_test$ili_lag2 <- lag(flu_test$ILI, 2)
flu_test$pred <- predict(m2, flu_test)
flu_test$exp_pred <- exp(flu_test$pred)
sum(is.na(flu_test$exp_pred))

flu_test$ili_lag2[1] <- flu$ILI[416]
flu_test$ili_lag2[2] <- flu$ILI[417]

flu_test$rmse <- sqrt(mean((flu_test$ILI - flu_test$exp_pred)^2))


###############################################################


# read in climate data
list.files()
cc <- read_csv("climate_change.csv")
head(cc)

# rename CFC variables because they cause issue with lm function
names(cc)[which(grepl("CFC-11", names(cc)))] <- "cfc_11"
names(cc)[which(grepl("CFC-12", names(cc)))] <- "cfc_12"

# build training and testing sets
train <- filter(cc, Year < 2007)
test <- filter(cc, Year > 2006)

# build model 1.1
m1 <- lm(Temp ~ MEI + CO2 + CH4 + cfc_11 + cfc_12 + N2O + TSI + Aerosols, data = train)
summary(m1)

# correlation of train
train_cor <- cor(train)
corrplot(train_cor, method = "circle")
ggpairs(train)

# simpler model, bc N2O is insignificant and negative in m1
m2 <- lm(Temp ~ MEI + TSI + Aerosols + N2O, data = train)
summary(m2)

# stepwise regression on complex m1
step <- step(m1)
summary(step)

# predict on test
step_pred <- predict(step, test)
test_pred <- cbind(test, step_pred)
test_pred$error <- test_pred$Temp - test_pred$step_pred

# this is incorrect
test_pred %>% summarize(ESS = sum((test_pred$step_pred - mean(test_pred$Temp))^2),
                        ESS2 = sum((test_pred$step_pred - mean(train$Temp))^2),
                        RSS = sum((test_pred$Temp - test_pred$step_pred)^2),
                        TSS = sum((test_pred$Temp - mean(train$Temp))^2),
                        Rsquared1 = 1 - (RSS / TSS))

test_pred_sum <- test_pred %>% mutate(mean_train_temp = mean(train$Temp), tss = Temp - mean_train_temp, rss = Temp - step_pred,
                     ess1 = tss - rss, ess2 = step_pred - mean_train_temp, ess_plus_rss = ess1 + rss,
                     tss_squared = tss^2, rss_squared = rss^2,
                     ess1_squared = ess1^2, ess2_squared = ess2^2, 
                     ess2_sq_plus_rss_sq = ess2_squared + rss_squared) %>% 
        select(Temp, step_pred, mean_train_temp, tss, rss, ess1, ess2, ess_plus_rss, tss_squared, rss_squared, ess1_squared,
               ess2_squared, ess2_sq_plus_rss_sq) %>% summarize(tss_sum = sum(tss_squared), rss_sum = sum(rss_squared),
                                           ess1_sum = sum(ess1_squared), ess2_sum = sum(ess2_squared))

test_pred_sum %>% summarize()

#Explanation
#The R code to calculate the R-squared can be written as follows (your variable names may be different):
tempPredict = predict(step, newdata = test)
SSE = sum((tempPredict - test$Temp)^2)
SST = sum( (mean(train$Temp) - test$Temp)^2)
R2 = 1 - SSE/SST

# my calculations
# i don't know why i can't calculate ESS directly, but only get right answer when i back it out of R2*TSS
SST - SSE
ESS = R2*SST
ESS / SST
SSE
SST
ESS
R2
ESS + SSE


###########################################################################


# education pisa data
setwd("C:/Users/Stephen/Desktop/R/mit_edx/unit2")
list.files()

pisa_train <- data.frame(read_csv("pisa2009train.csv"))
pisa_test <- data.frame(read_csv("pisa2009test.csv"))
head(pisa_train)
dim(pisa_train)
dim(pisa_test)

pisa_train %>% filter(male == 1) %>% summarize(avg_read = mean(readingScore))
pisa_train %>% filter(male == 0) %>% summarize(avg_read = mean(readingScore))

names(pisa_train)[colSums(is.na(pisa_train)) > 0]

pisa_train <- na.omit(pisa_train)
pisa_test <- na.omit(pisa_test)

dim(pisa_train)
dim(pisa_test)
str(pisa_train)
pisa_train %>% count(male)
pisa_train %>% count(grade)
pisa_train %>% count(raceeth)

pisa_train$raceeth <- relevel(factor(pisa_train$raceeth), "White")
pisa_test$raceeth <- relevel(factor(pisa_test$raceeth), "White")

m1 <- lm(readingScore ~ ., pisa_train)
summary(m1)

pisa_train_pred <- predict(m1, pisa_train)
pisa_train_m1 <- data.frame(readingScore = pisa_train$readingScore, pred = pisa_train_pred)
pisa_train_rmse <- sqrt(mean((pisa_train_m1$readingScore - pisa_train_m1$pred)^2))

summary(m1)
pisa_train %>% count(grade)
(11*(29.542707)) - (9*(29.542707))

pisa_test$pred <- predict(m1, pisa_test)
pisa_test %>% summarize(diff = max(.$pred) - min(.$pred))
pisa_test %>% summarize(sse = sum((.$readingScore - .$pred)^2))
pisa_test_rmse <- sqrt(mean((pisa_test$readingScore - pisa_test$pred)^2))

mean(pisa_train$readingScore)
mean(pisa_train_m1$pred)

tss <- sum((pisa_test$readingScore - mean(pisa_train$readingScore))^2)
rss <- sum((pisa_test$pred - pisa_test$readingScore)^2)
1 - rss/tss
