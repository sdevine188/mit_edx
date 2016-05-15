library(stringr)
library(dplyr)
library(readr)
library(corrplot)
library(lubridate)
library(zoo)

# edx MIT unit 2
setwd("C:/Users/Stephen/Desktop/R/mit_edx/unit2")
list.files()

# read in wine data
wine <- read_csv("wine.csv")
dim(wine)
head(wine)

m1 <- lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(m1)



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

# read in climate data
list.files()
cc <- read_csv("climate_change.csv")

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
test_pred %>% summarize(ESS = sum((test_pred$step_pred - mean(test_pred$Temp))^2),
                        RSS = sum((test_pred$step_pred - test_pred$Temp)^2),
                        TSS = ESS + RSS, Rsquared = ESS / TSS)

SSE = sum((tempPredict - test$Temp)^2)

SST = sum( (mean(train$Temp) - test$Temp)^2)





# education pisa data
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

pisa_train$raceeth <- relevel(factor(pisa_train$raceeth), "White")
pisa_test$raceeth <- relevel(factor(pisa_test$raceeth), "White")

m1 <- lm(readingScore ~ ., pisa_train)
summary(m1)

pisa_train_pred <- predict(m1, pisa_train)
pisa_train_m1 <- data.frame(readingScore = pisa_train$readingScore, pred = pisa_train_pred)
pisa_train_rmse <- sqrt(mean((pisa_train_m1$readingScore - pisa_train_m1$pred)^2))

pisa_test$pred <- predict(m1, pisa_test)
pisa_test %>% summarize(diff = max(.$pred) - min(.$pred))
pisa_test %>% summarize(sse = sum((.$readingScore - .$pred)^2))
pisa_test_rmse <- sqrt(mean((pisa_test$readingScore - pisa_test$pred)^2))
pisa_test %>% 
        summarize(r2 = sum((.$pred - mean(.$readingScore))^2) / 
                          sum((.$readingScore - mean(.$readingScore))^2))

sum((pisa_test$readingScore - mean(pisa_test$readingScore))^2)

