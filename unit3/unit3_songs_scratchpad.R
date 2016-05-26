# unit 3 assignment on songs
library(dplyr)
library(stringr)
library(readr)
library(caret)
library(ggplot2)
library(mice)
library(corrplot)
library(ROCR)


setwd("C:/Users/Stephen/Desktop/R/mit_edx/unit3")
list.files()

# read in data
songs <- data.frame(read_csv("songs.csv"))
head(songs)
str(songs)
summary(songs)
names(songs)

songs %>% filter(year == 2010) %>% summarize(count = n())
songs %>% filter(grepl("Michael Jackson", artistname)) %>% summarize(count = n())
songs %>% filter(grepl("Michael Jackson", artistname), Top10 == 1) %>% select(songtitle)

x <- songs$timesignature
unique(songs$timesignature)
mode <- function(x) {
        unique_x <- unique(x)
        unique_x[which.max(tabulate(match(x, unique_x)))]
}
songs %>% group_by(timesignature) %>% summarize(count = n()) %>% filter(count == max(count)) %>% 
        select(timesignature)

songs %>% filter(tempo == max(tempo)) %>% select(songtitle)

# create training and test data
train <- filter(songs, year < 2010)
test <- filter(songs, year == 2010)
dim(train)

# create m1 with all numeric variables as predictors
train2 <- train %>% select(-year, -songtitle, -artistname, -songID, -artistID)
m1 <- glm(Top10 ~ ., data = train2, family = "binomial")
summary(m1)

train2_corr <- cor(train2)
train2_corr
cor(train2$energy, train2$loudness)
corrplot(train2_corr)
# energy and loudness are highly correlated

# remove loudness
m2 <- glm(Top10 ~ . - loudness, data = train2, family = "binomial")
summary(m2)

# remove energy
m3 <- glm(Top10 ~ . - energy, data = train2, family = "binomial")
summary(m3)

# predict m3 on test data
m3_predict <- predict(m3, newdata = test, type = "response")
test$predict_m3 <- m3_predict
test$predict_m3_dummy <- ifelse(test$predict_m3 > .45, 1, 0)
confusionMatrix(test$predict_m3_dummy, test$Top10, positive = "1") # .8794
unique(test$predict_m3)

# use rocr to see roc and accuracy curve based on threshold
m3_pred <- prediction(m3_predict, test$Top10)
m3_plot <- performance(m3_pred, measure = "tpr", x.measure = "fpr")
plot(m3_plot, colorize = TRUE)

m3_plot_acc <- performance(m3_pred, measure = "acc")
plot(m3_plot_acc)

opt.cut = function(perf, pred){
        cut.ind = mapply(FUN=function(x, y, p){
                d = (x - 0)^2 + (y-1)^2
                ind = which(d == min(d))
                c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
                  cutoff = p[[ind]])
        }, perf@x.values, perf@y.values, pred@cutoffs)
        print(cut.ind)
}
opt.cut(m3_plot, m3_pred)

# find baseline model for comparison
test %>% group_by(Top10) %>% summarize(count = n())
table(test$Top10) 

# not Top10 is most common, so that's baseline prediction
314 / (314 + 59) # .8418

# count of true positives
test %>% filter(predict_m3_dummy == 1, Top10 == 1) %>% summarize(count = n()) # 19

# count of false positives
test %>% filter(predict_m3_dummy == 1, Top10 == 0) %>% summarize(count = n()) # 5

# calculate sensitivity
# TP / TP + FN
tp <- test %>% filter(predict_m3_dummy == 1, Top10 == 1) %>% summarize(count = n()) %>% .[1, 1]
fn <- test %>% filter(predict_m3_dummy == 0, Top10 == 1) %>% summarize(count = n()) %>% .[1, 1]
tp / (tp + fn) # .322, oddly confusionMatrix gets this value as specificity??

# calculate specificity
# TN / (TN + FP)
tn <- test %>% filter(predict_m3_dummy == 0, Top10 == 0) %>% summarize(count = n()) %>% .[1, 1]
fp <- test %>% filter(predict_m3_dummy == 1, Top10 == 0) %>% summarize(count = n()) %>% .[1, 1]
tn / (tn + fp) # .984

# positive predictive value
# TP / TP + FP
tp / (tp + fp)
