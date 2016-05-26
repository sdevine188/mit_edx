library(readr)
library(dplyr)
library(stringr)
library(caret)
library(caTools)
library(ggplot2)
library(mice)
library(corrplot)
library(ROCR)

# unit 3
setwd("C:/Users/Stephen/Desktop/R/mit_edx/unit3")

# exercise 2
-1.5 + 3*1 -.5*5
1 / (1 + exp(1))
0.2689414 / (1 - 0.2689414)

# exercise 3
quality <- data.frame(read_csv("quality.csv"))

set.seed(88)
in_train <- createDataPartition(quality$PoorCare, p = .75, list = FALSE)
qual_train <- quality[in_train, ]
qual_test <- quality[-in_train, ]

m1 <- glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qual_train, family = binomial)
summary(m1)


# need to use their catools library to get same answer due to random splits
install.packages("caTools")

library(caTools)

set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)

m2 <- glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, family = binomial)
summary(m2)

m2_predict <- predict(m2, newdata = qualityTest, type = "response")
m2_predict_class <- sapply(m2_predict, function(x) ifelse(x > .23, 1, 0))
confusionMatrix(m2_predict_class, qualityTest$PoorCare)

# roc curve using ROCR
library(ROCR)
m2_pred <- prediction(m2_predict, qualityTest$PoorCare)
m2_plot <- performance(m2_pred, measure = "tpr", x.measure = "fpr") 
plot(m2_plot, colorize = TRUE, print.cutoffs.at = seq(0, 1, .1))
abline(a = 0, b = 1)

# accuracy curve
m2_plot_acc <- performance(m2_pred, measure = "acc")
plot(m2_plot_acc)

# find optimal cutpoint using ROCR
opt.cut = function(perf, pred){
        cut.ind = mapply(FUN=function(x, y, p){
                d = (x - 0)^2 + (y-1)^2
                ind = which(d == min(d))
                c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
                  cutoff = p[[ind]])
        }, perf@x.values, perf@y.values, pred@cutoffs)
        print(cut.ind)
}
print(opt.cut(m2_plot, m2_pred))

# get area under curve with ROCR
m2_auc <- performance(m2_pred, measure = "auc")
m2_auc@y.values

# roc curve using plotROC
library(plotROC)
qualityTest$pred <- m2_predict
ggplot(qualityTest, aes(d = PoorCare, m = pred)) + geom_roc() + style_roc()

#exercise 4
20/25
0.6

# exercise 6?
m3 <- glm(PoorCare ~ Narcotics + OfficeVisits, data = qualityTrain, family = binomial)
summary(m3)

predictTest = predict(m3, type="response", newdata = qualityTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)







# recitation
list.files()
poll <- read_csv("PollingData.csv")
dim(poll)
head(poll)
summary(poll)
sapply(poll, function(x) sum(is.na(x)))

# lots of missing data for two variables, so will use multiple imputation
simple <- select(poll, Rasmussen, SurveyUSA, DiffCount, PropR)
set.seed(144)
imputed <- mice(simple)
imputed2 <- complete(imputed)
poll2 <- poll
poll2$Rasmussen <- imputed$Rasmussen
poll2$SurveyUSA <- imputed$SurveyUSA
summary(poll2)


poll_imp <- read_csv("PollingData_Imputed.csv")

summary(poll_imp)

# create training and testing data
test <- filter(poll_imp, Year == 2012)
train <- filter(poll_imp, Year != 2012)

table(train$Republican)
# baseline model is republican wins
53 / 47 + 53
# 54% accuracy as baseline, but this is poor baseline
# compute a smart baseline using just the rasmussen poll
baseline_ras <- sign(train$Rasmussen)
length(which(baseline_ras == 1)) / length(baseline_ras)
table(baseline_ras)
table(baseline_ras, train$Republican)
(42 + 52) / (42 + 1 + 4 + 1 + 52) # 94% is smart baseline

# wanted to try confusion matrix, but its better to just use table and manually calculate accuracy, etc
# because to use confusion matrix, you need to fudge the two ties - see below notes
# convert baseline_ras to have 0 for democratic prediction, instead of -1
# convert baseline_ras 0 for tie into a 2 which will always fail
baseline_ras2 <- str_replace(baseline_ras, "0", "2")
baseline_ras2 <- str_replace(baseline_ras2, "-1", "0")
# manually convert two tied rasmussen polls to 1 or 0, 
# since confusion matrix cant let predictions have more factor levels than the reference data
baseline_ras2[50] <- 1
baseline_ras2[70] <- 0
table(baseline_ras2)
confusionMatrix(factor(baseline_ras2), factor(train$Republican))

# look at potential multicollinearity
train_corr <- cor(select(train, Rasmussen, SurveyUSA, DiffCount, PropR, Republican))
corrplot(train_corr)

# try model just using PropR, which is most highly correlated with Republican
m1 <- glm(Republican ~ PropR, data = train, family = "binomial")
summary(m1)

# evaluate roc curve for m1 on train
m1_train_pred <- predict(m1, train, type = "response")
m1_train_prediction <- prediction(m1_train_pred, train$Republican)

m1_perf <- performance(m1_train_prediction, measure = "tpr", x.measure = "fpr")
plot(m1_perf,  colorize = TRUE, print.cutoffs.at = c(.1, .5, .9))

m1_perf2 <- performance(m1_train_prediction, measure = "tpr")
plot(m1_perf2)

# try using accuracy
m1_perf_acc <- performance(m1_train_prediction, measure = "acc")
plot(m1_perf_acc)

# find optimal cutoff based on tpr and tnr
m1_perf <- performance(m1_train_prediction, measure = "tpr", x.measure = "fpr")
class(m1_perf)
m1_perf
m1_train_prediction

opt.cut = function(perf, pred){
        cut.ind = mapply(FUN=function(x, y, p){
                d = (x - 0)^2 + (y-1)^2
                ind = which(d == min(d))
                c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
                  cutoff = p[[ind]])
        }, perf@x.values, perf@y.values, pred@cutoffs)
        print(cut.ind)
}
opt.cut(m1_perf, m1_train_prediction)
# .6655 is optimal cut maximizing combination of tpr and tnr

# get accuracy on training using this optimal cutpoint
train$pred <- ifelse(m1_train_pred > .6655, 1, 0)
confusionMatrix(train$pred, train$Republican) # 96% accuracy vs 94% smart baseline

# get accuracy on test data
test$pred <- predict(m1, newdata = test, type = "response")
test$pred <- ifelse(test$pred > .6655, 1, 0)
confusionMatrix(test$pred, test$Republican) # actually does better on test, 97.7%


# new model with surveyUSA and DiffCount, because they are relatively less correlated with each other
m2 <- glm(Republican ~ SurveyUSA + DiffCount, data = train)
summary(m2)
m2_pred <- predict(m2, train, type = "response")
m2_pred <- ifelse(m2_pred > .5, 1, 0)
confusionMatrix(m2_pred, train$Republican) # 97%

# test model2 on test set
m2_pred_test <- predict(m2, test, type = "response")
m2_pred_test <- ifelse(m2_pred_test > .5, 1, 0)
confusionMatrix(m2_pred_test, test$Republican) # 97.7%

# check testing smart baseline
table(sign(test$Rasmussen), test$Republican)
(18 + 21) / (18 + 2 + 4 + 21) # .8666667%
