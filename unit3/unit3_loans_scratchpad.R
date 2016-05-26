# unit3 loans assignment
library(dplyr)
library(stringr)
library(readr)
library(caret)
library(ggplot2)
library(mice)
library(corrplot)
library(ROCR)
library(VIM)

setwd("C:/Users/Stephen/Desktop/R/mit_edx/unit3")
list.files()

# read in data
# not fully paid is dependent variable
loans <- data.frame(read_csv("loans.csv"))
dim(loans)
str(loans)
head(loans)
table(loans$not.fully.paid)

1533 / (8045 + 1533)
# baseline prediction accuracy
8045 / (8045 + 1533)
sapply(loans, function(x) sum(is.na(x)))
loans_NA <- loans %>% filter(is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) |
                                     is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
dim(loans_NA)
table(loans_NA$not.fully.paid)
12 / (50 + 12)

# use mice and vim package for NA diagnostics and imputation
md.pattern(loans)
aggr_plot <- aggr(loans, col = c('navyblue','red'), numbers = TRUE, sortVars = TRUE, 
                  labels = names(loans), cex.axis = .7, gap = 3, 
                  ylab = c("Histogram of missing data", "Pattern"))

var_imp <- setdiff(names(loans), "not.fully.paid")
set.seed(144)
loans_imp <- mice(loans[ , var_imp])
summary(loans_imp)
# see the imputations of variable from all five rounds
loans_imp$imp$log.annual.inc
loans_imp <- complete(loans_imp)
# check for NAs
sapply(loans_imp, function(x) sum(is.na(x)))
# check final imputed for fun; it uses the first column in the loans_imp$imp$log.annual.inc matrix
loans_imp[7727, ]
# re-add dependent variable to imputed data
loans_imp$not.fully.paid <- loans$not.fully.paid 

# split data in train and test sets
set.seed(144)
in_train <- sample.split(loans_imp$not.fully.paid, SplitRatio = .7)
train <- loans_imp[in_train, ]
test <- loans_imp[!in_train, ]
 
# model 1
m1 <- glm(not.fully.paid ~ ., data = train, family = "binomial")
summary(m1)

# test of m1 fit
# roc curve
m1_predict <- predict(m1, newdata = test, type = "response")
m1_predict_class <- ifelse(m1_predict > .5, 1, 0)
confusionMatrix(m1_predict_class, test$not.fully.paid, positive = "1")
m1_prediction <- prediction(m1_predict, test$not.fully.paid)
m1_plot_roc <- performance(m1_prediction, measure = "tpr", x.measure = "fpr")
plot(m1_plot_roc, colorize = TRUE)

# accuracy curve
m1_plot_acc <- performance(m1_prediction, measure = "acc")
plot(m1_plot_acc)

# auc
m1_auc <- performance(m1_prediction, measure = "auc")
m1_auc@y.values 

# model2 using just interest rate
m2 <- glm(not.fully.paid ~ int.rate, data = train, family = "binomial")
summary(m2)

# check for correlation of independent variables
cor_train <- cor(select(train, -purpose))
corrplot(cor_train)

# evaluate m2 on test set
m2_predict <- predict(m2, newdata = test, type = "response")
m2_predict_class <- ifelse(m2_predict > .5, 1, 0)
confusionMatrix(m2_predict_class, test$not.fully.paid, positive = "1")
max(m2_predict)
test$m2.predict <- m2_predict

# m2 roc curve
m2_prediction <- prediction(m2_predict, test$not.fully.paid)
m2_plot_roc <- performance(m2_prediction, measure = "tpr", x.measure = "fpr")
plot(m2_plot_roc, colorize = TRUE)

# m2 accuracy
m2_plot_acc <- performance(m2_prediction, measure = "acc")
plot(m2_plot_acc)

# m2 auc
m2_auc <- performance(m2_prediction, measure = "auc")
m2_auc@y.values

# investment returns
10*exp(.06*3)
test$profit <- exp(test$int.rate * 3) - 1
test$profit[test$not.fully.paid == 1] <- -1
max(test$profit) * 10
sum(test$profit) / nrow(test)

# look at just high-interest loans
hi_test <- filter(test, int.rate >= .15)
mean(hi_test$profit)
table(hi_test$not.fully.paid)
110 / (327 + 110)
cutoff <- arrange(hi_test, m2.predict) %>% select(m2.predict) %>% slice(., 100)
hi_test_low_int <- filter(hi_test, m2.predict <= cutoff[1, 1])
hi_test_low_int <- arrange(hi_test_low_int, m2.predict)
hi_test_low_int <- hi_test_low_int[1:100, ]
sum(hi_test_low_int$profit)
table(hi_test_low_int$not.fully.paid)
