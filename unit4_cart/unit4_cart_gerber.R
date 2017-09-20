library(stringr)
library(readr)
library(dplyr)
library(caret)
library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)

# load gerber data
setwd("C:/Users/Stephen/Desktop/R/mit_edx/unit4_cart")
list.files()
gerber <- read_csv("gerber.csv")
head(gerber)
glimpse(gerber)

# what proportion voted
gerber %>% summarize(pct_voting = sum(voting) / n())
gerber %>% filter(hawthorne == 1) %>% summarize(pct_voting = sum(voting) / n())
gerber %>% filter(civicduty == 1) %>% summarize(pct_voting = sum(voting) / n())
gerber %>% filter(neighbors == 1) %>% summarize(pct_voting = sum(voting) / n())
gerber %>% filter(self == 1) %>% summarize(pct_voting = sum(voting) / n())

# build logistic regression
gerber_m1_logistic <- glm(voting ~ hawthorne + civicduty + neighbors + self, data = gerber, family = "binomial")
summary(gerber_m1_logistic)
names(gerber_m1_logistic)
# note that $fitted values gives you probabilities, not log odds
head(gerber_m1_logistic$fitted.values)

# check accuracry using .3 threshold
gerber_m1_logistic_pred_prob <- predict(gerber_m1_logistic, newdata = gerber, type = "response")
head(gerber_m1_logistic_pred)
summary(gerber_m1_logistic_pred_prob)

gerber_m1_logistic_pred <- ifelse(gerber_m1_logistic_pred_prob >= .3, 1, 0)
confusionMatrix(gerber_m1_logistic_pred, reference = gerber$voting)

# check accuracy using .5 threshold
gerber_m1_logistic_pred <- ifelse(gerber_m1_logistic_pred_prob >= .5, 1, 0)
confusionMatrix(gerber_m1_logistic_pred, reference = gerber$voting)

# baseline model 
gerber %>% group_by(voting) %>% tally()
# not voting is majority group
gerber %>% filter(voting == 0) %>% summarize(baseline_accuracy = n() / nrow(gerber))

# compute auc
gerber_m1_logistic_roc_pred <- prediction(gerber_m1_logistic_pred, gerber$voting)
gerber_m1_logistic_auc <- performance(gerber_m1_logistic_roc_pred, measure = "auc")
gerber_m1_logistic_auc@y.values


##############################


# build cart tree
gerber_m2_cart = rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber)
gerber_m2_cart
summary(gerber_m2_cart)

prp(gerber_m2_cart)

# try second cart tree
gerber_m3_cart <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, cp = 0.0)
gerber_m3_cart
summary(gerber_m3_cart)
prp(gerber_m3_cart)
fancyRpartPlot(gerber_m3_cart)

# third cart tree with sex variable
gerber_m4_cart <- rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data = gerber, cp = 0.0)
gerber_m4_cart
summary(gerber_m3_cart)
prp(gerber_m4_cart)


##########################


# crate m5 cart with just control
gerber_m5_cart <- rpart(voting ~ control, data = gerber, cp = 0.0)
gerber_m5_cart
summary(gerber_m5_cart)
prp(gerber_m5_cart, digits = 6)

# absolute difference in pct btw treatment and control
abs(.296638 - .34)

# crate m6 cart with just control and sex
gerber_m6_cart <- rpart(voting ~ control + sex, data = gerber, cp = 0.0)
gerber_m6_cart
summary(gerber_m6_cart)
prp(gerber_m6_cart, digits = 6)

# which sex is affected more by treatment
# women
women_effect <- 0.3341757 - 0.2904558
# men
men_effect <- 0.3458183 - 0.3027947

women_effect - men_effect


# build a logisitc regression model using sex and control
gerber_m7_logistic <- glm(voting ~ sex + control, data = gerber, family = "binomial")
gerber_m7_logistic
summary(gerber_m7_logistic)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(gerber_m7_logistic, newdata = Possibilities, type = "response")
# abs diff for women control using m6 and m7
# m7 - m6
0.2908065 - 0.2904558


#################################


# build m8 logistic model with interaction term
gerber_m8_logistic <- glm(voting ~ sex + control + sex:control, data = gerber, family = "binomial")
gerber_m8_logistic
summary(gerber_m8_logistic)

# get average probabilities for sex/control categories
Possibilities
predict(gerber_m8_logistic, newdata = Possibilities, type = "response")
# m7 - m6
0.2904558 - 0.2904558












