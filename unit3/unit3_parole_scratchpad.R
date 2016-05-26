# unit3 parole assignment
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

parole <- data.frame(read_csv("parole.csv"))
dim(parole)
str(parole)
head(parole)
names(parole)
summary(parole)
table(parole$race)
table(parole$crime)
table(parole$multiple.offenses)
table(parole$violator)
table(parole$state)

# convert crime and state to factors
parole$crime <- factor(parole$crime)
parole$state <- factor(parole$state)

# create training and test set
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

# in caret for fun
in_train <- createDataPartition(parole$violator, p = .7, list = FALSE)
train_caret <- parole[in_train, ]
test_caret <- parole[-in_train, ]

# look for correlations
parole_cor <- parole %>% select(-crime, -state) %>% cor(.)
corrplot(parole_cor) # nothing significant

# model 1
m1 <- glm(violator ~ ., data = train, family = "binomial")
summary(m1)

# interpret coefficient on multiple.offenses as odds
exp(1.6119919)

# problem 4.3
m1_logit <- -4.2411574 + 0.3869904 + 0.8867192 + -0.0001756*50 + -0.1238867*3 + 0.0802954*12 +  0.6837143
odds <- exp(m1_logit)
prob <- 1 / (1 + exp(-(m1_logit)))

# predict m1 on test set
test$m1_pred <- predict(m1, newdata = test, type = "response")
head(test)
max(test$m1_pred)
test$m1_pred_class <- ifelse(test$m1_pred > .5, 1, 0)
head(test)
confusionMatrix(test$m1_pred_class, test$violator, positive = "1")

m1_pred <- prediction(test$m1_pred, test$violator)
m1_plot <- performance(m1_pred, measure = "tpr", x.measure = "fpr")
plot(m1_plot, colorize = TRUE)

m1_plot_acc <- performance(m1_pred, measure = "acc")
plot(m1_plot_acc)

# find area under roc curve
# The probability the model can correctly differentiate between a 
# randomly selected parole violator and a randomly selected parole non-violator.
auc <- performance(m1_pred, measure = "auc")
auc@y.values

# find baseline prediction
table(test$violator)
179 / (23 + 179)
