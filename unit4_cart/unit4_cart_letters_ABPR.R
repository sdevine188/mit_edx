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
letters <- read_csv("letters_ABPR.csv")

# create dummy when letter=B
letters$isB = as.factor(letters$letter == "B")

# split train and test
set.seed(1000)
in_train <- sample.split(letters$isB, SplitRatio = .5)
letters_train <- subset(letters, in_train == TRUE)
letters_test <- subset(letters, in_train == FALSE)

# find baseline model on test set
letters_test %>% group_by(isB) %>% tally()
letters_test %>% filter(isB == FALSE) %>% summarize(baseline_accuracy = n() / nrow(letters_test))


###########################################


# build m1 cart model
letters_m1_cart <- rpart(isB ~ . - letter, data = letters_train, method = "class")
letters_m1_cart
prp(letters_m1_cart)

letters_m1_cart_pred <- predict(letters_m1_cart, newdata = letters_test, type = "class")
confusionMatrix(letters_m1_cart_pred, reference = letters_test$isB)


###############################


# build m2 rf model
set.seed(1000)
letters_m2_rf <- randomForest(isB ~ . - letter, data = letters_train, method = "class")
letters_m2_rf
summary(letters_m2_rf)
(1165 + 369) / (1165 + 369 + 14 + 10)


##################################


# create new train and test so we can predict multi-class outcomes
letters$letter = as.factor(letters$letter)
set.seed(2000)
in_train <- sample.split(letters$isB, SplitRatio = .5)
letters_train <- subset(letters, in_train == TRUE)
letters_test <- subset(letters, in_train == FALSE)

# compute baseline accuracy
letters_test %>% group_by(letter) %>% tally()
letters_test %>% filter(letter == "P") %>% summarize(baseline_accuracy = n() / nrow(letters_test))

# build m3 rpart
letters_m3_cart <- rpart(letter ~ . - isB, data = letters_train, method = "class")
letters_m3_cart
prp(letters_m3_cart)

letters_m3_cart_pred <- predict(letters_m3_cart, newdata = letters_test, type = "class")
confusionMatrix(letters_m3_cart_pred, reference = letters_test$letter)

letters_m3_cart <- rpart(letter ~ . - isB, data=letters_train, method="class")
predictLetter = predict(letters_m3_cart, newdata=letters_test, type="class")
table(letters_test$letter, predictLetter)


########################################


# build m4 rf model
set.seed(1000)
letters_train_m4_rf <- randomForest(letter ~ . - isB, data=letters_train, method="class")
letters_train_m4_rf
summary(letters_train_m4_rf)

letters_test_m4_rf_pred <- predict(letters_train_m4_rf, newdata = letters_test, type = "class")
confusionMatrix(letters_test_m4_rf_pred, reference = letters_test$letter)
