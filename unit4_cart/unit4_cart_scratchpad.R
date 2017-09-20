library(stringr)
library(readr)
library(dplyr)
library(caret)
library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)

# load stevens data
setwd("C:/Users/Stephen/Desktop/R/mit_edx/unit4_cart")
list.files()
stevens <- read_csv("stevens.csv")
glimpse(stevens)
stevens$Reverse <- factor(stevens$Reverse)
stevens$Docket <- factor(stevens$Docket)
stevens$Circuit <- factor(stevens$Circuit)
stevens$Issue <- factor(stevens$Issue)
stevens$Petitioner <- factor(stevens$Petitioner)
stevens$Respondent <- factor(stevens$Respondent)
stevens$LowerCourt <- factor(stevens$LowerCourt)



# create trainng and test data
set.seed(3000)
in_train <- sample.split(stevens$Reverse, SplitRatio = .7)
stevens_train <- subset(stevens, in_train == TRUE)
stevens_test <- subset(stevens, in_train == FALSE)

# in_train <- createDataPartition(stevens$Reverse, p = .7, list = FALSE)
# stevens_train <- stevens[in_train, ]
# stevens_test <- stevens[-in_train, ]

dim(stevens)
dim(stevens_train)
dim(stevens_test)


##################################################


# build cart model
stevens_train_m1_rpart <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = stevens_train, 
                          method = "class", minbucket = 25)
names(stevens_train_m1_rpart)
stevens_train_m1_rpart
prp(stevens_train_m1_rpart)
fancyRpartPlot(stevens_train_m1_rpart)
stevens_test_m1_rpart_pred <- predict(stevens_train_m1_rpart, newdata = stevens_test, type = "class")
confusionMatrix(stevens_test_m1_rpart_pred, reference = stevens_test$Reverse)

# caret performs better than rpart
stevens_train_m1_caret <- train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,  
                          method = "rpart", data = stevens_train)
names(stevens_train_m1_caret)
stevens_train_m1_caret$finalModel
fancyRpartPlot(stevens_train_m1_caret$finalModel)
prp(stevens_train_m1_caret$finalModel)
stevens_test_m1_caret_pred <- predict(stevens_train_m1_caret, newdata = stevens_test, method = "rpart")
confusionMatrix(stevens_test_m1_caret_pred, reference = stevens_test$Reverse)
head(stevens_test$LowerCourt)
head(stevens_test_m1_caret_pred)

# confirm caret model is just using LowerCourt
stevens_test_manual_pred <- ifelse(stevens_test$LowerCourt == "liberal", 0, 1)
confusionMatrix(stevens_test_manual_pred, reference = stevens_test$Reverse)


############################################3


# find roc on rpart model
stevens_test_m1_rpart_pred_prob <- predict(stevens_train_m1_rpart, newdata = stevens_test)
stevens_test_m1_rpart_roc_pred <- prediction(stevens_test_m1_rpart_pred_prob[ , 2], stevens_test$Reverse)

stevens_test_m1_rpart_roc_perf <- performance(stevens_test_m1_rpart_roc_pred, measure = "tpr", x.measure = "fpr")
plot(stevens_test_m1_rpart_roc_perf,  colorize = TRUE, print.cutoffs.at = c(.1, .5, .9))

stevens_test_m1_rpart_roc_perf2 <- performance(stevens_test_m1_rpart_roc_pred, measure = "tpr", x.measure = "cutoff")
plot(stevens_test_m1_rpart_roc_perf2)

# try using accuracy
stevens_test_m1_rpart_roc_perf3 <- performance(stevens_test_m1_rpart_roc_pred, measure = "acc", x.measure = "cutoff")
plot(stevens_test_m1_rpart_roc_perf3)

# get area under curve with ROCR
stevens_test_m1_rpart_roc_auc <- performance(stevens_test_m1_rpart_roc_pred, measure = "auc")
stevens_test_m1_rpart_roc_auc@y.values

# find optimal cutoff based on tpr and tnr
stevens_test_m1_rpart_roc_perf3 <- performance(stevens_test_m1_rpart_roc_pred, measure = "tpr", x.measure = "fpr")
class(stevens_test_m1_rpart_roc_perf3)
stevens_test_m1_rpart_roc_perf3

opt.cut = function(perf, pred){
        cut.ind = mapply(FUN=function(x, y, p){
                d = (x - 0)^2 + (y-1)^2
                ind = which(d == min(d))
                c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
                  cutoff = p[[ind]])
        }, perf@x.values, perf@y.values, pred@cutoffs)
        print(cut.ind)
}
opt.cut(stevens_test_m1_rpart_roc_perf3, stevens_test_m1_rpart_roc_pred)


#######################################


# build cart model with minbucket = 5
stevens_train_m1_rpart <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = stevens_train, 
                                method = "class", minbucket = 5)
names(stevens_train_m1_rpart)
stevens_train_m1_rpart
stevens_train_m1_rpart$splits
prp(stevens_train_m1_rpart)
fancyRpartPlot(stevens_train_m1_rpart)
stevens_test_m1_rpart_pred <- predict(stevens_train_m1_rpart, newdata = stevens_test, type = "class")
confusionMatrix(stevens_test_m1_rpart_pred, reference = stevens_test$Reverse)

# build cart model with minbucket = 100
stevens_train_m1_rpart <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = stevens_train, 
                                method = "class", minbucket = 100)
names(stevens_train_m1_rpart)
stevens_train_m1_rpart
stevens_train_m1_rpart$splits
prp(stevens_train_m1_rpart)
fancyRpartPlot(stevens_train_m1_rpart)

stevens_test_m1_rpart_pred <- predict(stevens_train_m1_rpart, newdata = stevens_test, type = "class")
confusionMatrix(stevens_test_m1_rpart_pred, reference = stevens_test$Reverse)


############################################3


# build random forest using randomForest
stevens_train_m2_rf <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                                    data = stevens_train, nodesize = 25, ntree = 200)

stevens_test_m2_rf_pred <- predict(stevens_train_m2_rf, newdata = stevens_test)
confusionMatrix(stevens_test_m2_rf_pred, reference = stevens_test$Reverse)

# build random forest using caret
stevens_train_m2_caret <- train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                                data = stevens_train, method = "rf", prox = TRUE, ntree = 200)

stevens_test_m2_caret_pred <- predict(stevens_train_m2_caret, newdata = stevens_test)
confusionMatrix(stevens_test_m2_caret_pred, reference = stevens_test$Reverse)


###################################################


# rebuild using randomForest with seed(100)
set.seed(200)
stevens_train_m2_rf <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                                    data = stevens_train, nodesize = 25, ntree = 200)

stevens_test_m2_rf_pred <- predict(stevens_train_m2_rf, newdata = stevens_test)
confusionMatrix(stevens_test_m2_rf_pred, reference = stevens_test$Reverse)


################################################


# use k-fold cross validation
number_folds <- trainControl(method = "cv", number = 10)
cp_grid <- expand.grid(.cp = seq(.01, .05, .01))

# without manual tuning across multiple complexity parameters (cp) and without 10-fold cv, 
# without k-fold, it bootstraps by default, 
# caret automatically tests different cp values on each bootstrap sample
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
      data = stevens_train, method = "rpart")

# without manual tuning across multiple complexity parameters (cp) but with 10-fold cv, 
# caret automatically tests different cp values
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
      data = stevens_train, method = "rpart", trControl = number_folds)

# with tuning across cp
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
      data = stevens_train, method = "rpart", trControl = number_folds, tuneGrid = cp_grid)

names(stevens_train_m3)
stevens_train_m3$xlevels

# then, we'll build a tree using the optimal cp value we got from k-fold cross validation, instead of minbucket parameter
# i got cp = .022, but lecture got 0.18
stevens_train_m3_rpart <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                               data = stevens_train, method = "class", cp = .18)
stevens_train_m3_rpart 
names(stevens_train_m3_rpart)
stevens_train_m3_rpart$variable.importance

# can get probabilities of each class
predict(stevens_train_m3_rpart, newdata = stevens_test)

# or can get predicted class
stevens_test_m3_rpart_pred <- predict(stevens_train_m3_rpart, newdata = stevens_test, type = "class")

confusionMatrix(stevens_test_m3_rpart_pred, reference = stevens_test$Reverse)

prp(stevens_train_m3_rpart)
fancyRpartPlot(stevens_train_m1_rpart)


#######################################################
######################################################
#####################################################3


# d2Hawkeye lecture

list.files()
claims <- read_csv("ClaimsData.csv")
glimpse(claims)

# get percent of patients in each cost bucket
claims %>% group_by(bucket2009) %>% summarize(pct_claimants = n() / nrow(.)) 
table(claims$bucket2009) / nrow(claims)

# split train and test set
set.seed(88)
in_train <- sample.split(claims$bucket2009, SplitRatio = .6)
claims_train <- subset(claims, in_train == TRUE)
claims_test <- subset(claims, in_train == FALSE)

dim(claims_train)
dim(claims_test)
dim(claims)
274803 + 183202 

# avg age of patients in training set
claims_train %>% summarize(avg_age = mean(age, na.rm = TRUE))

# proportion of patients with at least one diabetes diagnosis
claims_train %>% filter(diabetes > 0) %>% summarize(pct_diabetes = n() / nrow(claims_train))

# create baseline model, which is to just predict 2009 is same risk bucket as 2008
claims_test_baseline_cm <- confusionMatrix(claims_test$bucket2009, reference = claims_test$bucket2008)
names(claims_test_baseline_cm)
claims_test_baseline_cm$table

# create penalty matrix
penalty_matrix <- matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow = TRUE, nrow = 5)
penalty_matrix

# then multiply confusion matrix by penalty matrix to get penalized error count 
claims_test_basline_cm_penalized <- claims_test_baseline_cm$table * penalty_matrix
sum(claims_test_basline_cm_penalized) / nrow(claims_test)

# what would be test set accuracy and penalty error of alternative baseline 
# which predicts all patients as most frequent risk category 1
claims_test %>% group_by(bucket2009) %>% tally()
claims_test %>% group_by(bucket2009) %>% tally() %>% summarize(alt_baseline_accuracy = max(n) / sum(n))

claims_test_alt_baseline_pred <- rep(1, nrow(claims_test))
# since the penalty matrix they gave is setup so that predictions are columns and rows are outcomes
# and i don't want to have to recreate the penalty matrix, i'll use their table function
claims_test_alt_baseline_cm <- confusionMatrix(claims_test_alt_baseline_pred, reference = claims_test$bucket2009)
claims_test_alt_baseline_cm <- table(claims_test$bucket2009, claims_test_alt_baseline_pred)

claims_test_alt_baseline_cm_penalized <- claims_test_alt_baseline_cm * penalty_matrix
sum(claims_test_alt_baseline_cm_penalized) / nrow(claims_test)


# leaving this alone, because need to finish in time
# alt baseline penalty error correct answer is 1.044301


#############################################################3


# build claims cart model
claims_train_m1 <- rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure + 
                                 ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, 
                         data = claims_train, method = "class", cp = 0.00005)
names(claims_train_m1)
claims_train_m1

# prp(stevens_train_m1_rpart)
fancyRpartPlot(claims_train_m1)

# predict m1 on test
claims_test_m1_pred <- predict(claims_train_m1, newdata = claims_test, type = "class")
claims_test_m1_pred_cm <- table(claims_test$bucket2009, claims_test_m1_pred)

# get penalized error rate
claims_test_m1_pred_cm_penalized <- claims_test_m1_pred_cm * penalty_matrix
sum(claims_test_m1_pred_cm_penalized) / nrow(claims_test)

# note that unless told otherwise, the rpart function thinks weights all errors alike
# but we can pass it our penalty matrix using the loss option in the parameters argument
# we might get lower accuracy with new model w penalty weights, but should get lower penalty error rate
claims_train_m2 <- rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure + 
                                 ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, 
                         data = claims_train, method = "class", cp = 0.00005, 
                         parms = list(loss = penalty_matrix))

# predict m1 on test
claims_test_m2_pred <- predict(claims_train_m2, newdata = claims_test, type = "class")
claims_test_m2_pred_cm <- table(claims_test$bucket2009, claims_test_m2_pred)
(94310 + 18942 + 4692 + 636 + 2) / nrow(claims_test) 

# get penalized error rate
claims_test_m2_pred_cm_penalized <- claims_test_m2_pred_cm * penalty_matrix
sum(claims_test_m2_pred_cm_penalized) / nrow(claims_test)

# m2 with penalty predicted bucket 1 what percent of time?
claims_test_m2_pred_cm
(94310 + 7176 + 3590 + 1304 + 135) / nrow(claims_test)


######################################################
##################################################
#####################################################


# boston home prices

# read data
list.files()
boston <- read_csv("boston.csv")
glimpse(boston)

# plot census tracts
plot(boston$LON, boston$LAT)
points(boston$LON[boston$CHAS == 1], boston$LAT[boston$CHAS == 1], col = "blue", pch = 19)
points(boston$LON[boston$TRACT == 3531], boston$LAT[boston$TRACT == 3531], col = "red", pch = 19)

# view distribution of NOX
summary(boston$NOX)
# ggplot(boston, aes(x = NOX)) + geom_histogram()

points(boston$LON[boston$NOX >= .55], boston$LAT[boston$NOX >= .55], col = "green", pch = 19)

# reset plot and then add prices
plot(boston$LON, boston$LAT)
summary(boston$MEDV)

points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "red", pch = 19)
        
# plot prices against lat and lon
# both seem nonlinear relationship, so linear regression just using lat long will be ineffective
plot(boston$LON, boston$MEDV)
plot(boston$LAT, boston$MEDV)
lat_lon_lm <- lm(MEDV ~ LAT + LON, data = boston)
summary(lat_lon_lm)

# plot model
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "red", pch = 19)
names(lat_lon_lm)
lat_lon_lm$fitted.values
points(boston$LON[lat_lon_lm$fitted.values >= 21.2], boston$LAT[lat_lon_lm$fitted.values >= 21.2], col = "blue", pch = "$")

# try using cart trees
lat_lon_rpart <- rpart(MEDV ~ LAT + LON, data = boston)
names(lat_lon_rpart)
summary(lat_lon_rpart)
lat_lon_rpart

prp(lat_lon_rpart)

# plot rpart model
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "red", pch = 19)

lat_lon_rpart_pred <- predict(lat_lon_rpart, boston)
points(boston$LON[lat_lon_rpart_pred >= 21.2], boston$LAT[lat_lon_rpart_pred >= 21.2], col = "blue", pch = "$")

# since first tree might have been overfitting, try building new tree with larger minbucket size
lat_lon_rpart2 <- rpart(MEDV ~ LAT + LON, data = boston, minbucket = 50)
lat_lon_rpart2

# plot rpart2 model 3 different ways for fun
prp(lat_lon_rpart2)
fancyRpartPlot(lat_lon_rpart2)

plot(lat_lon_rpart2)
text(lat_lon_rpart2)

plot(boston$LON, boston$LAT)
abline(v = -71.07)
abline(h = 42.28)
abline(h = 42.17)
abline(h = 42.21)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "red", pch = 19)


#################################


# build full model with all predictors
set.seed(123)
in_train <- sample.split(boston$MEDV, SplitRatio = .7)
boston_train <- subset(boston, in_train == TRUE)
boston_test <- subset(boston, in_train == FALSE)

dim(boston_train)
dim(boston_test)
dim(boston)

# create linear model
boston_train_m1 <- lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, 
                      data = boston_train)
summary(boston_train_m1)

# get RMSE for m1
boston_test_m1_pred <- predict(boston_train_m1, newdata = boston_test)
m1_SSE <- sum((boston_test_m1_pred - boston_test$MEDV)^2)
m1_rmse <- sqrt(m1_SSE / nrow(boston_test))

# create rpart model
boston_train_m2 <- rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, 
                      data = boston_train)
boston_train_m2

prp(boston_train_m2)
fancyRpartPlot(boston_train_m2)

boston_test_m2_pred <- predict(boston_train_m2, newdata = boston_test)
m2_SSE <- sum((boston_test_m2_pred - boston_test$MEDV)^2)
m2_rmse <- sqrt(mean(m2_SSE))


###############################


# try building cart model with 10-fold cross validation
boston_train_m3_control <- trainControl(method = "cv", number = 10)
boston_train_m3_cp_grid <- expand.grid(.cp = (1:10) * .001)
# interesting that if i don't pass train function tunegrid, it selects cp = .06 with rmse = 6.09,
# but if i force it to try tuning at .01 level, it will select that cp value with rmse = 4.7
# so it doesn't seem to tune across a very broad range of values???
# boston_train_m3_cp_grid <- expand.grid(.cp = (1:10) * .01)

# just trying to see if regular data.frame works too, or if you need expand.grid
# update: it does seem to work fine with data.frame, not sure why they use the expand.grid???
boston_train_m3_cp_grid2 <- data.frame(.cp = seq(.001, .01, .001)) 


# tree without train_control or tunegrid
boston_train_m3 <- train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO,
                         data = boston_train, method = "rpart")
boston_train_m3

# tree with just trainControl
boston_train_m3 <- train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO,
                         data = boston_train, method = "rpart", trControl = boston_train_m3_control)
boston_train_m3

# tree with train_control and tunegrid
boston_train_m3 <- train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO,
                        data = boston_train, method = "rpart", trControl = boston_train_m3_control, 
                        tuneGrid = boston_train_m3_cp_grid)
boston_train_m3

# get final model
boston_train_m3_final <- boston_train_m3$finalModel

prp(boston_train_m3_final)

# evaluate m3 tree against test data
boston_test_m3_pred <- predict(boston_train_m3_final, newdata = boston_test)
m3_test_RSS <- sum((boston_test_m3_pred - boston_test$MEDV)^2) 
m3_test_rmse <- sqrt(mean(m3_test_RSS))
