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
census <- read_csv("census.csv")
glimpse(census)

# split train and test
unique(census$over50k)
# census$over50k <- factor(census$over50k)
census$over50_dummy <- ifelse(census$over50k == "<=50K", 0, 1)
unique(census$over50_dummy)

set.seed(2000)
# in_train <- sample.split(census$over50_dummy, SplitRatio = .6)
in_train <- sample.split(census$over50k, SplitRatio = .6)
census_train <- subset(census, in_train == TRUE)
census_test <- subset(census, in_train == FALSE)

# build m1 logistic
census_m1_logistic <- glm(over50_dummy ~ ., data = census_train, family = "binomial")
summary(census_m1_logistic)



set.seed(2000)
spl = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, spl==TRUE)
test = subset(census, spl==FALSE)
censusglm = glm( over50k ~ . , family="binomial", data = train)
summary(censusglm)


####################################


# baseline accuracy
census_test %>% group_by(over50k) %>% tally()
census_test %>% filter(over50k == "<=50K") %>% summarize(baseline_accuracy = n() / nrow(census_test))


####################################


# build rpart
census_m2_rpart <- rpart(over50k ~ ., data = census_train, method = "class")
census_m2_rpart
summary(census_m2_rpart)
prp(census_m2_rpart)

census_m2_rpart_pred <- predict(census_m2_rpart, newdata = census_test, type = "class")
confusionMatrix(census_m2_rpart_pred, reference = census_test$over50k)

census_m2_rpart_roc_pred <- prediction(census_m2_rpart_pred, census_test$over50k)
census_m2_rpart_auc <- performance(census_m2_rpart_roc_pred, measure = "auc")
census_m2_rpart_auc@y.values


###############################


# skipped to problem 3 - build rf model
set.seed(2000)
in_train = sample.split(census$over50k, SplitRatio = 0.6)
census_train = subset(census, in_train == TRUE)
census_test = subset(census, in_train == FALSE)

set.seed(1)
census_train_small <- census_train[sample(nrow(census_train), 2000), ]
# census_train_small$over50k <- factor(census_train_small$over50k)
glimpse(census_train_small)

set.seed(1)
census_train_small_m3_rf <- randomForest(over50k ~ ., data = census_train_small)
census_train_small_m3_rf
summary(census_train_small_m3_rf)
names(census_train_small_m3_rf)
census_train_small_m3_rf$importance

# evaluate m3 on test set
census_test_m3_rf_pred <- predict(census_train_small_m3_rf, newdata = census_test)
confusionMatrix(census_test_m3_rf_pred, reference = census_test$over50k)

# inspect variable importance by how frequently variable was selected for split
vu <- varUsed(census_train_small_m3_rf, count = TRUE)
names(vu)
vusorted <- sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(census_train_small_m3_rf$forest$xlevels[vusorted$ix]))

# also could use importance attribute of model to get mean gini decrease
m3_var_imp <- data.frame(variable = rownames(census_train_small_m3_rf$importance), 
                         mean_gini_decrease = census_train_small_m3_rf$importance)
rownames(m3_var_imp) <- NULL
m3_var_imp <- m3_var_imp %>% rename(mean_gini_decrease = MeanDecreaseGini) %>%
        arrange(desc(mean_gini_decrease))
m3_var_imp

# get plot of mean gini decrease
varImp(census_train_small_m3_rf)
varImpPlot(census_train_small_m3_rf)


# use 10-fold cv to tune complexity parameter
cp_grid <- expand.grid(.cp = seq(0.002,0.1,0.002))
census_train_m4_rpart_train_control <- trainControl(method = "cv", number = 10)
census_train_m4_rpart <- train(over50k ~ ., data = census_train, method = "rpart", 
                               trControl = census_train_m4_rpart_train_control, 
                               tuneGrid = cp_grid)

census_train_m4_rpart 
summary(census_train_m4_rpart)
names(census_train_m4_rpart)
census_train_m4_rpart$finalModel
prp(census_train_m4_rpart$finalModel)


# train model with the optimal cp value
census_train_m4_rpart_final <- rpart(over50k ~ ., data = census_train, method = "class", cp = .002) 
census_train_m4_rpart_final

census_test_m4_rpart_pred <- predict(census_train_m4_rpart_final, newdata = census_test, type = "class")
confusionMatrix(census_test_m4_rpart_pred, reference = census_test$over50k)

prp(census_train_m4_rpart_final)

