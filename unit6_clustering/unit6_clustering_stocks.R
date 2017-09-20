library(stringr)
library(readr)
library(dplyr)
library(GGally)
library(corrplot)
library(caTools)
library(caret)
library(flexclust)

# setwd
setwd("C:/Users/Stephen/Desktop/R/mit_edx/unit6_clustering")

# load data
list.files()
stocks <- read_csv("StocksCluster.csv")
glimpse(stocks)

# random questions
glimpse(stocks)
stocks %>% filter(PositiveDec == 1) %>% summarize(pct_positive = nrow(.) / nrow(stocks))

# largest correlation
correlation_matrix <- data.frame(cor(stocks))
str(correlation_matrix)
months <- row.names(correlation_matrix)
correlation_matrix$month <- months

second_largest <- function(vector) { 
        sort(vector, decreasing = TRUE)[2]
        }
x <- c(1, 2, 3, 4, 5)
second_largest(x)

correlation_matrix %>% select(-month) %>% summarize_all(funs(second_largest))

correlation_matrix <- cor(stocks)
corrplot(correlation_matrix)
ggpairs(stocks)
sapply(correlation_matrix, function(x) { max(x) })

# largest avg return
stocks %>% summarize_all(funs(mean)) %>% select(-PositiveDec) %>% max(.[1, ])
stocks %>% summarize_all(funs(mean)) %>% select(-PositiveDec) %>% min(.[1, ])


############################################


# build logistic model
set.seed(144)
in_train <- sample.split(stocks$PositiveDec, SplitRatio = .7)
stocks_train <- subset(stocks, in_train == TRUE)
stocks_test <- subset(stocks, in_train == FALSE)

stocks_train_m1_logistic <- glm(PositiveDec ~ ., data = stocks_train, family = "binomial")
stocks_train_m1_logistic
names(stocks_train_m1_logistic)

# train set accuracy
stocks_train_m1_logistic_pred <- stocks_train_m1_logistic$fitted.values
stocks_train_m1_logistic_pred_class <- ifelse(stocks_train_m1_logistic_pred > .5, 1, 0)
table(stocks_train_m1_logistic_pred_class, stocks_train$PositiveDec)
(990 + 3640) / (990 + 3640 + 787 + 2689)

# test set accuracy
stocks_test_m1_logistic_pred <- predict(stocks_train_m1_logistic, newdata = stocks_test, type = "response")
stocks_test_m1_logistic_pred_class <- ifelse(stocks_test_m1_logistic_pred > .5, 1, 0)
table(stocks_test_m1_logistic_pred_class, stocks_test$PositiveDec)
(417 + 1553) / (417 + 1553 + 344 + 1160)

# baseline model accuracy on test set
stocks_test %>% group_by(PositiveDec) %>% tally() %>% summarize(baseline_accuracy = max(n) / nrow(stocks_test))


##########################################


# build cluster model

# remove dependent variable
limited_stocks_train <- stocks_train
limited_stocks_train$PositiveDec <- NULL

limited_stocks_test <- stocks_test
limited_stocks_test$PositiveDec <- NULL

# preprocess to normalize data
glimpse(limited_stocks_train)
limited_stocks_train_preprocess <- preProcess(limited_stocks_train)
str(limited_stocks_train_preprocess)
limited_stocks_train_norm <- predict(limited_stocks_train_preprocess, limited_stocks_train)
glimpse(limited_stocks_train_norm)
mean(limited_stocks_train_norm$ReturnJan)

# note you have to use preprocess normalization from train set on the test set
limited_stocks_test_norm <- predict(limited_stocks_train_preprocess, limited_stocks_test)
glimpse(limited_stocks_test_norm)
mean(limited_stocks_test_norm$ReturnJan)

# run kmeans clustering with 3 clusters
k <- 3
set.seed(144)
limited_stocks_train_clusters <- kmeans(limited_stocks_train_norm, centers = k)
names(limited_stocks_train_clusters)
limited_stocks_train_clusters$centers
limited_stocks_train_clusters$cluster
head(limited_stocks_train_clusters$cluster, 10)

# add cluster_groups to train data
limited_stocks_train$cluster_groups <- limited_stocks_train_clusters$cluster
limited_stocks_train %>% group_by(cluster_groups) %>% tally()

# use flexclust to build a model of the train kmeans cluster model, which can then be used to predict test set clusters
train_kcca <- as.kcca(limited_stocks_train_clusters, limited_stocks_train_norm)
str(train_kcca)

# note that predicting train_kcca is same as kmeans cluster output above - different ways to skin cat
train_kcca_clusters <- predict(train_kcca) 
head(train_kcca_clusters, 10)
train_kcca_clusters <- predict(train_kcca, newdata = limited_stocks_train_norm) 
head(train_kcca_clusters, 10)

# now use kcca model to predict on test set
test_kcca_clusters <- predict(train_kcca, newdata = limited_stocks_test_norm)
test_kcca_clusters
limited_stocks_test$kcca_cluster_groups <- test_kcca_clusters

limited_stocks_test %>% group_by(kcca_cluster_groups) %>% tally()

# create subsets based on three clusters in train and test data
stocks_train$kcca_cluster_groups <- train_kcca_clusters
stocks_train1 <- stocks_train %>% filter(kcca_cluster_groups == 1)
stocks_train2 <- stocks_train %>% filter(kcca_cluster_groups == 2)
stocks_train3 <- stocks_train %>% filter(kcca_cluster_groups == 3)

stocks_test$kcca_cluster_groups <- test_kcca_clusters
stocks_test1 <- stocks_test %>% filter(kcca_cluster_groups == 1)
stocks_test2 <- stocks_test %>% filter(kcca_cluster_groups == 2)
stocks_test3 <- stocks_test %>% filter(kcca_cluster_groups == 3)

# random questions
mean(stocks_train1$PositiveDec)
mean(stocks_train2$PositiveDec)
mean(stocks_train3$PositiveDec)


#################################3


# build logisitic reg models for seperate training sets based on clusters
stocks_train1_m1_logistic <- glm(PositiveDec ~ ., data = stocks_train1, family = "binomial")
summary(stocks_train1_m1_logistic)

stocks_train2_m1_logistic <- glm(PositiveDec ~ ., data = stocks_train2, family = "binomial")
summary(stocks_train2_m1_logistic)

stocks_train3_m1_logistic <- glm(PositiveDec ~ ., data = stocks_train3, family = "binomial")
summary(stocks_train3_m1_logistic)

# predict on test sets
stocks_test1_m1_logistic_pred <- predict(stocks_train1_m1_logistic, newdata = stocks_test1, type = "response")
head(stocks_test1_m1_logistic_pred)
stocks_test1_m1_logistic_class <- ifelse(stocks_test1_m1_logistic_pred > .5, 1, 0)
stocks_test1$pred <- stocks_test1_m1_logistic_class 
confusionMatrix(stocks_test1$pred, reference = stocks_test1$PositiveDec)

stocks_test2_m1_logistic_pred <- predict(stocks_train2_m1_logistic, newdata = stocks_test2, type = "response")
stocks_test2_m1_logistic_class <- ifelse(stocks_test2_m1_logistic_pred > .5, 1, 0)
stocks_test2$pred <- stocks_test2_m1_logistic_class 
confusionMatrix(stocks_test2$pred, reference = stocks_test2$PositiveDec)

stocks_test3_m1_logistic_pred <- predict(stocks_train3_m1_logistic, newdata = stocks_test3, type = "response")
stocks_test3_m1_logistic_class <- ifelse(stocks_test3_m1_logistic_pred > .5, 1, 0)
stocks_test3$pred <- stocks_test3_m1_logistic_class 
confusionMatrix(stocks_test3$pred, reference = stocks_test3$PositiveDec)

# to then get overall predictive accuracy of cluster-then-predict method, combine all 3 sets of predictions and outcomes
all_pred <- c(stocks_test1$pred, stocks_test2$pred, stocks_test3$pred)
all_outcome <- c(stocks_test1$PositiveDec, stocks_test2$PositiveDec, stocks_test3$PositiveDec)
confusionMatrix(all_pred, reference = all_outcome)








