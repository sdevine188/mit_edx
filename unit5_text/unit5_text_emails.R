library(readr)
library(stringr)
library(dplyr)
library(tm)
library(SnowballC)
library(caret)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)

# setwd
setwd("C:/Users/Stephen/Desktop/R/mit_edx/unit5_text")

# read in tweets data
list.files()
emails <- read_csv("emails.csv")
glimpse(emails)

# general questions
dim(emails)
table(emails$spam)
head(emails$text)
emails %>% filter(nchar(text) == max(nchar(text))) %>% summarize(nchar_text = nchar(text))
emails %>% filter(nchar(text) == min(nchar(text)))
emails %>% filter(text == "Subject: fyi")
which(emails$text == "Subject: fyi")


#############################################


# create and clean corpus, then create sparse dtm

# create corpus
emails_corpus <- Corpus(VectorSource(emails$text))

# inspect corpus
emails_corpus
emails_corpus[[1]]$content

# clean corpus

# to lower
emails_corpus2 <- tm_map(emails_corpus, tolower)
emails_corpus2[[1]]$content

# remove punctuation
emails_corpus2 <- tm_map(emails_corpus2, removePunctuation)
emails_corpus2[[1]]$content

# remove stopwords
stopwords("english")[1:10]
emails_corpus2 <- tm_map(emails_corpus2, removeWords, stopwords("english"))
emails_corpus2[[1]]$content

# stem document
emails_corpus2 <- tm_map(emails_corpus2, stemDocument)
emails_corpus2[[1]]$content

# create document term matrix
emails_dtm <- DocumentTermMatrix(emails_corpus2)
emails_dtm

# remove sparse terms - only keep terms that are in at least 5% of tweets
emails_dtm <- removeSparseTerms(emails_dtm, .95)
emails_dtm

# build data.frame
emails_df <- as.data.frame(as.matrix(emails_dtm))
sort(colSums(emails_df), decreasing = TRUE)

# find most frequent word
emails_dtm_col_sums <- as.matrix(slam::col_sums(emails_dtm, na.rm = TRUE))
# convert to a dataframe
emails_freq <- data.frame(term = rownames(emails_dtm_col_sums), freq = emails_dtm_col_sums[, 1])
rownames(emails_freq) <- NULL
emails_freq <- arrange(emails_freq, desc(freq))
head(emails_freq)

# add spam variable as dependent variable
dim(emails)
dim(emails_df)
emails_df$spam <- emails$spam

###############################################


# how many terms appear at least 5000 times in the ham subset
emails_ham_col_sums <- emails_df %>% filter(spam == 0) %>% colSums(.) %>% data.frame(.)
names(emails_ham_col_sums) <- "freq"
emails_ham_freq <- data.frame(term = rownames(emails_col_sums), freq = emails_ham_col_sums$freq)
rownames(emails_ham_freq) <- NULL
dim(emails_ham_freq)
dim(emails_df)
emails_ham_freq %>% filter(freq >= 5000) %>% tally()

# how many terms appear at least 1000 times in the spam subset
emails_spam_col_sums <- emails_df %>% filter(spam == 1) %>% colSums(.) %>% data.frame(.)
names(emails_spam_col_sums) <- "freq"
emails_spam_freq <- data.frame(term = rownames(emails_spam_col_sums), freq = emails_spam_col_sums$freq)
rownames(emails_spam_freq) <- NULL
dim(emails_spam_freq)
dim(emails_df)
emails_spam_freq <- emails_spam_freq %>% filter(term != "spam")
emails_spam_freq %>% filter(freq >= 1000) %>% tally()


######################################################


# create trainin/test

# make spam a factor
emails_df$spam <- as.factor(emails_df$spam)

# make names non-problematic
names(emails_df) <- make.names(names(emails_df))

# split train/test
set.seed(123)
in_train <- sample.split(emails_df$spam, SplitRatio = .7)
emails_train <- subset(emails_df, in_train == TRUE)
emails_test <- subset(emails_df, in_train == FALSE)
dim(emails_train)
dim(emails_test)
dim(emails_df)
4010 + 1718


########################################


# build model 1 - logistic 
emails_train_m1_logistic <- glm(spam ~ ., data = emails_train, family = "binomial")
emails_train_m1_logistic_fitted_prob <- predict(emails_train_m1_logistic, newdata = emails_train, type = "response")

summary(emails_train_m1_logistic_fitted_prob)
length(emails_train_m1_logistic_fitted_prob[emails_train_m1_logistic_fitted_prob < .00001])
length(emails_train_m1_logistic_fitted_prob[emails_train_m1_logistic_fitted_prob > .99999])
length(emails_train_m1_logistic_fitted_prob[emails_train_m1_logistic_fitted_prob > .00001 && 
                                                    emails_train_m1_logistic_fitted_prob < .99999])
summary(emails_train_m1_logistic)
names(emails_train_m1_logistic)
head(emails_train_m1_logistic$coefficients)
emails_train_m1_logistic_pvalues <- summary(emails_train_m1_logistic)$coefficients[ , 4]
emails_train_m1_logistic_variable <- summary(emails_train_m1_logistic)$coefficients[ , 1]
emails_train_m1_logistic_pvalues <- data.frame(emails_train_m1_logistic_pvalues)
head(emails_train_m1_logistic_pvalues)
names(emails_train_m1_logistic_pvalues) <- "pvalue"
emails_train_m1_logistic_pvalues %>% filter(pvalue <= .005) %>% tally()

# predict m1 training accuracy
emails_train_m1_logistic_fitted_class <- ifelse(emails_train_m1_logistic_fitted_prob > .5, 1, 0)
confusionMatrix(emails_train_m1_logistic_fitted_class, emails_train$spam)

# training set AUC for m1
emails_train_m1_logistic_roc_pred <- prediction(emails_train_m1_logistic_fitted_prob, labels = emails_train$spam)
emails_train_m1_logistic_roc_auc <- performance(emails_train_m1_logistic_roc_pred, measure = "auc")
emails_train_m1_logistic_roc_auc@y.values

# test set accuracy for m1
emails_test_m1_logistic_pred <- predict(emails_train_m1_logistic, newdata = emails_test, type = "response")
emails_test_m1_logistic_pred_class <- ifelse(emails_test_m1_logistic_pred > .5, 1, 0)
confusionMatrix(emails_test_m1_logistic_pred_class, reference = emails_test$spam)

# testing set AUC for m1
emails_test_m1_logistic_roc_pred <- prediction(emails_test_m1_logistic_pred, labels = emails_test$spam)
emails_test_m1_logistic_roc_auc <- performance(emails_test_m1_logistic_roc_pred, measure = "auc")
emails_test_m1_logistic_roc_auc@y.values


############################################################


# build model 2 - rpart
emails_train_m2_rpart <- rpart(spam ~ ., data = emails_train, method = "class")
emails_train_m2_rpart
prp(emails_train_m2_rpart)

# training accuracy for m2
emails_train_m2_rpart_fitted_class <- predict(emails_train_m2_rpart, newdata = emails_train, type = "class")
confusionMatrix(emails_train_m2_rpart_fitted_class, reference = emails_train$spam)

# training set auc
emails_train_m2_rpart_fitted_prob <- predict(emails_train_m2_rpart, newdata = emails_train)
emails_train_m2_rpart_fitted_prob <- emails_train_m2_rpart_fitted_prob[ , 2]
emails_train_m2_rpart_roc_pred <- prediction(emails_train_m2_rpart_fitted_prob, labels = emails_train$spam)
emails_train_m2_rpart_roc_auc <- performance(emails_train_m2_rpart_roc_pred, measure = "auc")
emails_train_m2_rpart_roc_auc@y.values

# accuracy on testing set
emails_test_m2_rpart_pred_prob <- predict(emails_train_m2_rpart, newdata = emails_test)
emails_test_m2_rpart_pred_prob <- emails_test_m2_rpart_pred_prob[ , 2]
emails_test_m2_rpart_pred_class <- ifelse(emails_test_m2_rpart_pred_prob > .5, 1, 0)
confusionMatrix(emails_test_m2_rpart_pred_class, reference = emails_test$spam)

# testing set auc for m2
emails_test_m2_rpart_roc_pred <- prediction(emails_test_m2_rpart_pred_prob, labels = emails_test$spam)
emails_test_m2_rpart_roc_auc <- performance(emails_test_m2_rpart_roc_pred, measure = "auc")
emails_test_m2_rpart_roc_auc@y.values


###################################################


# build model 3 - rf
set.seed(123)
emails_train_m4_rf <- randomForest(spam ~ ., data = emails_train)
emails_train_m4_rf 

# training set accuracy of rf
emails_train_m4_rf_fitted_class <- predict(emails_train_m4_rf, newdata = emails_train, type = "class")
confusionMatrix(emails_train_m4_rf_fitted_class, reference = emails_train$spam)

# training set auc of rf
emails_train_m4_rf_fitted_prob <- predict(emails_train_m4_rf, newdata = emails_train, type = "prob")
emails_train_m4_rf_fitted_prob <- emails_train_m4_rf_fitted_prob[ , 2]
emails_train_m4_rf_roc_pred <- prediction(emails_train_m4_rf_fitted_prob, labels = emails_train$spam)
emails_train_m4_rf_roc_auc <- performance(emails_train_m4_rf_roc_pred, measure = "auc")
emails_train_m4_rf_roc_auc@y.values

# test set accuracy
emails_test_m4_rf_pred_prob <- predict(emails_train_m4_rf, newdata = emails_test, type = "prob")
emails_test_m4_rf_pred_prob <- emails_test_m4_rf_pred_prob[ , 2]
emails_test_m4_rf_pred_class <- ifelse(emails_test_m4_rf_pred_prob > .5, 1, 0)
confusionMatrix(emails_test_m4_rf_pred_class, reference = emails_test$spam)

# testing set auc
emails_test_m4_rf_roc_pred <- prediction(emails_test_m4_rf_pred_prob, labels = emails_test$spam)
emails_test_m4_rf_roc_auc <- performance(emails_test_m4_rf_roc_pred, measure = "auc")
emails_test_m4_rf_roc_auc@y.values






