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

# setwd
setwd("C:/Users/Stephen/Desktop/R/mit_edx/unit5_text")

# read in tweets data
list.files()
tweets <- read_csv("tweets.csv")
glimpse(tweets)

# create flag for negative tweets
tweets$negative <- as.factor(tweets$Avg <= -1)
table(tweets$negative)

# create corpus
tweets_corpus <- Corpus(VectorSource(tweets$Tweet))

# inspect corpus
tweets_corpus
names(tweets_corpus[[1]])

inspect(tweets_corpus[1])
tweets_corpus[[1]]$content

# clean corpus

# to lower
tweets_corpus2 <- tm_map(tweets_corpus, tolower)
tweets_corpus2[[1]]$content

# remove punctuation
tweets_corpus2 <- tm_map(tweets_corpus2, removePunctuation)
tweets_corpus2[[1]]$content

# remove stopwords
stopwords("english")[1:10]
tweets_corpus2 <- tm_map(tweets_corpus2, removeWords, c("apple", stopwords("english")))
tweets_corpus2[[1]]$content

# stem document
tweets_corpus2 <- tm_map(tweets_corpus2, stemDocument)
tweets_corpus2[[1]]$content


###################################################


# create document term matrix
tweets_dtm <- DocumentTermMatrix(tweets_corpus2)
tweets_dtm
names(tweets_dtm)
inspect(tweets_dtm)
inspect(tweets_dtm[1000:1005, 505:515])

# just for fun - get term frequency by col sums
tweets_dtm_col_sums <- as.matrix(slam::col_sums(tweets_dtm, na.rm=TRUE))
# convert to a dataframe
tweets_freq <- data.frame(term = rownames(tweets_dtm_col_sums), freq = tweets_dtm_col_sums[, 1])
rownames(tweets_freq) <- NULL
tweets_freq <- arrange(tweets_freq, desc(freq))
head(tweets_freq)

# find frequent terms
sort(findFreqTerms(tweets_dtm, lowfreq = 20))
tweets_freq %>% filter(freq >= 20) %>% arrange(term)

# remove sparse terms - only keep terms that are in at least .05% of tweets
tweets_dtm_sparse <- removeSparseTerms(tweets_dtm, .995)
tweets_dtm_sparse
tweets_dtm

# convert tweets_dtm_sparse to dataframe
tweets_df <- as.data.frame(as.matrix(tweets_dtm_sparse))

# run column names through make.names function in case any terms begin with numbers, etc or otherwise problematic names
colnames(tweets_df) <- make.names(colnames(tweets_df))

# add dependent variable tweets$negative
dim(tweets)
head(tweets$negative)
tweets_df$negative <- tweets$negative

# split train/test, with 70% in training
set.seed(123)
in_train <- sample.split(tweets_df$negative, SplitRatio = .7)
tweets_train <- subset(tweets_df, in_train == TRUE)
tweets_test <- subset(tweets_df, in_train == FALSE)
dim(tweets_train)
dim(tweets_test)
dim(tweets_df)

# which words appeared in corpus at least 100 times
findFreqTerms(tweets_dtm, lowfreq = 100)


#############################################


# build cart model m1
tweets_train_m1_rpart <- rpart(negative ~ ., data = tweets_train, method = "class")
tweets_train_m1_rpart
names(tweets_train_m1_rpart)        
prp(tweets_train_m1_rpart)        
        
# evaluate m1 performance
tweets_test_m1_rpart_pred <- predict(tweets_train_m1_rpart, newdata = tweets_test, type = "class")
head(tweets_test_m1_rpart_pred)
confusionMatrix(tweets_test_m1_rpart_pred, reference = tweets_test$negative)        

# compute baseline model
tweets_test %>% group_by(negative) %>% tally()
tweets_test %>% filter(negative == FALSE) %>% summarize(baseline_accuracy = n() / nrow(tweets_test))        
        
# build m2 randomforest
set.seed(123)
tweets_train_m2_rf <- randomForest(negative ~ ., data = tweets_train)
tweets_train_m2_rf
summary(tweets_train_m2_rf)
names(tweets_train_m2_rf)

tweets_test_m2_rf <- predict(tweets_train_m2_rf, newdata = tweets_test, type = "class")
confusionMatrix(tweets_test_m2_rf, reference = tweets_test$negative)


###############################################33


# try using logistic regression model
glimpse(tweets_train)
tweets_test_placeholder <- tweets_test
tweets_train_placeholder <- tweets_train
tweets_test <- tweets_test %>% select(-c(negative))
tweets_train <- tweets_train %>% select(-c(negative))

tweets_test$negative_dummy <- ifelse(tweets_test$negative == TRUE, 1, 0)
tweets_train$negative_dummy <- ifelse(tweets_train$negative == TRUE, 1, 0)
tweets_train %>% distinct(negative, negative_dummy)
tweets_test %>% distinct(negative, negative_dummy)

tweets_train_m3_logistic <- glm(negative_dummy ~ ., data = tweets_train, family = "binomial")
# tweets_train_m3_logistic <- glm(negative ~ . - negative_dummy, data = tweets_train, family = "binomial")
tweets_train_m3_logistic
summary(tweets_train_m3_logistic)

tweets_test_m3_logistic_pred <- predict(tweets_train_m3_logistic, newdata = tweets_test, type = "response")
summary(tweets_test_m3_logistic_pred)
tweets_test_m3_logistic_pred_class <- ifelse(tweets_test_m3_logistic_pred > .5, 1, 0)
table(tweets_test_m3_logistic_pred_class, tweets_test$negative_dummy)
(251 + 36) / (251 + 36 + 19 + 49)


