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
clinical <- read_csv("clinical_trial.csv")
glimpse(clinical)

# assorted questions
clinical %>% filter(!is.na(abstract)) %>% summarize(max_nchar = max(nchar(abstract)))
clinical %>% filter(is.na(abstract)) %>% tally()
clinical %>% filter(nchar(title) == min(nchar(title))) %>% data.frame(.)


##########################################


# create and clean seperate corpus, then create sparse dtm, for title variable

# create corpus
clinical_title_corpus <- Corpus(VectorSource(clinical$title))

# inspect corpus
clinical_title_corpus
clinical_title_corpus[[1]]$content

# clean corpus

# to lower
clinical_title_corpus2 <- tm_map(clinical_title_corpus, tolower)
clinical_title_corpus2[[1]]$content

# remove punctuation
clinical_title_corpus2 <- tm_map(clinical_title_corpus2, removePunctuation)
clinical_title_corpus2[[1]]$content

# remove stopwords
stopwords("english")[1:10]
clinical_title_corpus2 <- tm_map(clinical_title_corpus2, removeWords, stopwords("english"))
clinical_title_corpus2[[1]]$content

# stem document
clinical_title_corpus2 <- tm_map(clinical_title_corpus2, stemDocument)
clinical_title_corpus2[[1]]$content

# create document term matrix
clinical_title_dtm <- DocumentTermMatrix(clinical_title_corpus2)
clinical_title_dtm

# remove sparse terms - only keep terms that are in at least 5% of tweets
clinical_title_dtm <- removeSparseTerms(clinical_title_dtm, .95)
clinical_title_dtm


################################################


# create and clean seperate corpus, then create sparse dtm, for abstract variable

# create corpus
clinical_abstract_corpus <- Corpus(VectorSource(clinical$abstract))

# inspect corpus
clinical_abstract_corpus
clinical_abstract_corpus[[2]]$content

# clean corpus

# to lower
clinical_abstract_corpus2 <- tm_map(clinical_abstract_corpus, tolower)
clinical_abstract_corpus2[[2]]$content

# remove punctuation
clinical_abstract_corpus2 <- tm_map(clinical_abstract_corpus2, removePunctuation)
clinical_abstract_corpus2[[2]]$content

# remove stopwords
stopwords("english")[1:10]
clinical_abstract_corpus2 <- tm_map(clinical_abstract_corpus2, removeWords, stopwords("english"))
clinical_abstract_corpus2[[2]]$content

# stem document
clinical_abstract_corpus2 <- tm_map(clinical_abstract_corpus2, stemDocument)
clinical_abstract_corpus2[[2]]$content

# create document term matrix
clinical_abstract_dtm <- DocumentTermMatrix(clinical_abstract_corpus2)
clinical_abstract_dtm

# remove sparse terms - only keep terms that are in at least 5% of tweets
clinical_abstract_dtm <- removeSparseTerms(clinical_abstract_dtm, .95)
clinical_abstract_dtm


#################################################


# find most frequent word in abstracts
# get term frequency by col sums
clinical_abstract_dtm_col_sums <- as.matrix(slam::col_sums(clinical_abstract_dtm, na.rm = TRUE))
# convert to a dataframe
clinical_abstract_freq <- data.frame(term = rownames(clinical_abstract_dtm_col_sums), freq = clinical_abstract_dtm_col_sums[, 1])
rownames(clinical_abstract_freq) <- NULL
clinical_abstract_freq <- arrange(clinical_abstract_freq, desc(freq))
head(clinical_abstract_freq)


######################################################


# combine abstract and tilte dtm into one dataframe
clinical_title_df <- as.data.frame(as.matrix(clinical_title_dtm))
names(clinical_title_df)
names(clinical_title_df) <- str_c("T_", names(clinical_title_df))

clinical_abstract_df <- as.data.frame(as.matrix(clinical_abstract_dtm))
names(clinical_abstract_df)
names(clinical_abstract_df) <- str_c("A_", names(clinical_abstract_df))

clinical_df <- cbind(clinical_title_df, clinical_abstract_df)
dim(clinical_df)
dim(clinical)
clinical_df$trial <- clinical$trial
dim(clinical_df)


############################################


# split into training and test
set.seed(144)
in_train <- sample.split(clinical_df$trial, SplitRatio = .7)
clinical_train <- subset(clinical_df, in_train == TRUE)
clinical_test <- subset(clinical_df, in_train == FALSE)
dim(clinical_train)
dim(clinical_test)
dim(clinical_df)
558+1302


############################################


# build model 1

# compute baseline accuracy on testing set
clinical_test %>% group_by(trial) %>% tally()
clinical_test %>% filter(trial == 0) %>% summarize(baseline_accuracy = n() / nrow(clinical_test))

# build m1 rpart
clinical_train_m1_rpart <- rpart(trial ~ ., data = clinical_train, method = "class")
clinical_train_m1_rpart
prp(clinical_train_m1_rpart)

# extract training set predictions
names(clinical_train_m1_rpart)
clinical_train_m1_rpart_fitted_prob <- predict(clinical_train_m1_rpart, newdata = clinical_train)
head(clinical_train_m1_rpart_fitted_prob)
clinical_train_m1_rpart_fitted_prob <- clinical_train_m1_rpart_fitted_prob[ , 2]
max(clinical_train_m1_rpart_fitted_prob)

# what is training set accuracy
# clinical_train_m1_rpart_fitted_class <- predict(clinical_train_m1_rpart, newdata = clinical_train, type = "class")
clinical_train_m1_rpart_fitted_class <- ifelse(clinical_train_m1_rpart_fitted_prob > .5, 1, 0)
confusionMatrix(clinical_train_m1_rpart_fitted_class, reference = clinical_train$trial)

# evaluate m1
clinical_test_m1_rpart_pred <- predict(clinical_train_m1_rpart, newdata = clinical_test, type = "class")
confusionMatrix(clinical_test_m1_rpart_pred, reference = clinical_test$trial)

# get area under curve with ROCR
clinical_test_m1_rpart_prob <- predict(clinical_train_m1_rpart, newdata = clinical_test)
clinical_test_m1_rpart_prob <- clinical_test_m1_rpart_prob[ , 2]

clinical_test_m1_rpart_roc_pred <- prediction(clinical_test_m1_rpart_prob, labels = clinical_test$trial)
clinical_test_m1_rpart_roc_auc <- performance(clinical_test_m1_rpart_roc_pred, measure = "auc")
clinical_test_m1_rpart_roc_auc@y.values


