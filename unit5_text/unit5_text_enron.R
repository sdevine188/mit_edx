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
emails <- read_csv("energy_bids.csv")
glimpse(emails)

emails$email[1]
emails$responsive[1]
table(emails$responsive)


#########################################


# create corpus
emails_corpus <- Corpus(VectorSource(emails$email))
emails_corpus
emails_corpus[[1]]$content


############################################


# clean corpus
emails_corpus2 <- emails_corpus

# to lower
emails_corpus2 <- tm_map(emails_corpus, tolower)
emails_corpus2[[1]]$content

# remove punctuation
emails_corpus2 <- tm_map(emails_corpus2, removePunctuation)
emails_corpus2[[1]]$content

# remove stopwords
stopwords("english")[1:10]
emails_corpus2 <- tm_map(emails_corpus2, removeWords, c("apple", stopwords("english")))
emails_corpus2[[1]]$content

# stem document
emails_corpus2 <- tm_map(emails_corpus2, stemDocument)
emails_corpus2[[1]]$content


#########################################


# create document term matrix
emails_dtm <- DocumentTermMatrix(emails_corpus2)
emails_dtm
inspect(emails_dtm)

# remove sparse terms - remove any term not appearing in at least 3% of documents
# reduced terms from 22k ish down to 619
emails_dtm <- removeSparseTerms(emails_dtm, .97)
emails_dtm

# convert dtm to data.frame
emails_df <- as.data.frame(as.matrix(emails_dtm))

# add "responsive" variable to be the dependent variable
emails_df$responsive <- emails$responsive


##############################################


# split data into training and test
set.seed(144)
in_train <- sample.split(emails_df$responsive, SplitRatio = .7)
emails_train <- subset(emails_df, in_train == TRUE)
emails_test <- subset(emails_df, in_train == FALSE)
dim(emails_train)
dim(emails_test)
dim(emails_df)


#############################################


# build m1 cart model
emails_train_m1_rpart <- rpart(responsive ~ ., data = emails_train, method = "class")
emails_train_m1_rpart
summary(emails_train_m1_rpart)
names(emails_train_m1_rpart)
prp(emails_train_m1_rpart)

emails_test_m1_rpart_pred <- predict(emails_train_m1_rpart, newdata = emails_test, type = "class")
confusionMatrix(emails_test_m1_rpart_pred, reference = emails_test$responsive)

# compute baseline accuracy
emails_test %>% group_by(responsive) %>% tally()
emails_test %>% filter(responsive == 0) %>% summarize(basline_accuracy = n() / nrow(emails_test))

# extract predicted probability of being responsive
emails_test_m1_rpart_pred_prob <- predict(emails_train_m1_rpart, newdata = emails_test)
emails_test_m1_rpart_pred_prob <- emails_test_m1_rpart_pred_prob[ , 2]
head(emails_test_m1_rpart_pred_prob)

# inspect using threshold of 0.5
table(emails_test_m1_rpart_pred_prob > .5, emails_test$responsive)
(204 + 15) / (204 + 15 + 26 + 10)


###############################################


# look at roc curve at inspect other cutoffs to penalize false negatives, since then responsive email goes unreviewed
emails_test_m1_rpart_roc_pred <- prediction(predictions = emails_test_m1_rpart_pred_prob, 
                                            labels = emails_test$responsive)
emails_test_m1_rpart_roc_perf <- performance(emails_test_m1_rpart_roc_pred, measure = "tpr", x.measure = "fpr")
plot(emails_test_m1_rpart_roc_perf, colorize = TRUE, print.cutoffs.at = c(.1, .5, .9))

emails_test_m1_rpart_roc_auc <- performance(emails_test_m1_rpart_roc_pred, measure = "auc")
emails_test_m1_rpart_roc_auc@y.values

















