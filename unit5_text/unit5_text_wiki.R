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
wiki <- read_csv("wiki.csv")
glimpse(wiki)

# convert Vandal to factor
wiki$Vandal <- as.factor(wiki$Vandal)

wiki %>% group_by(Vandal) %>% tally()


#################################33


# create and clean corpus of Added words 
# note the words are already lowercase and removed punctuation
wiki_added_corpus <- Corpus(VectorSource(wiki$Added))
wiki_added_corpus
wiki_added_corpus[[1]]$content

# remove stopwords
length(stopwords("english")) 
stopwords("english")[1:10]
wiki_added_corpus2 <- tm_map(wiki_added_corpus, removeWords, stopwords("english"))
wiki_added_corpus2[[1]]$content

# stem document
wiki_added_corpus2 <- tm_map(wiki_added_corpus2, stemDocument)
wiki_added_corpus2[[1]]$content


#######################################

# create document term matrix and data frame

# create document term matrix for Added words
wiki_added_dtm <- DocumentTermMatrix(wiki_added_corpus2)
wiki_added_dtm
summary(wiki_added_dtm)

# remove sparse terms appearing in less than 0.3% of documents
wiki_added_dtm <- removeSparseTerms(wiki_added_dtm, sparse = .997)
wiki_added_dtm

# create dataframe
wiki_added_df <- as.data.frame(as.matrix(wiki_added_dtm))
dim(wiki_added_df)
names(wiki_added_df)

# add letter "A" to font of all term column names
names(wiki_added_df) <- str_c("A", names(wiki_added_df))
names(wiki_added_df)


###################################################


# create and clean corpus of removed words 
# note the words are already lowercase and removed punctuation
wiki_removed_corpus <- Corpus(VectorSource(wiki$Removed))
wiki_removed_corpus
wiki_removed_corpus[[2]]$content

# remove stopwords
length(stopwords("english")) 
stopwords("english")[1:10]
wiki_removed_corpus2 <- tm_map(wiki_removed_corpus, removeWords, stopwords("english"))
wiki_removed_corpus2[[2]]$content

# stem document
wiki_removed_corpus2 <- tm_map(wiki_removed_corpus2, stemDocument)
wiki_removed_corpus2[[2]]$content


#######################################

# create document term matrix and data frame

# create document term matrix for removed words
wiki_removed_dtm <- DocumentTermMatrix(wiki_removed_corpus2)
wiki_removed_dtm

# remove sparse terms appearing in less than 0.3% of documents
wiki_removed_dtm <- removeSparseTerms(wiki_removed_dtm, sparse = .997)
wiki_removed_dtm

# create dataframe
wiki_removed_df <- as.data.frame(as.matrix(wiki_removed_dtm))
dim(wiki_removed_df)
names(wiki_removed_df)

# add letter "A" to font of all term column names
names(wiki_removed_df) <- str_c("R", names(wiki_removed_df))
names(wiki_removed_df)


####################################


# prepare final dataframe

# combine added and removed into wiki_df
wiki_df <- cbind(wiki_added_df, wiki_removed_df)
dim(wiki_df)

# add Vandal as dependent variable
wiki_df$vandal <- wiki$Vandal


########################################


# prepare training/test set
set.seed(123)
in_train <- sample.split(wiki_df$vandal, SplitRatio = .7)
wiki_train <- subset(wiki_df, in_train == TRUE)
wiki_test <- subset(wiki_df, in_train == FALSE)
dim(wiki_train)
dim(wiki_test)
dim(wiki_df)
2713+1163


##########################################


# build and test m1 cart model

# compute baseline model accuracy on test set
wiki_test %>% group_by(vandal) %>% tally()
wiki_test %>% filter(vandal == 0) %>% summarize(baseline_accuracy = n() / nrow(wiki_test))

# build m1 cart model
wiki_train_m1_rpart <- rpart(vandal ~ ., data = wiki_train, method = "class")
wiki_train_m1_rpart
summary(wiki_train_m1_rpart)
prp(wiki_train_m1_rpart)

# test m1
wiki_test_m1_rpart_pred <- predict(wiki_train_m1_rpart, newdata = wiki_test, type = "class")
confusionMatrix(wiki_test_m1_rpart_pred, reference = wiki_test$vandal)


####################################################


# create model 2

# add http variable dummy if revisions include website url
names(wiki)
dim(wiki)
wiki_df2 <- wiki_df
dim(wiki_df2)
wiki_df2$http <- ifelse(grepl("http", wiki$Added, fixed = TRUE), 1, 0)
wiki_df2 %>% group_by(http) %>% tally()

# re-create new training/test sets using old split
wiki_train2 <- subset(wiki_df2, in_train == TRUE)
wiki_test2 <- subset(wiki_df2, in_train == FALSE)
dim(wiki_train2)
dim(wiki_test2)
dim(wiki_df2)
2713+1163

# create model 2 
wiki_train_m2_rpart <- rpart(vandal ~ ., data = wiki_train2, method = "class")
wiki_train_m2_rpart
prp(wiki_train_m2_rpart)

# test m2 
wiki_test_m2_rpart_pred <- predict(wiki_train_m2_rpart, newdata = wiki_test2, type = "class")
confusionMatrix(wiki_test_m2_rpart_pred, reference = wiki_test2$vandal)


######################################


# add another feature of number of words added/removed
# need to recreate the dtm, because i overwrote the original dtm with the sparse dtm

# create document term matrix for removed words
wiki_removed_dtm <- DocumentTermMatrix(wiki_removed_corpus2)
wiki_removed_dtm

# create document term matrix for added words
wiki_added_dtm <- DocumentTermMatrix(wiki_added_corpus2)
wiki_added_dtm

wiki_df2$number_added <- rowSums(as.matrix(wiki_added_dtm))
wiki_df2$number_removed <- rowSums(as.matrix(wiki_removed_dtm))
summary(wiki_df2$number_added)

# re-create new training/test sets using old split
wiki_train2 <- subset(wiki_df2, in_train == TRUE)
wiki_test2 <- subset(wiki_df2, in_train == FALSE)
dim(wiki_train2)
dim(wiki_test2)
dim(wiki_df2)
2713+1163

# create model 3 
wiki_train_m3_rpart <- rpart(vandal ~ ., data = wiki_train2, method = "class")
wiki_train_m3_rpart
prp(wiki_train_m3_rpart)

# test m3
wiki_test_m3_rpart_pred <- predict(wiki_train_m3_rpart, newdata = wiki_test2, type = "class")
confusionMatrix(wiki_test_m3_rpart_pred, reference = wiki_test2$vandal)


######################################################


# add additional variables and create model 4
wiki_df3 <- wiki_df2

wiki_df3$minor <- wiki$Minor
wiki_df3$logged_in <- wiki$Loggedin

# re-create new training/test sets using old split
wiki_train3 <- subset(wiki_df3, in_train == TRUE)
wiki_test3 <- subset(wiki_df3, in_train == FALSE)
dim(wiki_train3)
dim(wiki_test3)
dim(wiki_df3)
2713+1163

# create model 4
wiki_train_m4_rpart <- rpart(vandal ~ ., data = wiki_train3, method = "class")
wiki_train_m4_rpart
prp(wiki_train_m4_rpart)

# test m4
wiki_test_m4_rpart_pred <- predict(wiki_train_m4_rpart, newdata = wiki_test3, type = "class")
confusionMatrix(wiki_test_m4_rpart_pred, reference = wiki_test3$vandal)








