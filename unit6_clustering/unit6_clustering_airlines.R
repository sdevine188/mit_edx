library(stringr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(ggdendro)

# setwd
setwd("C:/Users/Stephen/Desktop/R/mit_edx/unit6_clustering")

# load data
list.files()
air <- read_csv("AirlinesCluster.csv")
glimpse(air)

# random questions
colMeans(air)
air %>% summarize_all(funs(mean))

# normalize data
air_pre_process <- preProcess(air)
names(air_pre_process)
class(air_pre_process)
air_norm <- predict(air_pre_process, air)
glimpse(air_norm)
air_norm %>% summarize_all(funs(mean))
air_norm %>% summarize_all(funs(sd))
air_norm %>% summarize_all(funs(max))
air_norm %>% summarize_all(funs(min))

# confirm normalization for fun
air %>% summarize_at(vars(Balance:DaysSinceEnroll), mean)
balance_avg <- air %>% summarize(balance_avg = mean(Balance)) %>% .$balance_avg
balance_sd <- air %>% summarize(balance_sd = sd(Balance)) %>% .$balance_sd
air %>% mutate(balance_norm = (Balance - balance_avg) / balance_sd) 

# compute euclidean distance matrix
air_norm_distance <- dist(air_norm, method = "euclidean")
names(air_norm_distance)
glimpse(air_norm_distance)
head(air_norm_distance)

# run hierarchical clustering
air_clusters <- hclust(air_norm_distance, method = "ward.D")
str(air_clusters)
summary(air_clusters)

ggdendrogram(air_clusters, size = 2)
plot(air_clusters)

# cut tree to 5 clusters
air_clusters_groups <- cutree(air_clusters, k = 5)
str(air_clusters_groups)
unique(air_clusters_groups)
glimpse(air)

# add groups to air and air_norm, then get centroids for each cluster (aka the mean for each variable in each cluster)
air_norm$groups <- air_clusters_groups
air_norm %>% group_by(groups) %>% tally()

air$groups <- air_clusters_groups
air %>% group_by(groups) %>% summarize_all(funs(mean))


########################################################################


# run kmeans clustering
k <- 5
set.seed(88)
air_kmeans_clusters <- kmeans(air_norm, centers = k, iter.max = 1000)
str(air_kmeans_clusters)
unique(air_kmeans_clusters$cluster)
air_kmeans_clusters$centers

air$kmeans_groups <- air_kmeans_clusters$cluster
air %>% group_by(kmeans_groups) %>% tally()
air %>% group_by(kmeans_groups) %>% summarize_all(funs(mean))













