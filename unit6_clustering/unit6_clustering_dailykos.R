library(stringr)
library(readr)
library(dplyr)
library(GGally)
library(corrplot)
library(caTools)
library(caret)
library(flexclust)
library(ggplot2)
library(ggdendro)

# setwd
# setwd("C:/Users/Stephen/Desktop/R/mit_edx/unit6_clustering")
setwd("C:/Users/sdevine/Desktop/edx/unit6_clustering")

# load data
# data is document-term matrix converted to dataframe
list.files()
dk <- read_csv("dailykos.csv")
glimpse(dk)
dim(dk)
names(dk)
names(dk)[1:10]


# create distance matrix using all variables (since all variables are words)
dk_distance <- dist(dk, method = "euclidean")
head(dk_distance)
str(dk_distance)

# create hierarchical clustering
dk_clusters <- hclust(dk_distance, method = "ward.D")
dk_clusters
names(dk_clusters)
plot(dk_clusters)
ggdendrogram(dk_clusters)

# purpose is to create clusters to create categories for recommending political articles to users
# lets use 7 clusters
dk_clusters_7 <- cutree(dk_clusters, k = 7)
glimpse(dk_clusters_7)
length(dk_clusters_7)
unique(dk_clusters_7)
dim(dk)

# combine clusters w dk dataframe
# answer descriptive questions
dk$cluster <- dk_clusters_7
dk %>% filter(cluster == 3) %>% tally()
dk %>% group_by(cluster) %>% tally() %>% arrange(desc(n))
dk %>% filter(cluster == 4) %>% summarize_each(funs(mean)) %>% gather(term, mean, abandon:zone) %>% top_n(6, mean)
dk %>% group_by(cluster) %>% summarize_each(funs(mean)) %>% group_by(cluster) %>% 
        gather(term, mean, abandon:zone) %>% top_n(6, mean) %>% arrange(cluster, desc(mean)) %>% data.frame(.)


#######################################################3


# run k-means algorithm
set.seed(1000)
k <- 7
dk_kmeans_clusters <- kmeans(dk, centers = k)
names(dk_kmeans_clusters)
dk_kmeans_clusters$centers
dk_kmeans_clusters$cluster

# add clusters to df
dk$kmeans_clusters <- dk_kmeans_clusters$cluster
dk %>% group_by(kmeans_clusters) %>% tally() %>% arrange(n)

# get common words for kmeans_clusters
dk %>% group_by(kmeans_clusters) %>% summarize_each(funs(mean)) %>% group_by(kmeans_clusters) %>% 
        gather(term, mean, abandon:zone) %>% top_n(6, mean) %>% arrange(kmeans_clusters, desc(mean)) %>% data.frame(.)

# compare clusters
dk %>% group_by(cluster) %>% summarize_each(funs(mean)) %>% group_by(cluster) %>% 
        gather(term, mean, abandon:zone) %>% top_n(6, mean) %>% arrange(cluster, desc(mean)) %>%
        data.frame(.)

dk %>% group_by(kmeans_clusters) %>% summarize_each(funs(mean)) %>% group_by(kmeans_clusters) %>% 
        gather(term, mean, abandon:zone) %>% top_n(6, mean) %>% arrange(kmeans_clusters, desc(mean)) %>% data.frame(.)






