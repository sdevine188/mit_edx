library(stringr)
library(readr)
library(dplyr)

# setwd
setwd("C:/Users/Stephen/Desktop/R/mit_edx/unit6_clustering")

# load data
list.files()
movies <- read_delim("movies.txt", delim = "|", col_names = FALSE)
glimpse(movies)

# clean data
names(movies) <- c("id", "title", "release_date", "video_release_date", "imdb", "unknown", "action", "adventure",
                   "animation", "children", "comedy", "crime", "documentary", "drama", "fantasy", "film_noir",
                   "horror", "musical", "mystery", "romance", "sci_fi", "thriller", "war", "western")
movies <- movies %>% select(-c(id, release_date, video_release_date, imdb))
movies <- unique(movies)
glimpse(movies)

# random questions
movies %>% select(-c(title)) %>% colSums(.)
movies %>% filter(romance == 1 & drama == 1) %>% tally()

# compute euclidian distance
movies_dist <- dist(movies[2:20], method = "euclidean")
head(movies_dist)

# example of distance function
x <- c(1, 2, 3)
y <- c(3, 4, 5)
sqrt(12)
df <- data.frame(x, y)
df
dist(df)
# distance between row 1 and row 2
sqrt((1-2)^2 + (3-4)^2)

# run hierarchical clustering
movies_cluster <- hclust(movies_dist, method = "ward.D")
movies_cluster
names(movies_cluster)
plot(movies_cluster)

movies_cluster_groups <- cutree(movies_cluster, k = 10)
glimpse(movies_cluster_groups)
length(movies_cluster_groups)
glimpse(movies)

# inspect clusters
movies$cluster_group <- movies_cluster_groups
movies %>% group_by(cluster_group) %>% summarize(action_avg = mean(action))
movies %>% group_by(cluster_group) %>% summarize(romance_avg = mean(romance))

# inspect
movies %>% filter(grepl("men in black", title, ignore.case = TRUE)) %>% select(title, cluster_group)
movies %>% filter(cluster_group == 2) %>% select(title)

# quiz question
movies_cluster_groups2 <- cutree(movies_cluster, k = 2)
movies$cluster_group2 <- movies_cluster_groups2
unique(movies$cluster_group2)
movies %>% filter(cluster_group2 == 1) %>% select(-c(title)) %>% colSums(.)
movies %>% filter(cluster_group2 == 2) %>% select(-c(title)) %>% colSums(.)


########################################################################


# recitation

# load flower data
list.files()
flower <- read_csv("flower.csv", col_names = FALSE)
glimpse(flower)
# 50 pixels by 50 pixels

# need to convert to matrix, then to vector, so it's a long sequence of 2500 pixel intensities
flower_matrix <- as.matrix(flower)
flower_vector <- as.vector(flower_matrix)
str(flower_vector)

# create distance pair-wise euclidean distance matrix 
flower_distance <- dist(flower_vector, method = "euclidean")
head(flower_distance)
str(flower_distance)
(2500 * (2500 - 1)) / 2

# run hierarchical clustering
flower_clusters <- hclust(flower_distance, method = "ward.D")
str(flower_clusters)

# inspect dendrogram
plot(flower_clusters)
rect.hclust(flower_clusters, k = 3, border = "red")

# cut tree at k = 3
flower_cluster_groups <- cutree(flower_clusters, k = 3)
str(flower_cluster_groups)
unique(flower_cluster_groups)

# add cluster groups to flower df
flower_vector_df <- data.frame(pixel = flower_vector, cluster_group = flower_cluster_groups)
flower_vector_df %>% group_by(cluster_group) %>% 

