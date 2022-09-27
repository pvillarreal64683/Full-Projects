## Task 1: Loading the dataset and packages
# Load ggplot2 package
library(ggplot2)
library(visdat)
library(broom)
library(dplyr)

# Load the data
heart_disease <- read.csv("C:/Users/Peter Villarreal/Desktop/Data Analytics/Data Camp/Courses & Projects/Projects/R/Heart Rate/Cleveland_hd.csv")

# Print the first ten rows of the dataset
head(heart_disease, n = 10)

# Glimpse the dataset
glimpse(heart_disease)

# Visualizing and dropping missing data
vis_miss(heart_disease)
sum(is.na(heart_disease))
no_na_df <- heart_disease %>% filter(!is.na(ca)) %>% filter(!is.na(thal))
vis_miss(no_na_df)


##________________________________________________________________________________________________________________________##


## Task 2: Quantifying Patient Differences
# Evidence that the data should be scaled?
summary(no_na_df)

# Remove id
heart_disease <- no_na_df[ , !(names(no_na_df) %in% c("id"))]

# Scaling data and saving as a data frame
scaled <- scale(heart_disease)

# What do the data look like now?
summary(scaled)


##________________________________________________________________________________________________________________________##


## Task 3: Grouping Patients
# Set the seed so that results are reproducible
seed_val <- 10
set.seed(seed_val)

# Select a number of clusters
k <- 5

# Run the k-means algorithm
first_clust <- kmeans(scaled, centers = k, nstart = 1)

# How many patients are in each cluster?
first_clust$size


##_______________________________________________________________________________________________________________________##


## Task 4: A Second Round of k-means
# Set the seed
seed_val <- 38
set.seed(seed_val)

# Select a number of clusters and run the k-means algorithm
k <- 5
second_clust <- kmeans(scaled, centers = k, nstart = 1)

# How many patients are in each cluster?
second_clust$size


##_______________________________________________________________________________________________________________________##


## Task 5: Comparing Patient Clusters
# Add cluster assignments to the data
heart_disease["first_clust"] <- first_clust$cluster
heart_disease["second_clust"] <- second_clust$cluster

# Create and print the plot of age and chol for the first clustering algorithm
plot_one <- ggplot(heart_disease, aes(x=age, y=chol, color=as.factor(first_clust))) +
  geom_point()
plot_one

# Create and print the plot of age and chol for the second clustering algorithm
plot_two <- ggplot(heart_disease, aes(x=age, y=chol, color=as.factor(second_clust))) + 
  geom_point()
plot_two


##_______________________________________________________________________________________________________________________##


## Task 6: Hierarchical Clustering: Another Clustering Approach
# Execute hierarchical clustering with complete linkage
hier_clust_1 <- hclust(dist(scaled), method = "complete")

# Print the dendrogram
plot(hier_clust_1)

# Get cluster assignments based on number of selected clusters
hc_1_assign <- cutree(hier_clust_1, 5)


##_______________________________________________________________________________________________________________________##


## Task 7: Second Round of Hierarchical Clustering
# Execute hierarchical clustering with single linkage
hier_clust_2 <- hclust(dist(scaled), method = "single")

# Print the dendrogram
plot(hier_clust_2)

# Get cluster assignments based on number of selected clusters
hc_2_assign <- cutree(hier_clust_2, 5)


##_______________________________________________________________________________________________________________________##


## Task 8: Comparing Clustering Results
# Add assignment of chosen hierarchical linkage
heart_disease["hc_clust"] <- hc_1_assign

# Remove the sex, first_clust, and second_clust variables
hd_simple <- heart_disease[, !(names(heart_disease) %in% c("sex", "first_clust", "second_clust"))]

# Get the mean and standard deviation summary statistics
clust_summary <- do.call(data.frame, aggregate(. ~hc_clust, data = hd_simple, function(x) c(avg = mean(x), sd = sd(x))))
clust_summary


##______________________________________________________________________________________________________________________##


## Task 9: Visualizing the Clusters
# Plot age and chol
plot_one <- ggplot(heart_disease, aes(x = age, y = chol, 
                                      color = as.factor(hc_clust))) + 
  geom_point()
plot_one 

# Plot oldpeak and trestbps
plot_two <- ggplot(heart_disease, aes(x = oldpeak, y = trestbps, 
                                      color = as.factor(hc_clust))) + 
  geom_point()
plot_two


##______________________________________________________________________________________________________________________##


## Task 10: Conclusions: Which Algorithm Groups Patients Best? (balanced number of patients in each group)
# Add TRUE if the algorithm shows promise, add FALSE if it does not
explore_kmeans <- FALSE
explore_hierarch_complete <- TRUE
explore_hierarch_single <- FALSE