# Helper packages
library(dplyr)       # for data manipulation
library(ggplot2)     # for data visualization
library(stringr)     # for string functionality
library(gridExtra)   # for manipulaiting the grid
library(tidyverse)  # data manipulation
library(cluster)     # for general clustering algorithms
library(factoextra)  # for visualizing cluster results
library(mclust)

########################### K Means ######################################
df <- na.omit(USArrests)
df <- scale(df)
head(df)


#Determining Optimal Number of Clusters
set.seed(123)

#function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10)$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#or use this
fviz_nbclust(df, kmeans, method = "silhouette")

# compute gap statistic
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)

# Compute k-means clustering with k = 2
set.seed(123)
final <- kmeans(df, 2, nstart = 25)
print(final)

#final data
fviz_cluster(final, data = df)


##################################### Hierarchical ###########################
# Plot cluster results
p1 <- fviz_nbclust(df, FUN = hcut, method = "wss", 
                   k.max = 10) +
  ggtitle("(A) Elbow method")
p2 <- fviz_nbclust(df, FUN = hcut, method = "silhouette", 
                   k.max = 10) +
  ggtitle("(B) Silhouette method")
p3 <- fviz_nbclust(df, FUN = hcut, method = "gap_stat", 
                   k.max = 10) +
  ggtitle("(C) Gap statistic")

# Display plots side by side
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)

# Construct dendorgram for the Ames housing example
hc5 <- hclust(d, method = "ward.D2" )
dend_plot <- fviz_dend(hc5)
dend_data <- attr(dend_plot, "dendrogram")
dend_cuts <- cut(dend_data, h = 2)
fviz_dend(dend_cuts$lower[[2]])

# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 2)

# Number of members in each cluster
table(sub_grp)

# Plot full dendogram
fviz_dend(
  hc5,
  k = 2,
  horiz = TRUE,
  rect = TRUE,
  rect_fill = TRUE,
  rect_border = "jco",
  k_colors = "jco",
  cex = 0.1
)


##################### Model-based ############################################
# Apply GMM model with 3 components
arrest_mc <- Mclust(df, G = 3)

# Plot results
plot(arrest_mc, what = "density")
plot(arrest_mc, what = "uncertainty")

# Observations with high uncertainty
sort(arrest_mc$uncertainty, decreasing = TRUE) %>% head()


summary(arrest_mc)

arrest_optimal_mc <- Mclust(df)

summary(arrest_optimal_mc)

legend_args <- list(x = "bottomright", ncol = 5)
plot(arrest_optimal_mc, what = 'BIC', legendArgs = legend_args)
plot(arrest_optimal_mc, what = 'classification')
plot(arrest_optimal_mc, what = 'uncertainty')

df_mc <- Mclust(df, 1:20)

summary(df_mc)

plot(df_mc, what = 'BIC', 
     legendArgs = list(x = "bottomright", ncol = 5))

probabilities <- df_mc$z 

probabilities <- probabilities %>%
  as.data.frame() %>%
  mutate(id = row_number()) %>%
  tidyr::gather(cluster, probability, -id)

ggplot(probabilities, aes(probability)) +
  geom_histogram() +
  facet_wrap(~ cluster, nrow = 2)

uncertainty <- data.frame(
  id = 1:nrow(df),
  cluster = df_mc$classification,
  uncertainty = df_mc$uncertainty
)

uncertainty %>%
  group_by(cluster) %>%
  filter(uncertainty > 0.0001) %>%
  ggplot(aes(uncertainty, reorder(id, uncertainty))) +
  geom_point() +
  facet_wrap(~ cluster, scales = 'free_y', nrow = 1)


cluster2 <- df %>%
  scale() %>%
  as.data.frame() %>%
  mutate(cluster = df_mc$classification) %>%
  filter(cluster == 2) %>%
  select(-cluster)

cluster2 %>%
  tidyr::gather(product, std_count) %>%
  group_by(product) %>%
  summarize(avg = mean(std_count)) %>%
  ggplot(aes(avg, reorder(product, avg))) +
  geom_point() +
  labs(x = "Average standardized consumption", y = NULL)
