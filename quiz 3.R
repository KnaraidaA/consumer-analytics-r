#run tidyverse and data set
library(tidyverse)
setwd("C:\\Users\\kchar\\OneDrive\\Desktop\\mgt 100")
cust_dat <- read_csv ("smartphone_customer_data.csv")
#1 want scale video and social

sub <- cust_dat |> select(video, social) 
sub
#scale data
scl <- sub |> scale() |> as_tibble()
scl

K <- 5
D <- 10

set.seed(123)
out <- kmeans(scl, centers=K, nstart=D)
sub <- sub |> mutate(cluster = factor(out$cluster))
# for counting and labeling clusters
sub |> group_by(cluster) |> summarize(n = n()) |> arrange(desc(n)) |> mutate(label = LETTERS[1:5])


#2 centers of unscaled clusters
unscale <- function(c, d) {
  numeric_data <- d |> select(where(is.numeric))
  
  # calculate mean and sd
  SD   <- numeric_data |> summarize(across(everything(), sd)) |> unlist()
  MEAN <- numeric_data |> summarize(across(everything(), mean)) |> unlist()
  
  # Convert back to original scale
  as_tibble(c * SD + MEAN) |> round(0)
}
# Compute unscaled cluster centers
centers <- unscale(out$centers, sub)

# Label clusters by size
cluster_sizes <- sub |> group_by(cluster) |> summarize(n = n()) |> arrange(desc(n)) |> mutate(label = LETTERS[1:5])

# Map clusters to segment labels
centers <- centers |> mutate(Segment = cluster_sizes$label)

# Print final centroids
print(centers)

#q3 WSS 
out$tot.withinss

#finding elbow with scree plot
for(i in 1:5) {
  # run k means
  out <- kmeans(scl, centers=i, nstart=10)
  
  # grab the WSS value, store it in the i'th position of res
  res[i] <- out$tot.withinss
}

# let's plot the WSS for each value of k
ggplot(data.frame(x=1:5, y=res), aes(x,y)) + 
  geom_line(color="grey") + 
  geom_point(size=3) + 
  xlab("Number of Clusters (K)") + 
  ylab("Within-Group Sum of Squares (WSS)") + 
  theme_minimal()

#q5 kmeans with 3

K <- 3
D <- 25

out <- kmeans(scl, centers=K, nstart=D)
sub <- sub |> mutate(cluster = factor(out$cluster))
#create plot
ggplot(sub, aes(x = video,y = social, color = cluster)) +
  geom_point(size = 1) +
  labs(title = "K=3", x= "video min", y = "social min") +
  theme_minimal()
#cluster analysis
cluster_sum <- sub |>
  group_by(cluster) |>
  summarize(mean_video = mean(video), 
            mean_social = mean(social))