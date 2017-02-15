library(dplyr) # for data cleaning
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization

dat=read.csv("")


#The concept of Gower distance is actually quite simple. For each variable type,
#a particular distance metric that works well for that type is used and 
#scaled to fall between 0 and 1. 
#quantitative (interval): range-normalized Manhattan distance
#ordinal: variable is first ranked, then Manhattan distance is used
#with a special adjustment for ties
#nominal: variables of k categories are first converted into k binary columns 
#and then the Dice coefficient is used

gower_dist <- daisy(dat,
                    metric = "gower",
                    type = list(logratio = 3))
summary(gower_dist)

gower_mat <- as.matrix(gower_dist)



#Choosing a clustering algorithm

#Now that the distance matrix has been calculated, it is time to select an algorithm for clustering. While many algorithms that can handle a custom distance matrix exist, partitioning around medoids (PAM) will be used here.

#Partitioning around medoids is an iterative clustering procedure with the following steps:
  
#Choose k random entities to become the medoids
#Assign every entity to its closest medoid (using our custom distance matrix in this case)
#For each cluster, identify the observation that would yield the lowest average distance if it were to be re-assigned as the medoid. If so, make this observation the new medoid.
#If at least one medoid has changed, return to step 2. Otherwise, end the algorithm.

#If you know the k-means algorithm, this might look very familiar. In fact, both approaches are identical, except k-means has cluster centers defined by Euclidean distance (i.e., centroids), while cluster centers for PAM are restricted to be the observations themselves (i.e., medoids).


#A variety of metrics exist to help choose the number of clusters to be extracted
#in a cluster analysis. We will use silhouette width, an internal validation 
#metric which is an aggregated measure of how similar an observation is to its 
#own cluster compared its closest neighboring cluster. The metric can range 
#from -1 to 1, where higher values are better. After calculating silhouette 
#width for clusters ranging from 2 to 10 for the PAM algorithm

sil_width <- c(NA)

for(i in 2:11){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:11, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:11, sil_width)

#clustering with k=4
pam_fit <- pam(gower_dist, diss = TRUE, k = 4)


pam_results <- dat %>%
   mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary


#visualizing the cluster


tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering)
        )

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

pam_results$the_summary[3][1]

cluster1=pam_results$the_summary[1][1]


