--------------K-means Clustering-------------------

###################################################
# Step 1: Set the Working Directory
###################################################
setwd("~/raveen/Kmeans_R")

###################################################
# Step 2: Load the RODBC Package
###################################################
library('RODBC')

###################################################
# Step 3: Open Connections to ODBC Database
###################################################
ch<-odbcConnect('ODBC')

###################################################
# Step 4: Examine Table in the Database
###################################################

sqlColumns(ch,"Geo_Cluster")

###################################################
# Step 5: Read in the Data for Modeling
###################################################
#Similar to following:
#Geo_Cluster <- read.csv("Geo Cluster.csv")

Geo_Cluster <- data.frame(sqlFetch(ch,'Geo_Cluster'))
summary(Geo_Cluster)
View(Geo_Cluster)

###################################################
# Step 6: Metric Selection
###################################################
#Take a look at the k-means function documentation
help(kmeans)

# Clustering based on competitive intensity
# The matrix or dataframe for kmeans should have only 
# the columns that should be used for clustering
Geo_Clus_CI <- data.frame(subset(Geo_Cluster,select = 4))
# Distribution of competitive intensity
hist(Geo_Clus_CI[,1])

# K means model is run here with 3 centres and 15 iterations
# set seed to make sure k means returns same clusters each time
set.seed(243555)
km <- kmeans (Geo_Clus_CI,3,15,nstart = 25)

###################################################
# Step 7: Review the Output
###################################################
km

###################################################
# Step 8: Plot the Results
###################################################

#Plot centers. 
points(km$centers, y=c(0,0,0), col = 1:3, pch = 8)
sorted_centres <- sort(km$centers) 
abline( v= mean(sorted_centres[1:2]))
abline( v= mean(sorted_centres[2:3]))

# Plot Clusters

###################################################
# Step 9: Find the Appropriate Number of Clusters
###################################################

#Plot the within-group-sum of squares and 
#look for an "elbow" of the plot. The elbow 
#(if you can find one) tells you what the 
#appropriate number of clusters probably is.

# for k = 1 to 15 fit the kmeans, 25 times, 
# to determine the smallest within sum of squares (wss)

set.seed(237874)
wss <- numeric(15)
for (i in 1:15) 
  wss[i] <- kmeans(Geo_Clus_CI, centers=i, nstart=25,iter.max = 30)$tot.withinss
plot(1:15, wss, type="b", main="Optimal number of clusters", xlab="Number of Clusters",
     ylab="Within Sum of Squares")

#check withinss matches above

c(wss[3] , sum(km$withinss))


###################################################
# Step 10: Multi Dimensional K means
###################################################
# Kmeans can be performed with more than 1 variable
set.seed(237874)

# Normalizing both variables for equal emphasis in analysis
nor_avg_mth_sale <- (Geo_Cluster$avg_mth_sale - mean(Geo_Cluster$avg_mth_sale))/sd(Geo_Cluster$avg_mth_sale)
nor_comp_int <- (Geo_Cluster$competitive_intensity - mean(Geo_Cluster$competitive_intensity))/sd(Geo_Cluster$competitive_intensity)
Geo_Cluster_2_var <- data.frame(nor_avg_mth_sale,nor_comp_int)

# Faster Normalization using scale function
Geo_Cluster_2_var_scale <- data.frame(scale(Geo_Cluster[,c(3,4)]))

identical(Geo_Cluster_2_var,Geo_Cluster_2_var_scale)

  # find the appropriate number of clusters
  set.seed(237874)
  wss <- numeric(15)
  for (i in 1:15) 
    wss[i] <- kmeans(Geo_Cluster_2_var, centers=i, nstart=25, iter.max = 30)$tot.withinss
  plot(1:15, wss, type="b", main="Optimal number of clusters", xlab="Number of Clusters",
       ylab="Within Sum of Squares")
  # As seen 5 looks the right number of clusters

# run the model
set.seed(237874)
km <- kmeans (Geo_Cluster_2_var,5,25,iter.max = 30)
km

# check withinss matches above
c(wss[5] , sum(km$withinss))

# Plot the clusters
plot(Geo_Cluster_2_var, col = km$cluster)
# Plot with original data
plot(Geo_Cluster[,c(3,4)], col = km$cluster)

###################################################
# Step 11: K means using three normalized variable
###################################################

set.seed(237874)

# Creating dataset for k means without transformation
Geo_Cluster_orig <- data.frame(Geo_Cluster[,c(2,3,4)])

# Adding normalized variable
# Creating data frame for k means
# using scale function to get normalized variable
Geo_Cluster_nor <- scale(Geo_Cluster_orig)

# find the appropriate number of clusters
wss <- numeric(15)
for (i in 1:15) 
  wss[i] <- kmeans(Geo_Cluster_nor, centers=i, nstart=25,iter.max = 30)$tot.withinss
plot(1:15, wss, type="b", main="Optimal number of clusters", xlab="Number of Clusters",
     ylab="Within Sum of Squares")
# As seen 4 looks the right number of clusters

# run the model
km <- kmeans (Geo_Cluster_nor,4,nstart = 25,iter.max = 30)
km

# check withinss matches above

c(wss[4] , sum(km$withinss))

# Plot the clusters
pairs(Geo_Cluster_nor, col = km$cluster)


###################################################
# Step 12: Close Database Connection
###################################################
odbcClose(ch)

###################################################
# Step 13: quit R
###################################################
q()
