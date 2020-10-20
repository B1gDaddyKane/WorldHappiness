install.packages(c("NbClust","cluster"))
library("NbClust")
library("cluster")

#Loading all the data.
happiness_2015 <- read.csv("2015.csv")
happiness_2016 <- read.csv("2016.csv")
happiness_2017 <- read.csv("2017.csv")
happiness_2018 <- read.csv("2018.csv")
happiness_2019 <- read.csv("2019.csv")

#----------------------CLUSTER ANALYSIS FOR 2015---------------------------#

#Removing non-numerical data from 2015 and scaling it.
fed <- happiness_2015[c(-1,-2,-3)]
df <- scale(fed)

#Setting seed to avoid randomization .
#Finding the optimal number for clusters.
set.seed(1234)
nc <- NbClust(df,min.nc = 2,max.nc = 15,method = "kmeans")
table(nc$Best.n[1,])

#Plotting the optimal number of clusters with barplot.
barplot(table(nc$Best.n[1,]),xlab="Number of Clusters",
        ylab = "Number of Criteria",
        main = "Number of Clusters Chosen by x Criteria")

#Setting seed and making 3 clusters.
set.seed(1234)
fit.km <- kmeans(df,3,nstart=25)
fit.km$size

#Printing the three centers of the clusters.
fit.km$centers

#We aggregate the data to determine variable means 
#for each cluster in the original metric.
aggregate(fed,by=list(cluster=fit.km$cluster),mean)

#A cross-tabulation of Country and cluster membership is given.
ct.country.km <- table(happiness_2015$Country, fit.km$cluster)
ct.country.km

#A cross-tabulation of Region and cluster membership is given.
ct.region.km <- table(happiness_2015$Region, fit.km$cluster)
ct.region.km

#We partion around medoids (PAM) and make a cluster plot
set.seed(1234)
fit.pam <- pam(fed,k=3,stand = TRUE)
fit.pam$medoids
clusplot(fit.pam, main="Bivariate Cluster Plot")

#----------------------CLUSTER ANALYSIS FOR 2016---------------------------#


#----------------------CLUSTER ANALYSIS FOR 2017---------------------------#


#----------------------CLUSTER ANALYSIS FOR 2018---------------------------#


#----------------------CLUSTER ANALYSIS FOR 2019---------------------------#