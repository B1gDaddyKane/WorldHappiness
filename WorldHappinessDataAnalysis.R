install.packages(c("NbClust","cluster","flexclust"))
library("NbClust")
library("cluster")
library("flexclust")

#Loading all the data.
happiness_2015 <- read.csv("2015.csv")
happiness_2016 <- read.csv("2016.csv")
happiness_2017 <- read.csv("2017.csv")
happiness_2018 <- read.csv("2018.csv")
happiness_2019 <- read.csv("2019.csv")

#--------------------PARTIONING CLUSTER ANALYSIS FOR 2015------------------#

#Removing non-numerical data from 2015 and scaling it.
num.15 <- happiness_2015[c(-1,-2,-3)]
df.15 <- scale(num.15)

#Setting seed to avoid randomization .
#Finding the optimal number for clusters.
set.seed(1234)
nc.15 <- NbClust(df.15,min.nc = 2,max.nc = 15,method = "kmeans")
table(nc.15$Best.n[1,])

#Plotting the optimal number of clusters with barplot.
barplot(table(nc.15$Best.n[1,]),xlab="Number of Clusters",
        ylab = "Number of Criteria",
        main = "Number of Clusters Chosen by 26 Criteria")

#Setting seed and making 3 clusters.
set.seed(1234)
fit.km.15 <- kmeans(df.15,3,nstart=25)
fit.km.15$size

#Printing the three centers of the clusters.
fit.km.15$centers

#We aggregate the data to determine variable means 
#for each cluster in the original metric.
aggregate(num.15,by=list(cluster=fit.km.15$cluster),mean)

#A cross-tabulation of Country and cluster membership is given.
ct.country.km.15 <- table(happiness_2015$Country, fit.km.15$cluster)
ct.country.km.15

#A cross-tabulation of Region and cluster membership is given.
ct.region.km.15 <- table(happiness_2015$Region, fit.km.15$cluster)
ct.region.km.15

#We partion around medoids (PAM) and make a cluster plot
set.seed(1234)
fit.pam.15 <- pam(num.15,k=3,stand = TRUE)
fit.pam.15$medoids
clusplot(fit.pam.15, main="Bivariate Cluster Plot")

#--------------------HIERARCHICAL CLUSTER ANALYSIS 2015--------------------#

#Removing non-numerical data from 2015 and scaling it.
num.15 <- happiness_2015[c(-1,-2,-3)]

#Find ud af hvordan vi får landenavne ind i dataframen uden at fucke med scaling.

#num.15$Country <- factor(num.15$Country, levels= unique(happiness_2015$Happiness.Rank),)
#levels(num.15$Country)

df.15 <- scale(num.15)

d.15 <- dist(df.15)

fit.average.15 <- hclust(d.15, method = "average")
plot(fit.average.15, hang=-1,cex=.5, main="Average Linkage Clustering")

nc.15 <- NbClust(df.15, distance="euclidean",min.nc=2,max.nc=15,method="average")
table(nc.15$Best.n[1,])

barplot(table(nc.15$Best.n[1,]), xlab="Number of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

clusters <- cutree(fit.average.15, k=6)
table(clusters)

aggregate(num.15, by=list(cluster=clusters),median)

aggregate(as.data.frame(df.15),by=list(cluster=clusters),median)

plot(fit.average.15, hang=-1, cex=.5, main="Average Linkage Clustering\n6 Cluster Solution")
rect.hclust(fit.average.15, k=6)

#-------------------PARTIONING CLUSTER ANALYSIS FOR 2016-------------------#


#-------------------PARTIONING CLUSTER ANALYSIS FOR 2017-------------------#


#-------------------PARTIONING CLUSTER ANALYSIS FOR 2018-------------------#


#-------------------PARTIONING CLUSTER ANALYSIS FOR 2019-------------------#



