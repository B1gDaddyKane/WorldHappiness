install.packages(c("NbClust","cluster","flexclust","rpart","rpart.plot","party",
                   "randomForest","e1071", "ggplot2", "RColorBrewer", "corrplot",
                   "ggfortify", "ggrepel"))
library("NbClust")
library("cluster")
library("flexclust")
library("rpart")
library("rpart.plot")
library("party")
library("randomForest")
library("e1071")
library("ggplot2")
library("RColorBrewer")
library("corrplot")
library("ggfortify")
library("ggrepel")

#Loading all the data.
happiness_2015 <- read.csv("2015.csv")
happiness_2016 <- read.csv("2016.csv")
happiness_2017 <- read.csv("2017.csv")
happiness_2018 <- read.csv("2018.csv")
happiness_2019 <- read.csv("2019.csv")

####------------------------DATA VISUALIZATION--------------------------####

####Bar plot of top 10 countries by GDP####

#Sort data by GDP
data_sorted <- happiness_2015[order(-happiness_2015$Economy..GDP.per.Capita.),]
#Choose top 10 countries by GDP
data_top <- data_sorted[1:10,]
#Make country a factor and reverse order (a hack to avoid reversal of bars
#when we do coord_flip)
data_top$Country <- factor(data_top$Country, levels = rev(data_top$Country))
#Create ggplot object
plot <- ggplot(data=data_top,
  aes(x=Country, y=Economy..GDP.per.Capita., fill=Economy..GDP.per.Capita.)) + 
  geom_bar(stat="identity") + 
  theme(legend.position="none", axis.title.y=element_blank()) +
  scale_fill_gradient(low = "red", high = "yellow")
#Display ggplot
plot + coord_flip()


####Bar plot of top 10 countries by Family####

#Sort data by Family
data_sorted <- happiness_2015[order(-happiness_2015$Family),]
#Choose top 10 countries by Family
data_top <- data_sorted[1:10,]
#Make country a factor and reverse order (a hack to avoid reversal of bars
#when we do coord_flip)
data_top$Country <- factor(data_top$Country, levels = rev(data_top$Country))
#Create ggplot object
plot <- ggplot(data=data_top,
               aes(x=Country, y=Family, fill=Family)) + 
  geom_bar(stat="identity") + 
  theme(legend.position="none", axis.title.y=element_blank()) +
  scale_fill_gradient(low = "yellow", high = "green")
#Display ggplot
plot + coord_flip()


####Bar plot of top 10 countries by Life Expectancy####

#Sort data by Life Expectancy
data_sorted <- happiness_2015[order(-happiness_2015$Health..Life.Expectancy.),]
#Choose top 10 countries by Life Expectancy
data_top <- data_sorted[1:10,]
#Make country a factor and reverse order (a hack to avoid reversal of bars
#when we do coord_flip)
data_top$Country <- factor(data_top$Country, levels = rev(data_top$Country))
#Create ggplot object
plot <- ggplot(data=data_top,
               aes(x=Country, y=Health..Life.Expectancy., fill=Health..Life.Expectancy.)) + 
  geom_bar(stat="identity") + 
  theme(legend.position="none", axis.title.y=element_blank()) +
  scale_fill_gradient(low = "green", high = "blue")
#Display ggplot
plot + coord_flip()


####Bar plot of top 10 countries by Freedom####

#Sort data by Freedom
data_sorted <- happiness_2015[order(-happiness_2015$Freedom),]
#Choose top 10 countries by Freedom
data_top <- data_sorted[1:10,]
#Make country a factor and reverse order (a hack to avoid reversal of bars
#when we do coord_flip)
data_top$Country <- factor(data_top$Country, levels = rev(data_top$Country))
#Create ggplot object
plot <- ggplot(data=data_top,
               aes(x=Country, y=Freedom, fill=Freedom)) + 
  geom_bar(stat="identity") + 
  theme(legend.position="none", axis.title.y=element_blank()) +
  scale_fill_gradient(low = "orange", high = "blue")
#Display ggplot
plot + coord_flip()


####Bar plot of top 10 countries by Trust/Government Corruption####

#Sort data by Trust/Government Corruption
data_sorted <- happiness_2015[order(-happiness_2015$Trust..Government.Corruption.),]
#Choose top 10 countries by Trust/Government Corruption
data_top <- data_sorted[1:10,]
#Make country a factor and reverse order (a hack to avoid reversal of bars
#when we do coord_flip)
data_top$Country <- factor(data_top$Country, levels = rev(data_top$Country))
#Create ggplot object
plot <- ggplot(data=data_top,
               aes(x=Country, y=Trust..Government.Corruption., fill=Trust..Government.Corruption.)) + 
  geom_bar(stat="identity") + 
  theme(legend.position="none", axis.title.y=element_blank()) +
  scale_fill_gradient(low = "blue", high = "purple")
#Display ggplot
plot + coord_flip()


####Bar plot of top 10 countries by Generosity####

#Sort data by Generosity
data_sorted <- happiness_2015[order(-happiness_2015$Generosity),]
#Choose top 10 countries by Generosity
data_top <- data_sorted[1:10,]
#Make country a factor and reverse order (a hack to avoid reversal of bars
#when we do coord_flip)
data_top$Country <- factor(data_top$Country, levels = rev(data_top$Country))
#Create ggplot object
plot <- ggplot(data=data_top,
               aes(x=Country, y=Generosity, fill=Generosity)) + 
  geom_bar(stat="identity") + 
  theme(legend.position="none", axis.title.y=element_blank()) +
  scale_fill_gradient(low = "green", high = "orange")
#Display ggplot
plot + coord_flip()


####Bar plot of top 10 countries by Dystopia####

#Sort data by Dystopia
data_sorted <- happiness_2015[order(-happiness_2015$Dystopia.Residual),]
#Choose top 10 countries by Dystopia
data_top <- data_sorted[1:10,]
#Make country a factor and reverse order (a hack to avoid reversal of bars
#when we do coord_flip)
data_top$Country <- factor(data_top$Country, levels = rev(data_top$Country))
#Create ggplot object
plot <- ggplot(data=data_top,
               aes(x=Country, y=Dystopia.Residual, fill=Dystopia.Residual)) + 
  geom_bar(stat="identity") + 
  theme(legend.position="none", axis.title.y=element_blank()) +
  scale_fill_gradient(low = "black", high = "purple")
#Display ggplot
plot + coord_flip()


####Pair plot of the dataset####

pairs(happiness_2015[,6:12], pch=20, cex=0.5, cex.labels=1.2, col="#00AFBB")


####Heatmap of the dataset####

columns <- sapply(happiness_2015, is.numeric)
correlated_data <- cor(happiness_2015[, columns])
corrplot(correlated_data, method = 'number', col= colorRampPalette(c("red", "yellow", "blue"))(200)) 

data_no_rank <- happiness_2015[2:12]
columns <- sapply(data_no_rank, is.numeric)
correlated_data <- cor(data_no_rank[, columns])
corrplot(correlated_data, method = 'number', col= colorRampPalette(c("red", "yellow", "blue"))(200)) 


####-----------------PARTIONING CLUSTER ANALYSIS FOR 2015---------------####

#Removing non-numerical data from 2015 and scaling it.
num.15 <- happiness_2015[c(-1,-2,-3,-4,-5)]

#Setting rownames to countries
rownames(num.15) = happiness_2015$Country

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
aggregate(num.15,by=list(cluster=fit.km.15$cluster), mean)

#A cross-tabulation of Country and cluster membership is given.
ct.country.km.15 <- table(happiness_2015$Country, fit.km.15$cluster)
ct.country.km.15

#A cross-tabulation of Region and cluster membership is given.
ct.region.km.15 <- table(happiness_2015$Region, fit.km.15$cluster)
ct.region.km.15

#We partion around medoids (PAM) and make a cluster plot
set.seed(1234)
fit.pam.15 <- pam(num.15, k=3, stand=TRUE)
fit.pam.15$medoids
clusplot(fit.pam.15, labels=3, main="Bivariate Cluster Plot")

#ggplot2 version of cluster plot
#autoplot(fit.pam.15, label=TRUE, frame=TRUE, frame.type='norm')

#ggplot(fit.pam.15)

####----------------HIERARCHICAL CLUSTER ANALYSIS 2015-----------------####

#Removing non-numerical data from 2015 and scaling it.
num.15 <- happiness_2015[c(-1,-2,-3,-4,-5)]

#Setting rownames to countries
rownames(num.15) = happiness_2015$Country

df.15 <- scale(num.15)

d.15 <- dist(df.15)

fit.average.15 <- hclust(d.15, method = "average")
plot(fit.average.15, hang=-1,cex=.5, main="Average Linkage Clustering")

nc.15 <- NbClust(df.15, distance="euclidean",min.nc=2,max.nc=15,method="average")
table(nc.15$Best.n[1,])

barplot(table(nc.15$Best.n[1,]), xlab="Number of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

clusters <- cutree(fit.average.15, k=8)
table(clusters)

aggregate(num.15, by=list(cluster=clusters),median)

aggregate(as.data.frame(df.15),by=list(cluster=clusters),median)

plot(fit.average.15, hang=-1, cex=.7, main="Average Linkage Clustering\n8 Cluster Solution")
rect.hclust(fit.average.15, k=8)

####-----------------Classification Random Forest 2015------------------####

num.15 <- data.frame(happiness_2015[c(-1,-2,-3,-5)])

set.seed(1234)
train <- sample(nrow(num.15), 0.7*nrow(num.15))
num.15.train <- num.15[train,]
num.15.validate <- num.15[-train,]

set.seed(1234)
fit.forest <- randomForest(formula=Happiness.Score~., data=num.15.train,
                           mtry=6, importance=TRUE)
fit.forest
importance(fit.forest, type=2)

forest.pred <- predict(fit.forest, num.15.validate)
forest.perf <- table(num.15.validate$Happiness.Score, forest.pred, dnn=c("Actual", "Predicted"))
forest.perf

####----------------PARTIONING CLUSTER ANALYSIS FOR 2016----------------####


####-----------------HIERARCHICAL CLUSTER ANALYSIS 2016-----------------####


####----------------PARTIONING CLUSTER ANALYSIS FOR 2017----------------####


####-----------------HIERARCHICAL CLUSTER ANALYSIS 2017-----------------####


####----------------PARTIONING CLUSTER ANALYSIS FOR 2018----------------####


####-----------------HIERARCHICAL CLUSTER ANALYSIS 2018-----------------####


####----------------PARTIONING CLUSTER ANALYSIS FOR 2019----------------####


####-----------------HIERARCHICAL CLUSTER ANALYSIS 2019-----------------####




