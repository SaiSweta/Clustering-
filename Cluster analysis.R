install.packages("NbClust")
library(xlsx)
library(ggplot2)
library(dplyr)
library(cluster)
library(fpc)
library(factoextra)
library(NbClust)

# Importing the dataset:
espndata <- read.xlsx("Imputeddata-V3.xlsx",sheetName = "Sheet1",header=TRUE)
espndata_copy <- espndata
summary(espndata)

# Split the files into batsman, bowler and All-rounder data
out <- split(espndata, f = espndata$Player_Type)
allrounderdata <- out[[1]]
batsmandata <- out[[2]]
bowlerdata <- out[[3]]

################################################
# CLUSTER ANALYSIS FOR BATSMAN DATA
################################################

# Leave out few variables as part of data prep for cluster analysis
summary(batsmandata)

# T20I_Bowl_4W and T20I_Bowl_5w are having only zeroes as value for Batsman data
# Hence scaling is populating it with all Null values...Columns 78 and 79 removed
# in case of Batsman data
seldata <- as.data.frame(batsmandata[-c(1:3,5:19,78:79,112:148)])

# Normalize the data
m <- apply(seldata, 2, mean)
s <- apply(seldata, 2, sd)
seldata <- scale(seldata,m,s)

summary(seldata)

# Scree plot
wss <- (nrow(seldata)-1)*sum(apply(seldata,2,var))
wss

pdf("batnbcluster.pdf")
fviz_nbclust(seldata, kmeans, method = "wss") + 
  geom_vline(xintercept = 4, linetype = 2)
dev.off()

for( i in 2:20) wss[i] <- sum(kmeans(seldata, centers=i)$withinss)
plot(1:20, wss, type="b", Xlab="Number of Cluster", ylab="Within group SS")

# K means cluster analysis

set.seed(1234)
head(seldata)
kc <- kmeans(seldata,4,nstart=25)
print(kc)

aggregate(batsmandata,by=list(kc$cluster),FUN=mean)
kc$cluster <- as.factor(kc$cluster)
batsmancluster <- data.frame(batsmandata,kc$cluster)
write.xlsx(batsmancluster,file="batcluster.xlsx",row.names=FALSE)

pdf("batcluster.pdf")
fviz_cluster(kc,data=seldata,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#FC00FC"), 
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())
dev.off()

################################################
# CLUSTER ANALYSIS FOR BOWLER DATA
################################################

summary(bowlerdata)

# T20_Bat_100, T20_St, T20I_St, T20I_Bat_100, T20I_Bat_50, ODI_St, ODI_Bat_100,
# LA_St and LA_Bat_100 are having only zeroes as value for Bowler data
# Scaling is populating it with all Null values...

seldata <- as.data.frame(bowlerdata[-c(1:3,5:19,28,33,42:43,47,56,61,97,102,112:148)])

summary(seldata)

# Normalize the data
m <- apply(seldata, 2, mean)
s <- apply(seldata, 2, sd)
seldata <- scale(seldata,m,s)

# Scree plot
wss <- (nrow(seldata)-1)*sum(apply(seldata,2,var))
wss


for( i in 2:20) wss[i] <- sum(kmeans(seldata, centers=i)$withinss)
plot(1:20, wss, type="b", Xlab="Number of Cluster", ylab = "Within group SS")


pdf("bowlnbcluster.pdf")
fviz_nbclust(seldata, kmeans, method = "wss") + 
  geom_vline(xintercept = 3, linetype = 2)
dev.off()

summary(seldata)

# K means cluster analysis

set.seed(1234)
head(seldata)
kc <- kmeans(seldata,3,nstart=25)
print(kc)

aggregate(bowlerdata,by=list(kc$cluster),FUN=mean)
kc$cluster <- as.factor(kc$cluster)
bowlercluster <- data.frame(bowlerdata,kc$cluster)
write.xlsx(bowlercluster,file="bowlercluster.xlsx",row.names=FALSE)

pdf("bowlerplot.pdf")
fviz_cluster(kc,data=seldata,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             main = "Cluster plot for Bowlers",
             ggtheme = theme_minimal())
dev.off()

################################################
# CLUSTER ANALYSIS FOR ALLROUNDER DATA
################################################

summary(allrounderdata)

# T20_Bat_100, T20_St, T20I_St, T20I_Bat_100, T20I_Bat_50, ODI_St, ODI_Bat_100,
# LA_St and LA_Bat_100 are having only zeroes as value for Bowler data
# Scaling is populating it with all Null values...Columns 78 and 79 removed
# in case of Batsman data

#seldata <- as.data.frame(bowlerdata[-c(1:3,5:19,28,33,42:43,47,56,61,97,102,112:148)])
seldata <- as.data.frame(allrounderdata[-c(1:3,5:19,33,47,78:79,102,112:148)])

# Normalize the data
m <- apply(seldata, 2, mean)
s <- apply(seldata, 2, sd)
seldata <- scale(seldata,m,s)

# Scree plot
wss <- (nrow(seldata)-1)*sum(apply(seldata,2,var))
wss

summary(seldata)

for( i in 2:20) wss[i] <- sum(kmeans(seldata, centers=i)$withinss)
plot(1:20, wss, type="b", Xlab="Number of Cluster", ylab = "Within group SS")


pdf("allnbcluster.pdf")
fviz_nbclust(seldata, kmeans, method = "wss") + 
  geom_vline(xintercept = 3, linetype = 2)
dev.off()

summary(seldata)

# K means cluster analysis

set.seed(1234)
head(seldata)
kc <- kmeans(seldata,3,nstart=25)
print(kc)

aggregate(allrounderdata,by=list(kc$cluster),FUN=mean)
kc$cluster <- as.factor(kc$cluster)
allroundercluster <- data.frame(allrounderdata,kc$cluster)
write.xlsx(allroundercluster,file="allroundercluster.xlsx",row.names=FALSE)

pdf("allrounderplot.pdf")
fviz_cluster(kc,data=seldata,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             main = "Cluster plot for All-rounders",
             ggtheme = theme_minimal())
dev.off()

