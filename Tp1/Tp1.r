########################### Set working directory #########################

rm(list = ls(all = TRUE))
setwd("D:/Master2/S1/Ingénierie des connaissances -- IC/Tp/Tp1/")
getwd()

######## Load datasets

Iris <- read.csv("data/iris.csv", header = TRUE)
str(Iris)
View(Iris)
Iris$species

################################# Data Preparation     ##################################
#### Explore Data

lapply(Iris, function(x) { length(which(is.null(x))) }) # Aucune champ vide trouver
summary(Iris) # On a 0 comme valeur minimal
Iris <- na.omit(Iris) # Permet de supprimer les éléments manquants
dim(Iris) # On a les meme dimensions parcequ'il aucune champ vide

#### Quelque Traitement dans le datasets

require(tidyverse)
install.packages("tidyverse")
require("cluster")
# Rename the first columns `..sepal_length by sepal_length by indexing
names(Iris)[1] <- "sepal_length"
head(Iris)
require(dplyr)
unique(Iris[["species"]]) # get iris category
Iris <- Iris %>% mutate(species = ifelse(as.character(species) == "setosa", 1, ifelse(as.character(species) == "versicolor", 2, ifelse(as.character(species) == "virginica", 3, as.numeric(species))))) # Now we have 1 for setosa
head(Iris)
unique(Iris[["species"]]) # always 3 category

#### Standardize

irisStandardize <- scale(Iris)
head(irisStandardize)
plot(irisStandardize[, c(1)], irisStandardize[, c(2)], type = 'l')

## chose only lenght and width

newIrisData <- Iris[, c(1, 2, 3, 4)] # standardize variables
newIrisData

# Estimating the optimal number of clusters

wss <- (nrow(newIrisData) - 1) * sum(apply(newIrisData, 2, var))
for (i in 2:15)
  wss[i] <- sum(kmeans(newIrisData, centers = i)$withinss)
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(newIrisData, 3, nstart = 100) # 3 cluster solution
ww <- fit$cluster
class(ww)
newX <- newIrisData[, c(3)]
newY <- newIrisData[, c(4)]

qplot(newX, newY, color = fit$cluster)
# get cluster means
aggregate(newIrisData, by = list(fit$cluster), FUN = mean)
# append cluster assignment
newIrisData <- data.frame(newIrisData, fit$cluster)

# Ward Hierarchical Clustering
d <- dist(newIrisData, method = "euclidean") # distance matrix
fit <- hclust(d, method = "ward")
plot(fit) # display dendogram
groups <- cutree(fit, k = 5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k = 5, border = "red")