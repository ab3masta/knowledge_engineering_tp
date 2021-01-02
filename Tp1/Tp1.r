########################### Remove all objects #########################

rm(list = ls(all = TRUE))

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

#### Quelque Traitement dans le dataset

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
head(newIrisData)
# Estimating the optimal number of clusters

wss <- (nrow(newIrisData) - 1) * sum(apply(newIrisData, 3, var))
for (i in 2:15)
  wss[i] <- sum(kmeans(newIrisData, centers = i)$withinss)
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# K-Means Cluster Analysis
###### exemple1

fit <- kmeans(newIrisData, 3, nstart = 25) # 3 cluster solution
ww <- fit$cluster
class(ww)
newX <- newIrisData[, c(3)]
newY <- newIrisData[, c(4)]
##### Plot clusters with Petal lenght and width
qplot(newX, newY, color = fit$cluster, xlab = "Petal Lenght", ylab = "Petal width")
##### exemple2 avec le packages factoextra
set.seed(123)
res.km <- kmeans(newIrisData, 3, nstart = 25)
require(factoextra)
fviz_cluster(res.km, data = Iris[, -5], palette = c("#2E9FDF", "#00AFBB", "#E7B800"), geom = "point",
             ellipse.type = "convex", ggtheme = theme_bw())