#### Library in R #########################################################
library(dbscan)
library(RODBC)
library("MASS")
library(vegan)
library(statnet)
library("gplots")
library("cluster")
library("fpc")
library(factoextra)
require("NbClust")
install.packages("NbClust")
########################### Set your work directory #########################
rm(list = ls(all = TRUE))
setwd("E://My-Directory-work// ")
################ Lecture of csv file #########################################
#help(read.csv)
Data_iris <- read.csv("D:/Master2/S1/Ingénierie des connaissances -- IC/Tp/Tp1/data/iris.csv", header = TRUE)

################# Select some columns to cluster your "data"#######################
numb_row = nrow(Data_iris)
numb_row
numb_col = ncol(Data_iris)
head(Data_iris)
datairis1 = Data_iris[, c(1, 2, 3, 4)]
datairis1
datairis2 = Data_iris[, 3:5]
matrix_datairis <- as.matrix(datairis1)
######################## Clustering method 1 - dbscan - ################################
require(dbscan)
require(fpc)
db <- fpc::dbscan(matrix_datairis, eps = 0.04, MinPts = 4)
db$cluster
plot(db$cluster, log = "x")
################# visualization of your results ###########################
library(factoextra)
require(factoextra)
fviz_cluster(db, data = matrix_datairis, stand = FALSE, ellipse = FALSE, show.clust.cent = TRUE, geom = "point", palette = "jco", ggtheme = theme_classic())
######################### Clustering with K-means ####################
km.res <- kmeans(datairis1, 5, nstart = 25)
km.res$cluster
plot(km.res$cluster, log = "x")
################# visualization of your results ####################
fviz_cluster(km.res, df, geom = "point", ellipse = FALSE, show.clust.cent = FALSE, palette = "jco", ggtheme = theme_classic())