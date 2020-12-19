library("dbscan")
m <- read.csv("D:/Master2/S1/Ingénierie des connaissances -- IC/Tp/Tp1/data/iris.csv", header = TRUE)

head(m)
m1 <- m[, c(1, 2, 3, 4)]
head(m1)
db <- dbscan(m1, eps = 0.4, MinPts = 4)
## Warning in dbscan(m1, eps = 0.4, MinPts = 4): converting argument MinPts
## (fpc) to minPts (dbscan)!
print(db)
hullplot(m1, db$cluster)