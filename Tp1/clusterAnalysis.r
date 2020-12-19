Iris <- read.csv("D:/Master2/S1/Ingénierie des connaissances -- IC/Tp/Tp1/data/iris.csv", header = TRUE)
head(Iris)
#Scatter Plot
plot(petal_length ~ species,Iris)