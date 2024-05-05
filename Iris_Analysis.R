rm(list=ls())
set.seed(100)

library(kknn)
library(factoextra)
library(ggplot2)

data=read.table("M:/OMSA/ISYE6501/HW2/iris.txt", stringsAsFactor=FALSE,header=TRUE)


#Plot to look for best combination of predictors
ggplot(data,aes(Sepal.Length,Sepal.Width,color=Species))+geom_point(size=6)
ggplot(data,aes(Petal.Length,Petal.Width,color=Species))+geom_point(size=6)
ggplot(data,aes(Sepal.Length,Petal.Length,color=Species))+geom_point(size=6)
ggplot(data,aes(Sepal.Length,Petal.Width,color=Species))+geom_point(size=6)
ggplot(data,aes(Petal.Length,Sepal.Width,color=Species))+geom_point(size=6)
ggplot(data,aes(Petal.Width,Sepal.Width,color=Species))+geom_point(size=6)

#create database without response column
iris_data=data[,1:4]

#convert species in numbers
species=c("setosa"=1,"versicolor"=2,"virginica"=3)
data$Species=species[data$Species]

#Finding Best number of Clusters
fviz_nbclust(iris_data,kmeans,method="wss")

#perform clustering
clustering=kmeans(iris_data,centers=3,nstart=10)

#saving predicted cluster values
predicted_species=clustering$cluster

#Checking Accuracy
accuracy=sum(predicted_species==data[,5])/nrow(data)
accuracy


#Plot to look for best combination of predictors
ggplot(data,aes(Sepal.Length,Sepal.Width,color=Species))

       