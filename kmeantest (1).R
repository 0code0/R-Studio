
library(cluster)
library(xlsx)
library(clue)
library(cba)
library(fpc)
library(arules)
library(caret)

iris<- read.csv("/Users/jas/Desktop/iris.csv")
irisNoClass<-iris
irisNoClass$class <- NULL
irisNoClass$X<- NULL




ClusterPurity <- function(clusters, classes) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}
DataSet<-c("Iris")

for(i in 1:10){

  KMeanResult<-c(kmeans(irisNoClass,i))
  kmedoidsResult<-c(pam(irisNoClass,i))
  claravalue<-c(clara(as.matrix(irisNoClass),k = i))
  
  ClaraAccuracy<-c(ClusterPurity(claravalue$clustering,iris$class))
  KmeanAccuracy<-c(ClusterPurity(KMeanResult$cluster,iris$class))
  KmeadoidsAccuracy<-c(ClusterPurity(kmedoidsResult$clustering,iris$class))
  NoOfCluster<-i
  Result<-data.frame(DataSet,NoOfCluster,ClaraAccuracy,KmeanAccuracy,KmeadoidsAccuracy)
  
  print(Result)  
  
  }   

  