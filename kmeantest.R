
library(cluster)
library(xlsx)
library(clue)
library(cba)
library(fpc)
library(arules)

iris<- read.csv("/Users/jas/Desktop/iris.csv")
irisNoClass<-iris
irisNoClass$class <- NULL
irisNoClass$X<- NULL




ClusterPurity <- function(clusters, classes) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

for(i in 1:4){

  KMeanResult<-kmeans(irisNoClass,i)
  kmedoidsResult<-pam(irisNoClass,i)
  claravalue<-clara(as.matrix(irisNoClass),k = i)
  ClaraAccuracy<-ClusterPurity(claravalue$clustering,iris$class)
  KmeanAccuracy<-ClusterPurity(KMeanResult$cluster,iris$class)
  KmeadoidsAccuracy<-ClusterPurity(kmedoidsResult$clustering,iris$class)
  NoOfCluster<-i
  DataSet<-c("Iris")
  Result<-data.frame(DataSet,NoOfCluster,ClaraAccuracy,KmeanAccuracy,KmeadoidsAccuracy)
  value<-rbind(Result,Result)
  
}



