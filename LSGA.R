library(igraph)
library(ape)
library(ade4)

data<-read.graph("Krate.gml",format = c("gml"))# Data read

KrateEdges<-get.data.frame(data,what = c("edges"))# From to edges
KrateEdges

AllNodes<-union(KrateEdges$from,KrateEdges$to)# Total no of nodes in a graph
AllNodes


KrateEachNeigherEdge<-lapply(1:length(AllEdges),function(i){neighbors(data,AllNodes[i])})# neighbour of each node

KrateEachNeigherEdge
#lapply(1:length(KrateEdges$from), function(i){KrateEdges$from[i]})


KrateEdges_DataFrame<-data.frame(KrateEdges$from,KrateEdges$to) # create data frame From to nodes

# Find Common Neighbour
CommonNeighbour<-lapply(1:length(KrateEdges_DataFrame$KrateEdges.from), function(i){
  
  KrateEdges_DataFrame[i,]
  value1<-KrateEdges_DataFrame[i,][[1]]
  value2<-KrateEdges_DataFrame[i,][[2]]
  intersect(unlist(KrateEachNeigherEdge[value1]),unlist(KrateEachNeigherEdge[value2]))
  
  
})
# Ademic/Adar Score
CommonNeighbour_Sum<-lapply(1:length(CommonNeighbour), function(i){
  
  1/log10(sum(degree(data,unlist(CommonNeighbour[i]))))# bai log layaaa
  
  
})

CommonNeighbour_Sum




deletedEdge<-which(CommonNeighbour_Sum %in% max(unlist(CommonNeighbour_Sum)))#max Ademic Score for delete edge
deletedEdge

update<-delete_edges(data,edges = deletedEdge)

tkplot(update)

cl <- clusters(update)$membership
modularity(data, cl)

---------------------------------------------------------------------------------------------------------------

  data<-read.graph("Krate.gml",format = c("gml"))# Data read

KrateEdges<-get.data.frame(data,what = c("edges"))# From to edges


AllNodes<-union(KrateEdges$from,KrateEdges$to)# Total no of nodes in a graph

  
  # Test Area ...........................................................
  
  KrateEachNeigherEdge<-lapply(1:length(AllEdges),function(i){neighbors(data,AllNodes[i])})# neighbour of each node
  
  KrateEachNeigherEdge
  #lapply(1:length(KrateEdges$from), function(i){KrateEdges$from[i]})
  
  
  KrateEdges_DataFrame<-data.frame(KrateEdges$from,KrateEdges$to) # create data frame From to nodes
  
  # Find Common Neighbour
  CommonNeighbour<-lapply(1:length(KrateEdges_DataFrame$KrateEdges.from), function(i){
    
    KrateEdges_DataFrame[i,]
    value1<-KrateEdges_DataFrame[i,][[1]]
    value2<-KrateEdges_DataFrame[i,][[2]]
    intersect(unlist(KrateEachNeigherEdge[value1]),unlist(KrateEachNeigherEdge[value2]))
    
    
  })
  # Ademic/Adar Score
  CommonNeighbour_Sum<-lapply(1:length(CommonNeighbour), function(i){
    
    1/log10(sum(degree(data,unlist(CommonNeighbour[i]))))# bai log layaaa
    
    
  })
  
  CommonNeighbour_Sum
  
  
  
  
  deletedEdge<-which(CommonNeighbour_Sum %in% max(unlist(CommonNeighbour_Sum)))#max Ademic Score for delete edge
  deletedEdge
  
  data<-delete_edges(data,edges = deletedEdge)
  
  tkplot(data)
  
  c1<-clusters(data)$membership
  modularity(newdata,c1)  
  newdata<-data
  data
  