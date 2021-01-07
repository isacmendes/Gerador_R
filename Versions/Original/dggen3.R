# Title     : TODO
# Objective : TODO
# Created by: Dell
# Created on: 17/10/2020

dggen<-function(MIN_PER_LAYER =1,
                MAX_PER_LAYER =1,
                MIN_LAYERS =3,
                MAX_LAYERS =8,
                EDGE_PROB =33,
                MAX_FAN_IN=1,
                MAX_FAN_OUT=1,
                plotGraph=FALSE){

  library(igraph)
  #derived constant
  MAX_NODES=MAX_PER_LAYER*MAX_LAYERS+2
  #data structures
  edges<-vector(mode="numeric")
  suc<-list()
  pre<-list()
  iM<-matrix(0,nrow=MAX_NODES,ncol=MAX_NODES)
  nodes = vector(mode='numeric');
  #aux function
  dfs<-function(start,end){
    found<-FALSE
    if (sum(iM[start,])!=0){ # successors
      if (end %in% which(iM[start,]==1)){
        found=TRUE
      } else
        for (i in which(iM[start,]==1)){
          if( found<-dfs(i,end))
            break
        }
    }
    found
  }


  #1-Initialize: sample number of layers
  nLayers<-sample(MIN_LAYERS:MAX_LAYERS,1)

  #2-set egdes and nodes from initial node
  new_nodes <- sample(MIN_PER_LAYER:MAX_PER_LAYER,1)
  for (k  in (1:new_nodes)){
      edges<-c(edges,1,1+k)
      iM[1,1+k]<-1
    }
  nodes<- 2:(1+new_nodes)

  #3-Generate nodes and edges for layers
  for (i in (1: nLayers)){
      print(i)
      #sample nodes in layer i
      new_nodes <- sample(MIN_PER_LAYER:MAX_PER_LAYER,1)
      #last node prior to new layer
      lastNode<-max(nodes)
      #nodes prior to new layer
      layerNodes<-nodes

      for (j  in layerNodes){
        nc<-1
        for (k  in (1:new_nodes)){
          if (runif (1) *100 < EDGE_PROB){
            nn <-lastNode+nc
            if ( (!dfs(j,nn))&                                             #(iM[j,nn] !=1)&
                 (sum(iM[,nn]) < MAX_FAN_IN)&
                 (sum(iM[j,]) < MAX_FAN_OUT)) {
              nc<-nc+1
              edges<-c(edges,j,nn)
              nodes<-c(nodes,nn)
              iM[j,nn]<-1
            }
          }
	    }
      }
  }

  #4-final node n+1: connect all nodes with no succs to end node
  lastNode<-max(nodes)
  s<-apply(iM[1:lastNode,],1,sum)
  noSuc<-which(s==0)
  endNode<-lastNode+1
  for(n in noSuc){
    edges<-c(edges,n,endNode)
    nodes<-c(nodes,n)
    iM[n,endNode]<-1
  }


  #5-return values
  iM<-iM[1:endNode,1:endNode]
  #generating successor list
  for (i in 1:lastNode){
    s<-which(iM[i,]==1)
    suc[[i]]<-s
  }
  suc[[endNode]]<-0
  #generating predecessor list
  for (i in 1:lastNode){
    p<-which(iM[,i]==1)
    pre[[i]]<-p
  }
  pre[[1]]<-0

  #tkplot graph
  if (plotGraph){
    g<-make_graph(edges=edges)
    tkplot(g,vertex.color='white')
  }
  #return structures
  r<-list(iM=iM,edges=edges,suc=suc,pre=pre)
}