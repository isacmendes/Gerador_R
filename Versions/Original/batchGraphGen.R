# Title     : TODO
# Objective : TODO
# Created by: Dell
# Created on: 17/10/2020

batchGraphGen<-function(batchSize=1){
#uses dggen to generate batches of digraphs
  source('dggen3.r')
#node parameters
  MIN_DUR=5
  MAX_DUR=10
  MIN_CF=-100
  MAX_CF=100
  CP_MULT=1

# dggen graph parameters
  #Lote teste
  MIN_PER_LAYER = 32
  MAX_PER_LAYER = 32
  MIN_LAYERS = 32
  MAX_LAYERS = 32
  EDGE_PROB =20
  MAX_FAN_IN=1030
  MAX_FAN_OUT=1030
  plotGraph=TRUE

  #MIN_PER_LAYER = 16
  #MAX_PER_LAYER = 32
  #MIN_LAYERS = 16
  #MAX_LAYERS = 32
  #EDGE_PROB = 33
  #MAX_FAN_IN=1000
  #MAX_FAN_OUT=1000
  #plotGraph=TRUE

  # Lotes 1 e 2
  #MIN_PER_LAYER = 4
  #MAX_PER_LAYER = 8
  #MIN_LAYERS = 4
  #MAX_LAYERS = 8
  #EDGE_PROB = 33
  #MAX_FAN_IN=2
  #MAX_FAN_OUT=3
  #plotGraph=TRUE

  # Lotes 3 e 4
  #MIN_PER_LAYER = 16
  #MAX_PER_LAYER = 32
  #MIN_LAYERS = 16
  #MAX_LAYERS = 32
  #EDGE_PROB = 33
  #MAX_FAN_IN=3
  #MAX_FAN_OUT=5
  #plotGraph=TRUE

printToFile<-function(fLabel,fIndex,suc){
  #aux function
  cpmf<-function(s,est){
    #find early start and early finish times
     eft[s]<-est[s]+d[s]
    if ((suc[[s]][1]!=0)){
      for (i in suc[[s]]){
        if (est[i] < eft[s]){
          est[i]<-eft[s]
        }
        est<-cpmf(i,est)
      }
    }
    est
  }

  #file name
  #dir.create('Lote')
  outFile<-paste('Lote/',fLabel,fIndex,'.tpf',sep="",collapse = "")
  outcon<-file(outFile,open="wt")

  nl<-length(suc)

  #set node duration and cash flow
  d<-sample(MIN_DUR:MAX_DUR,nl,replace = T)
  d[1]=0
  d[nl]=0
  cf<-sample(MIN_CF:MAX_CF,nl,replace = T)
  cf[1]=0
  cf[nl]=0

  #cp duration
  est<-vector (mode="numeric",length=nl)
  eft<-vector (mode="numeric",length=nl)
  est<-cpmf(1,est)

  #print first line
  r<-c(nl,est[nl]*(1+CP_MULT))
  write(r,file=outcon,append=TRUE, sep = " ")
  #read and print suc
  for (i in 1:nl){
    r<-(c(i,length(suc[[i]]),suc[[i]]))
    #print(r)
    write(r,file=outcon, ncolumns=1000, append=TRUE, sep = " ")
  }

  for (i in 1:nl){
    r<-(c(i,d[i],cf[i]))
    write(r,file=outcon, ncolumns=1000, append=TRUE, sep = " ")
  }
  close(outcon)
}

#main
  tini<-proc.time()
  for (i in 1:batchSize){
    g<-dggen(MIN_PER_LAYER,
             MAX_PER_LAYER,
             MIN_LAYERS,
             MAX_LAYERS,
             EDGE_PROB,
             MAX_FAN_IN,
             MAX_FAN_OUT,
             plotGraph
    )
    d<-vector(length = length(g$suc))
    printToFile(fLabel='dg',fIndex=i,suc=g$suc)
  }
  tproc<-proc.time()-tini
  tproc
}

batchGraphGen(1)
