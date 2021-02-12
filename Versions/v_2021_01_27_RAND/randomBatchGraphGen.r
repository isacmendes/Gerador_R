randomBatchGraphGen<-function(nSamples=500){
#Version 2: 2021/01/21
#Lote único para verificar impacto do número de vertices

#uses dggen to generate batches of digraphs 
  source('Versions/v_2021_01_27_RAND/dggenER.r')
# Factor parameters 
  #nodeFactorSet<-c(16,32,48,64,80,96,112,128)
  # nodeFactorSet<-16:100
  # LayerFactorSet<-c(4:8)
  # maxFanFactorSet<-c(2:5)
  # discRateFactorSet<-c(0:50)
  # percNegFactorSet<-c(0:50)
  # cpMultFactorSet<-c(1,1.5,2)

  nodeFactorSet<-16:80
  #LayerFactorSet<-c(4:8) #Definido depois do sorteio de 'nodeFactorSet'
  #nlayers<-2
  maxFanFactorSet<-c(2:3)
  discRateFactorSet<-c(0:20)
  percNegFactorSet<-c(10:90)
  cpMultFactorSet<-c(1,2)
  
# Range parameters
  minDur=5
  maxDur=10
  minCf=-50
  maxCf=100
  cpMult=1.5
  maxCfPos=100
  minCfNeg=-50
  cfPos=1:maxCfPos
  cfNeg=minCfNeg:(-1)

# dggen graph parameters
  nNodes =8
  nLayers =2
  maxFan  =3
  edgeProb =33
  plotGraph=FALSE

#aux functions  
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

  actDur<-function(n){
    #returns activity duration vector
    d<-sample(minDur:maxDur,n,replace = T)
    d[1]=0
    d[n]=0
    d
  }
  
  cFVal<-function(n,negFactor){
    #returns CF vector
    negFactor<-negFactor/100
    r<-vector(length=n)
    r[1]<-0
    r[n]<-0
    for ( i in 2:(n-1)){
      if (rbinom(1,1,negFactor)){
        r[i]<-sample(cfNeg,1)}
      else{
        r[i]<-sample(cfPos,1)
      }
    }
    r  
  }    
  
  cpDur<-function(suc){
    #return cp duration
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
    
    #returns cp duration
    n<-length(suc)
    est<-vector (mode="numeric",length=n)
    eft<-vector (mode="numeric",length=n)
    
    est<-cpmf(1,est)
    r<-est[n]
    r
  }
  
#print routine    
printToFile<-function(namedir,fIndex,suc,d,cF){
  nl<-length(suc)
  #1-file name
  outFile<-paste(namedir,'/',fIndex,'.tpf',sep="",collapse = "")
  outcon<-file(outFile,open="wt")
  
  #2-print first line
  est<-cpDur(suc)
  #r<-c(nl,est*(1+cpMult))
  r<-c(nNodes+2, nlayers, fan, discRate, percNeg, cpMult, est*(1+cpMult))
  write(r,file=outcon,append=TRUE, sep = " ", ncolumns=8)
  
  #read writeand suc
  for (i in 1:nl){
    r<-c(i,length(suc[[i]]),suc[[i]])
    write(r,file=outcon,append=TRUE, sep = " ",ncolumns = length(r))
  }
 
  
  for (i in 1:nl){
     r<-(c(i,d[i],cF[i]))
     write(r,file=outcon,append=TRUE, sep = "  ")
  }
  close(outcon)
}
  
#main
  tini<-proc.time()
  date.time <- format(Sys.time(), "%y%m%d%H%M")
  namedir<-paste('Samples/Random/LoteAleatorio_',date.time,sep="",collapse = "")
  print(namedir)              
  dir.create(namedir,recursive = TRUE)
  r<-data.frame(matrix(ncol=7,nrow=nSamples))
  rNames<-c('ind','nNodes','nLayers','fan','discRate','percNeg','cpMult')
  colnames(r)<-rNames
  
    for ( i in 1:nSamples){
        if(i %% 10 ==0 ) print(c('i=',i))
        nNodes<-sample(nodeFactorSet,1,replace = T)
        #nlayers<-sample(LayerFactorSet,1,replace =T)
        nlayers<-sample(c(2:(nNodes-1)),1,replace =T)
        fan<-sample (maxFanFactorSet,1,replace=T)
        discRate<-sample(discRateFactorSet,1,replace = T)
        percNeg<-sample(percNegFactorSet,1,replace = T)
        cpMult<-sample(cpMultFactorSet,1,replace = T)
        r[i,]<-c(i,nNodes,nlayers,fan,discRate,percNeg,cpMult)
        #generate single graph
        g<-dggenER(nNodes=nNodes,nLayers=nlayers,maxFan=fan,edgeProb=99,nodeMult=16)
        n<-length(g$suc)
        d<-actDur(n)
        cF<-cFVal(n,percNeg)
        #print graph representation .tpf
        printToFile(namedir, fIndex=i,suc=g$suc,d,cF)
      }
  
  tproc<-proc.time()-tini
  r
}
randomBatchGraphGen(1000)