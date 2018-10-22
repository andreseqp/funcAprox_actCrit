RBF<-function(x,xCenter,sigSq){
  return(exp(-(x-xCenter)^2/(2*sigSq)))
}

totRBF<-function(x,xCenter,sigSq,featWeig){
  tmp<-sapply(xCenter, RBF,x=x,sigSq=sigSq)
  tmp<-apply(tmp, 1, FUN=crossprod,y=featWeig)
  #tmp<-apply(tmp,1,FUN = sum)
  return(tmp)
}

crossprod(seq(1,3,1),seq(1,30,10))

totRBF(rangx,rangCent,rangSigSq)

sum(sapply(rangCent,RBF,rangx,rangSigSq))

numfeat<-3
rangSigSq<-100
rangCent<-seq(0,100,length=numfeat+2)[2:(numfeat+1)]
rangx<-seq(0,100,length=1000)
featW<-seq(0.5,4,length=numfeat)
  runif(numfeat,max=1)


plot(totRBF(rangx,rangCent,rangSigSq,featW)~rangx,type='l',col=1,
     xlab="x",ylab="response",ylim=c(0,5))
for(i in 1:numfeat){
  lines(RBF(rangx,rangCent[i],rangSigSq)~rangx,col=i+1)  
}

featLoc<-matrix(data = NA,nrow = 729,ncol = 6)

centers<-c(25,50,75)
count<-0
for(a in centers){
  for(b in centers){
    for(c in centers){
      for(d in centers){
        for(e in centers){
          for(f in centers){
            count<-count+1
            featLoc[count,]<-c(a,b,c,d,e,f)
          }
        }
      }
    }
  }
}

state<-matrix(c(runif(6,0,100)),nrow = 1)
state<-matrix(rep(0,6),nrow=1)

RBF(state,featLoc[1,],sigSq = 100)



distM<-dist(rbind(state,featLoc))[1:729]
distM[1:729]^2
distM[730:2*729]


