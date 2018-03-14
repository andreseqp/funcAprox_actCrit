# ------------------------ generate json files  ------------------------------------------ #

library("jsonlite")

codedir<-"D:\\quinonesa\\learning_models_c++\\functionAprox"

simsdir<-"D:\\quinonesa\\Simulation\\functionAprox"

fileName<-"parameters.json"


test<-fromJSON(paste(codedir,"\\test.json",sep=""))

param<-list(totRounds=80000,ResReward=10,VisReward=10,ResProb=0.2,VisProb=0.2,
            ResProbLeav=0,VisProbLeav=1,negativeRew=-10,experiment=FALSE,
            inbr=0,outbr=0,trainingRep=30,
            alphaT=0.00001,printGen=1,seed=1, gammaRange=c(0,0.5,0.8),
            tauRange=c(2,5,10),netaRange=c(0,0.5),mins=c(10,10),
            folder=simsdir)

param$visitors$Sp1$means<-c(30,20,40,40,40,40,40,40)
param$visitors$Sp1$sds<-rep(3,8)
param$visitors$Sp1$probs<-rep(1,3)
param$residents$Sp1$means<-c(20,30,40,40,40,40,40,40)
param$residents$Sp1$sds<-rep(3,8)
param$residents$Sp1$probs<-c(0,1,1)

setwd(simsdir)
rangOut<-c(0,0.1,0.2)

listfolders<-paste("out_",rangOut,sep = "")

currFolders<-lapply(listfolders,dir.exists)

if(sum(currFolders>0)){
  warning("At least one of the folders already exists \n Please check",immediate. = TRUE)
  cbind(listfolders,currFolders)
  ans<-readline("Want to continue?")
  if(substr(ans, 1, 1) == "y"){
    lapply(listfolders,dir.create)
  }
}else{
  lapply(listfolders,dir.create)
}

for (i in 1:3) {
  param$outbr <-rangOut[i]
  param$folder<-paste(simsdir,'\\',listfolders[i],'\\',sep='')
  outParam<-toJSON(param,auto_unbox = TRUE,pretty = TRUE)
  if(file.exists(paste(param$folder,fileName,sep = '')))
  {
    currFile<-fromJSON(paste(param$folder,fileName,sep = ''))
    if(sum(unlist(currFile)!=unlist(param))>0)
    {
      warning("You are erasing old files!! n\ Check first!!!",immediate. = TRUE)
      print("OLD value")
      print(unlist(currFile)[unlist(currFile)!=unlist(param)])
      print("NEW value")
      print(unlist(param)[unlist(currFile)!=unlist(param)])
      ans<-readline("Want to continue?")
      if(substr(ans, 1, 1) == "y"){
        write(outParam,paste(simsdir,listfolders[i],fileName,sep="\\"))
      }
    }
  }
  else
  {
    write(outParam,paste(simsdir,listfolders[i],fileName,sep="\\"))
  }
}


