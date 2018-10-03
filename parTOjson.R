# ------------------------ generate json files  ------------------------------------------ #

library("jsonlite")

projDir<-"D:\\quinonesa\\learning_models_c++\\functAprox_actCrit"

simsDir<-"S:/quinonesa/Simulations/functionAprox/ActCrit"

exedir<-paste(projDir,'/./FunctionAproxSarsa.exe',sep='')

fileName<-"parameters.json"


#test<-fromJSON(paste(codedir,"\\test.json",sep=""))



param<-list(totRounds=20000,ResReward=10,VisReward=10,ResProb=0.2,VisProb=0.2,
            ResProbLeav=0,VisProbLeav=1,negativeRew=-10,experiment=FALSE,
            inbr=0,outbr=0,trainingRep=30,
            alphaCrit=0.00001,alphaAct=0.00001,printGen=1,seed=1, 
            gammaRange=c(0,0.8),
            tauRange=c(5,10),netaRange=c(0,0.5),mins=c(10,10),
            folder=simsDir)

param$visitors$Sp1$means<-c(30,20,40,40,40,40,40,40)
param$visitors$Sp1$sds<-rep(1,8)
param$visitors$Sp1$probs<-rep(1,3)
param$visitors$Sp1$relAbun=1
param$residents$Sp1$means<-c(20,30,40,40,40,40,40,40)
param$residents$Sp1$sds<-rep(1,8)
param$residents$Sp1$probs<-c(1,1,1)
param$residents$Sp1$relAbun=1

set.seed(2)


setwd(simsDir)


check_create.dir<-function(folder,param,values){
  listfolders<-paste(param,values,"_",sep = "")  
  currFolders<-lapply(listfolders,dir.exists)
  if(sum(currFolders>0)){
    warning("At least one of the folders already exists \n Please check",immediate. = TRUE)
    print(cbind(listfolders,currFolders))
    ans<-readline("Want to continue?")
    if(substr(ans, 1, 1) == "y"){
      lapply(listfolders,dir.create)
      return(listfolders)
    }
    else{
      return(listfolders)
    }
  }else{
    lapply(listfolders,dir.create)
    return(listfolders)
  }
}

rang<-c(1,2,3,4,5,6,7,8,9)
listfolders<-check_create.dir(simsdir,rep("rdNumSP",length(rang)),rang)

for (i in 1:1) {
  param$folder<-paste(simsDir,'/',listfolders[i],'/',sep='')
  for(newSp in 1:rang[i]){
    param$visitors[[newSp]]<-
      list(means=c(floor(runif(min = 10,max = 50,n = 2)),
                   floor(runif(min = 0,max = 50,n = 6))),
           sds=rep(1,8),probs=rep(1,3),relAbun=1)
    names(param$visitors)[newSp]<-paste("Sp",newSp,sep = "")
    param$residents[[newSp]]<-
      list(means=c(floor(runif(min = 10,max = 50,n = 2)),
                   floor(runif(min = 0,max = 50,n = 6))),
           sds=rep(1,8),probs=rep(1,3),relAbun=1)
    names(param$residents)[newSp]<-paste("Sp",newSp,sep = "")
  }
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
        write(outParam,paste(simsDir,listfolders[i],fileName,sep="\\"))
      }
    }
  }
  else
  {
    write(outParam,paste(simsDir,listfolders[i],fileName,sep="\\"))
  }
  # system(paste(exedir,
  #   gsub("\\","/",paste(simsdir,listfolders[i],fileName,sep="\\"),fixed=TRUE)
  #   ,sep = " "))
}
gsub(pattern = "\\",replacement = "/",simsdir,fixed=TRUE)

# system(paste(exedir,
#              gsub("\\","/",paste(simsdir,listfolders[1],fileName,sep="\\"),fixed=TRUE)
#              ,sep = " "))

