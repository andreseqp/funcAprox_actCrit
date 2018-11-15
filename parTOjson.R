# ------------------------ generate json files  ------------------------------------------ #

library("jsonlite")

source(paste(projDir,"\\loadData.R",sep = ""))

projDir<-"D:\\quinonesa\\learning_models_c++\\functAprox_actCrit"

simsDir<-"S:/quinonesa/Simulations/functionAprox/ActCrit/multRegr"

exedir<-paste(projDir,'/./RBF_funcA.exe',sep='')

fileName<-"parameters.json"


#test<-fromJSON(paste(codedir,"\\test.json",sep=""))



param<-list(totRounds=100000,ResProb=0.2,VisProb=0.2,
            ResProbLeav=0,VisProbLeav=1,negativeRew=-0.5,experiment=FALSE,
            inbr=0,outbr=0,trainingRep=30,
            alphaCrit=0.00001,alphaAct=0.00001,printGen=100,seed=1, 
            gammaRange=c(0,0.8),
            netaRange=c(FALSE,TRUE),mins=c(10,10),
            folder=simsDir)

param$visitors$Sp1$means<-c(66,20,40)
param$visitors$Sp1$sds<-rep(1,3)
param$visitors$Sp1$reward<-c(1,0.1)
param$visitors$Sp1$relAbun=1
param$residents$Sp1$means<-c(33,30,40)
param$residents$Sp1$sds<-rep(1,3)
param$residents$Sp1$relAbun=1
param$residents$Sp1$reward<-c(1,0.1)

set.seed(2)


setwd(simsDir)

rangSp<-c(1,2,3,4)
listfolders<-check_create.dir(simsdir,rep("LenRewNumSp",length(rangSp)),rangSp)

for (i in 1:4) {
  param$folder<-paste(simsDir,'/',listfolders[i],'/',sep='')
  for(newSp in 1:rangSp[i]){
    param$visitors[[newSp]]<-
      list(means=c(66,floor(runif(min = 0,max = 100,n = 2))),
           sds=c(10,rep(1,2)),relAbun=1,reward=c(1,0.1))
    names(param$visitors)[newSp]<-paste("Sp",newSp,sep = "")
    param$residents[[newSp]]<-
      list(means=c(33,floor(runif(min = 0,max = 100,n = 2))),
           sds=c(10,rep(1,2)),relAbun=1,reward=c(1,0.1))
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
  system(paste(exedir,
    gsub("\\","/",paste(simsDir,listfolders[i],fileName,sep="\\"),fixed=TRUE)
    ,sep = " "))
}
gsub(pattern = "\\",replacement = "/",simsdir,fixed=TRUE)

# system(paste(exedir,
#              gsub("\\","/",paste(simsdir,listfolders[1],fileName,sep="\\"),fixed=TRUE)
#              ,sep = " "))

