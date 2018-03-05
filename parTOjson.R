# ------------------------ generate json files  ------------------------------------------ #

library("jsonlite")

codedir<-"D:\\quinonesa\\learning_models_c++\\functionAprox"

simsdir<-"D:\\quinonesa\\Simulation\\functionAprox"

param<-fromJSON(paste(codedir,"\\test.json",sep=""))

param$totRounds<-30000
param$outbr <-0
param$trainingRep<-10
param$alphaT<-0.0001
param$printGen <-100
basefolder<-"D:\\quinonesa\\Simulation\\functionAprox\\"

setwd(simsdir)
rangOut<-c(0,0.1,0.2)

listfolders<-paste("out_",rangOut,sep = "")

lapply(listfolders,dir.create)

for (i in 1:3) {
  param$outbr <-rangOut[i]
  param$folder<-paste(basefolder,listfolders[i],'\\',sep='')
  outParam<-toJSON(param,auto_unbox = TRUE,pretty = TRUE)
  write(outParam,paste(simsdir,"\\",listfolders[i],"\\parameters.json",sep=""))
}





