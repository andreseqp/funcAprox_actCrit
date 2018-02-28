# ------------------------ Load data ------------------------------------------#

library("data.table")

genDir<-"D:\\quinonesa\\Simulation\\functionAprox\\"

agenDir<-paste(genDir,"ActPosTy1",sep='')

setwd(agenDir)

listAgen<-list.files()

listTrain<-grep('IndTrain',listAgen,value = TRUE)

rawdata<-do.call(rbind,lapply(listTrain, fread))

filtList<-function(list,alpha,gamma,tau,neta,outbr)
{
  gamma<-c(0,0.5)
  gammaList<-lapply(gamma, paste,"gamma",sep="")
}

with(rawdata[(Gamma==0.8 & Tau==10) & Training==1],{
  plot(x=Age,y=Height_1)  
})



