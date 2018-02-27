# ------------------------ Load data ------------------------------------------#

genDir<-"D:\\quinonesa\\Simulation\\functionAprox\\"

agenDir<-paste(genDir,"ActPosTy1",sep='')

setwd(agenDir)

listAgen<-list.files()

filtList<-function(list,alpha,gamma,tau,neta,outbr)
{
  gamma<-c(0,0.5)
  gammaList<-lapply(gamma, paste,"gamma",sep="")
}
