# --------------------------------- Time intervals -------------------------------------------------#

projDir<-"d:/quinonesa/learning_models_c++/functionAprox/"

# libraries ---------------------------------------------------------------------------------------
source('d:/quinonesa/Dropbox/R_files/posPlots.R')
source(paste(scriptDir,"aesth_par.R",sep=""))
source(paste(scriptDir,"loadData.R",sep = ""))


# Load Data ---------------------------------------------------------------------------------------


# Define data to be loaded 

listPar<-c("gamma","tau","neta","outb")
listVal<-c(0.8,10,0,0)
param<-getParam(genDir,listparam = listPar,values = listVal)

# Load interval data for FIA from the raw data
# FIAtimeInt<-do.call(
#   rbind,lapply(
#     getFilelist(genDir,listPar,listVal)$FIA,
#     file2timeInter,interV=1001))

# Load FIA data from processed file

FIAtimeInt<-do.call(
  rbind,lapply(getFilelist(genDir,listPar,listVal)$FIA,fread))


# Load interval data for PIA from the raw data
# PIAtimeInt<-do.call(
#   rbind,lapply(
#     getFilelist(genDir,listPar,listVal)$PIA,
#     file2timeInter,interV=1001))

# Load PIA data from processed file

PIAtimeInt<-do.call(
  rbind,lapply(getFilelist(genDir,listPar,listVal)$PIA,fread))

# Load DP data from the raw data
# DPdataProb<-do.call(rbind,
#                     lapply(getFilelist(genDir,listPar,listVal)$DP,
#                            file2lastDP))

# Load DP data from processed file

DPdataprob<-do.call(
  rbind,lapply(getFilelist(genDir,listPar,listVal)$DP,fread))

# Plot the dynamics of VR choice -----------------------------------------------------------

FIAIntstats<-FIAtimeInt[,.(meanProb=mean(Type_choice.mean),
                           upIQR=fivenum(Type_choice.mean)[4],
                           lowIQR=fivenum(Type_choice.mean)[2])
                        ,by=.(Interv,Neta,Outbr,Tau,Gamma)]

PIAIntstats<-PIAtimeInt[,.(meanProb=mean(Type_choice.mean),
                           upIQR=fivenum(Type_choice.mean)[4],
                           lowIQR=fivenum(Type_choice.mean)[2])
                        ,by=.(Interv,Neta,Outbr,Tau,Gamma)]

par(plt=posPlot(numplotx = 2,idplotx = 1)+c(-0.05,-0.05,0,0),yaxt='s',las=1)
with(FIAIntstats,{
  plotCI(x=Interv,y=meanProb,
         ui = upIQR,li=lowIQR,
         pch=16,xlab='',ylab='',
         col=colboxes[match(Gamma,unique(Gamma))],
         sfrac=0.002,cex.axis=1.3)
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
})
with(DPdataProb,  
     {matlines(x = t(matrix(rep(max(FIAtimeInt$Interv)*c(0.75,1),
                                each=length(RV.V)),length(RV.V))),
               y=t(matrix(rep(probRV.V,2),length(RV.V))),
               lwd=2,lty = "dashed",
               col=colboxes[match(Gamma,unique(Gamma))])})

par(plt=posPlot(numplotx = 2,idplotx = 2)+c(-0.05,-0.05,0,0),
    new=TRUE,yaxt='s',xpd=TRUE)
with(PIAIntstats,{
  plotCI(x=Interv,y=meanProb,
         ui = upIQR,li=lowIQR,
         pch=16,xlab='',ylab='',
         col=colboxes[match(Gamma,unique(Gamma))],
         sfrac=0.002,cex.axis=1.3,yaxt='n')
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
  axis(side=4,cex.axis=1.3)
})

  
  
  