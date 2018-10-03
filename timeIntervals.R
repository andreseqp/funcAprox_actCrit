# --------------------------------- Time intervals -------------------------------------------------#

projDir<-"d:/quinonesa/learning_models_c++/functAprox_actCrit//"
simsDir<-"s:/quinonesa/Simulations/functionAprox/ActCrit/"

# libraries ---------------------------------------------------------------------------------------
source('d:/quinonesa/Dropbox/R_files/posPlots.R')
source(paste(projDir,"aesth_par.R",sep=""))
source(paste(projDir,"loadData.R",sep = ""))
library('plotrix')

# Load Data ---------------------------------------------------------------------------------------

setwd(simsDir)

# Define data to be loaded 

(listPar<-c(rep("rdNumSP",9)))
(listVal<-c(1,2,3,4,5,6,7,8,9))
param<-getParam(simsDir,listparam = listPar,values = listVal)

diffJsons(param[1],param[3])

list.files(simsDir)

# Load interval data for FIA from the raw data
FIAtimeInt<-rbindlist(lapply(
    grep("Ty1",getFilelist(simsDir,listPar,listVal)$FIA,value = TRUE),
    file2timeInterValue,interV=501))


# Load FIA data from processed file

# getFilelist(simsDir,listPar,listVal)$FIA

# FIAtimeInt<-do.call(
#   rbind,lapply(getFilelist(projDir,listPar,listVal)$FIA,fread))



# Load interval data for PIA from the raw data
# PIAtimeInt<-rbindlist(lapply(
#     getFilelist(simsDir,listPar,listVal)$PIA,
#     file2timeInter,interV=1001))

# Load PIA data from processed file

# PIAtimeInt<-do.call(
#   rbind,lapply(getFilelist(genDir,listPar,listVal)$PIA,fread))

# Average value and preference -------------------------------------------------

extpar<-listPar[1]

FIAIntValstats<-FIAtimeInt[,.(meanVal=mean(value.m),
                                   upIQRVal=fivenum(value.m)[4],
                                   lowIQRVal=fivenum(value.m)[2],
                                   meanPref=mean(preference.m),
                                   upIQRPref=fivenum(preference.m)[4],
                                   lowIQRPref=fivenum(preference.m)[2],
                                   meanVal.sd=mean(value.sd),
                                   upIQRVal.sd=fivenum(value.sd)[4],
                                   lowIQRVal.sd=fivenum(value.sd)[2],
                                   meanPref.sd=mean(preference.sd),
                                   upIQRPref.sd=fivenum(preference.sd)[4],
                                   lowIQRPref.sd=fivenum(preference.sd)[2],
                                   meanChoice=mean(type_choice.m),
                                   upIQRChoice=fivenum(type_choice.m)[4],
                                   lowIQRChoice=fivenum(type_choice.m)[2])
  ,by=.(Interv,Neta,Gamma,option,AlphaCri,
      AlphaAct,get(extpar))]
setnames(FIAIntValstats,'get',extpar)

FIAIntValstats[,posit:=(get(extpar)-2)*0.06]

# Plot the dynamics of value for different options ----------------------------

idextPar<-1


par(plt=posPlot(numplotx = 3,idplotx = 1)+c(-0.05,-0.05,0,0),yaxt='s',
    las=1,xaxt='s')
with(FIAIntValstats[(get(extpar)==listVal[idextPar]&Neta==0)&Gamma==0],{
  plotCI(x=Interv,y=meanVal,
         ui = upIQRVal,li=lowIQRVal,
         pch=16,xlab='',ylab='',
         col=coloptions[match(option,sort(unique(option)))],
         # pch=match(get(extpar),unique(get(extpar)))),
         sfrac=0.002,cex.axis=1.3)
  # legend('bottomright',col =coloptions,legend = sort(unique(option)),pch =20,
  #        ncol = 3,cex = 0.8)
  text(x=par('usr')[1]+0.5*(par('usr')[2]-par('usr')[1]),
       y=par('usr')[3]+0.2*(par('usr')[4]-par('usr')[3]),
       label = "no future - no punishment")
})

par(plt=posPlot(numplotx = 3,idplotx = 2)+c(-0.05,-0.05,0,0),yaxt='s',
    las=1,xaxt='s',new=TRUE)
with(FIAIntValstats[(get(extpar)==listVal[idextPar]&Neta==0)&Gamma==0.8],{
  plotCI(x=Interv,y=meanVal,
         ui = upIQRVal,li=lowIQRVal,
         pch=16,xlab='',ylab='',
         col=coloptions[match(option,sort(unique(option)))],
         # pch=match(get(extpar),unique(get(extpar)))),
         sfrac=0.002,cex.axis=1.3)
  # legend('topleft',col =coloptions,legend = sort(unique(option)),pch =20,
  #        ncol = 3,cex = 0.8)
  text(x=par('usr')[1]+0.5*(par('usr')[2]-par('usr')[1]),
       y=par('usr')[3]+0.2*(par('usr')[4]-par('usr')[3]),
       label = "future")
})

par(plt=posPlot(numplotx = 3,idplotx = 3)+c(-0.05,-0.05,0,0),yaxt='s',
    las=1,xaxt='s',new=TRUE)
with(FIAIntValstats[(get(extpar)==listVal[idextPar]&Neta==1)&Gamma==0],{
  plotCI(x=Interv,y=meanVal,
         ui = upIQRVal,li=lowIQRVal,
         pch=16,xlab='',ylab='',
         col=coloptions[match(option,sort(unique(option)))],
         # pch=match(get(extpar),unique(get(extpar)))),
         sfrac=0.002,cex.axis=1.3)
  legend('bottomright',col =coloptions,legend = sort(unique(option)),pch =20,
         ncol = 3,cex = 0.8)
  text(x=par('usr')[1]+0.5*(par('usr')[2]-par('usr')[1]),
       y=par('usr')[3]+0.2*(par('usr')[4]-par('usr')[3]),
       label = "punishment")
})


# # variation in value within simulations ----------------------------------------
# par(plt=posPlot(numplotx = 3,idplotx = 1)+c(-0.05,-0.05,0,0),yaxt='s',
#     las=1,xaxt='s')
# with(FIAIntValstats[(get(extpar)==listVal[idextPar]&Neta==0)&Gamma==0],{
#   plotCI(x=Interv,y=meanVal.sd,
#          ui = upIQRVal.sd,li=lowIQRVal.sd,
#          pch=16,xlab='',ylab='',
#          col=coloptions[match(option,sort(unique(option)))],
#          sfrac=0.002,cex.axis=1.3,yaxt='s')
#   # axis(side=4,cex.axis=1.3)
# })
# 
# par(plt=posPlot(numplotx = 3,idplotx = 2)+c(-0.05,-0.05,0,0),yaxt='s',
#     las=1,xaxt='s',new=TRUE)
# with(FIAIntValstats[(get(extpar)==listVal[idextPar]&Neta==0)&Gamma==0.8],{
#   plotCI(x=Interv,y=meanVal.sd,
#          ui = upIQRVal.sd,li=lowIQRVal.sd,
#          pch=16,xlab='',ylab='',
#          col=coloptions[match(option,sort(unique(option)))],
#          sfrac=0.002,cex.axis=1.3,yaxt='s')
#   # axis(side=4,cex.axis=1.3)
# })
# 
# par(plt=posPlot(numplotx = 3,idplotx = 3)+c(-0.05,-0.05,0,0),yaxt='s',
#     las=1,xaxt='s',new=TRUE)
# with(FIAIntValstats[(get(extpar)==listVal[idextPar]&Neta==1)&Gamma==0],{
#   plotCI(x=Interv,y=meanVal.sd,
#          ui = upIQRVal.sd,li=lowIQRVal.sd,
#          pch=16,xlab='',ylab='',
#          col=coloptions[match(option,sort(unique(option)))],
#          sfrac=0.002,cex.axis=1.3,yaxt='s')
#   # axis(side=4,cex.axis=1.3)
# })
# Dynamics of preference -------------------------------------------------------

par(plt=posPlot(numplotx = 3,idplotx = 1),yaxt='s',
    las=1,xaxt='s')
with(FIAIntValstats[(get(extpar)==listVal[idextPar]&Neta==0)&Gamma==0],{
  plotCI(x=Interv+(match(option,unique(option))-1)*0.08,y=meanPref,
         ui = upIQRPref,li=lowIQRPref,
         pch=16,xlab='',ylab='',
         col=coloptions[match(option,sort(unique(option)))],
         sfrac=0.002,cex.axis=1.3)
  lines(x=c(0,max(Interv)),y=c(0,0),col='grey')
  # legend('topleft',col =coloptions,legend = sort(unique(option)),pch =20,
  #        ncol = 3,cex = 0.8)
  text(x=par('usr')[1]+0.5*(par('usr')[2]-par('usr')[1]),
       y=par('usr')[3]+0.2*(par('usr')[4]-par('usr')[3]),
       label = "no future - no punishment")
})
par(plt=posPlot(numplotx = 3,idplotx = 2),yaxt='s',
    las=1,xaxt='s',new=TRUE)
with(FIAIntValstats[(get(extpar)==listVal[idextPar]&Neta==0)&Gamma==0.8],{
  plotCI(x=Interv+(match(option,unique(option))-1)*0.08,y=meanPref,
         ui = upIQRPref,li=lowIQRPref,
         pch=16,xlab='',ylab='',
         col=coloptions[match(option,sort(unique(option)))],
         sfrac=0.002,cex.axis=1.3)
  lines(x=c(0,max(Interv)),y=c(0,0),col='grey')
  # legend('topleft',col =coloptions,legend = sort(unique(option)),pch =20,
  #        ncol = 3,cex = 0.8)
  text(x=par('usr')[1]+0.5*(par('usr')[2]-par('usr')[1]),
       y=par('usr')[3]+0.2*(par('usr')[4]-par('usr')[3]),
       label = "future")
})
par(plt=posPlot(numplotx = 3,idplotx = 3),yaxt='s',
    las=1,xaxt='s',new=TRUE)
with(FIAIntValstats[(get(extpar)==listVal[idextPar]&Neta==1)&Gamma==0],{
  plotCI(x=Interv+(match(option,unique(option))-1)*0.08,y=meanPref,
         ui = upIQRPref,li=lowIQRPref,
         pch=16,xlab='',ylab='',
         col=coloptions[match(option,sort(unique(option)))],
         sfrac=0.002,cex.axis=1.3)
  lines(x=c(0,max(Interv)),y=c(0,0),col='grey')
  legend('topleft',col =coloptions,legend = sort(unique(option)),pch =20,
         ncol = 3,cex = 0.8)
  text(x=par('usr')[1]+0.5*(par('usr')[2]-par('usr')[1]),
       y=par('usr')[3]+0.2*(par('usr')[4]-par('usr')[3]),
       label = "punishment")
})

# Dynamics of choice -----------------------------------------------------------


par(plt=posPlot(numplotx = 3,idplotx = 1),yaxt='s',
    las=1,xaxt='s')
with(FIAIntValstats[(get(extpar)==listVal[idextPar]&Neta==0)&Gamma==0],{
  plotCI(x=Interv+(match(option,unique(option))-1)*0.08,
         y=meanChoice,
         ui = upIQRChoice,li=lowIQRChoice,
         pch=16,xlab='',ylab='',
         col=coloptions[match(option,sort(unique(option)))],
         sfrac=0.002,cex.axis=1.3,yaxt='s')
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
  # legend('topleft',col =coloptions,legend = unique(option),pch =20,
  #        ncol = 3,cex = 0.8)
  text(x=par('usr')[1]+0.5*(par('usr')[2]-par('usr')[1]),
       y=par('usr')[3]+0.2*(par('usr')[4]-par('usr')[3]),
       label = "no future - no punishment")
})

par(plt=posPlot(numplotx = 3,idplotx = 2),yaxt='s',
    las=1,xaxt='s',new=TRUE)
with(FIAIntValstats[(get(extpar)==listVal[idextPar]&Neta==0)&Gamma==0.8],{
  plotCI(x=Interv+(match(option,sort(unique(option)))-1)*0.08,
         y=meanChoice,
         ui = upIQRChoice,li=lowIQRChoice,
         pch=16,xlab='',ylab='',
         col=coloptions[match(option,sort(unique(option)))],
         sfrac=0.002,cex.axis=1.3,yaxt='n')
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
  # legend('topleft',col =coloptions,legend = unique(option),pch =20,
  #        ncol = 3,cex = 0.8)
  text(x=par('usr')[1]+0.5*(par('usr')[2]-par('usr')[1]),
       y=par('usr')[3]+0.2*(par('usr')[4]-par('usr')[3]),
       label = "future")
})

par(plt=posPlot(numplotx = 3,idplotx = 3),yaxt='s',
    las=1,xaxt='s',new=TRUE)
with(FIAIntValstats[(get(extpar)==listVal[idextPar]&Neta==1)&Gamma==0],{
  plotCI(x=Interv+(match(option,unique(option))-1)*0.08,
         y=meanChoice,
         ui = upIQRChoice,li=lowIQRChoice,
         pch=16,xlab='',ylab='',
         col=coloptions[match(option,sort(unique(option)))],
         sfrac=0.002,cex.axis=1.3,yaxt='n')
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
  legend('topleft',col =coloptions,legend = sort(unique(option)),pch =20,
         ncol = 3,cex = 0.8)
  text(x=par('usr')[1]+0.5*(par('usr')[2]-par('usr')[1]),
       y=par('usr')[3]+0.2*(par('usr')[4]-par('usr')[3]),
       label = "punishment")
  
})

dev.off()

# Plot the dynamics of VR choice -----------------------------------------------

png(paste(projDir,"\\numSp9.png",sep=""),width = 1000,height = 800)

par(plt=posPlot(numplotx = 3,idplotx = 1),yaxt='s',
    las=1,xaxt='s')
with(FIAIntValstats[(option=="RV"&Neta==0)&Gamma==0],{
  plotCI(x=Interv+posit,y=meanChoice,
         ui = upIQRChoice,li=lowIQRChoice,
         pch=16,xlab='',ylab='',ylim=c(0.3,1),
         col=colMany[match(get(extpar),unique(get(extpar)))],
         sfrac=0.002,cex.axis=1.3,yaxt='s')
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
  # axis(side=4,cex.axis=1.3)
  legend('topleft',col =colMany,
         legend = unique(get(extpar)),
         lwd=1, ncol = 3,cex = 0.8,pch = 20)
  text(x=par('usr')[1]+0.5*(par('usr')[2]-par('usr')[1]),
       y=par('usr')[3]+0.2*(par('usr')[4]-par('usr')[3]),
       label = "no future - no punishment")
})

par(plt=posPlot(numplotx = 3,idplotx = 2),yaxt='s',
    las=1,xaxt='s',new=TRUE)
with(FIAIntValstats[(option=="RV"&Neta==0)&Gamma==0.8],{
  plotCI(x=Interv+posit,y=meanChoice,
         ui = upIQRChoice,li=lowIQRChoice,
         pch=16,xlab='',ylab='',ylim=c(0.3,1),
         col=colMany[match(get(extpar),unique(get(extpar)))],
         sfrac=0.002,cex.axis=1.3,yaxt='n')
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
  # axis(side=4,cex.axis=1.3)
  # legend('topleft',col =colMany,
  #        legend = unique(get(extpar)),
  #        lwd=1, ncol = 3,cex = 0.8,pch = 20)
  text(x=par('usr')[1]+0.5*(par('usr')[2]-par('usr')[1]),
       y=par('usr')[3]+0.2*(par('usr')[4]-par('usr')[3]),
       label = "future")
})
  
par(plt=posPlot(numplotx = 3,idplotx = 3),yaxt='s',
    las=1,xaxt='s',new=TRUE)
with(FIAIntValstats[(option=="RV"&Neta==1)&Gamma==0],{
  plotCI(x=Interv+posit,y=meanChoice,
         ui = upIQRChoice,li=lowIQRChoice,
         pch=16,xlab='',ylab='',ylim=c(0.3,1),
         col=colMany[match(get(extpar),unique(get(extpar)))],
         sfrac=0.002,cex.axis=1.3,yaxt='n')
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
  # axis(side=4,cex.axis=1.3)
  # legend('topleft',col =colMany,
  #        legend = unique(get(extpar)),
  #        lwd=1, ncol = 3,cex = 0.8,pch = 20)
  text(x=par('usr')[1]+0.5*(par('usr')[2]-par('usr')[1]),
       y=par('usr')[3]+0.2*(par('usr')[4]-par('usr')[3]),
       label = "punishment")
})

dev.off()

# Old code ---------------------------------------------------------------------



extpar<-listPar[1]

FIAIntstats<-FIAtimeInt[,.(meanProb=mean(Type_choice.mean),
                           upIQR=fivenum(Type_choice.mean)[4],
                           lowIQR=fivenum(Type_choice.mean)[2])
                        ,by=.(Interv,Neta,Gamma,get(extpar))]
setnames(FIAIntstats,'get',extpar)

PIAIntstats<-PIAtimeInt[,.(meanProb=mean(Type_choice.mean),
                           upIQR=fivenum(Type_choice.mean)[4],
                           lowIQR=fivenum(Type_choice.mean)[2])
                        ,by=.(Interv,Neta,Outbr,Tau,Gamma,get(extpar))]
setnames(PIAIntstats,'get',extpar)

FIAIntstats[,posit:=(get(extpar)-30)*0.02]
# PIAIntstats[,posit:=ifelse(Gamma==0&Neta==0,0,
#                            ifelse(Gamma==0.8&Neta==0,0.1,
#                                   ifelse(Gamma==0&Neta==1,0.2,0.3)))]

par(plt=posPlot(numplotx = 2,idplotx = 1)+c(-0.05,-0.05,0,0),yaxt='s',
    las=1,xaxt='s')
with(FIAIntstats,{
  plotCI(x=Interv+posit,y=meanProb,
         ui = upIQR,li=lowIQR,
         pch=16,xlab='',ylab='',
         col=colboxes[match(get(extpar),unique(get(extpar)))],
         sfrac=0.002,cex.axis=1.3)
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
})

legend('bottomright',
       legend=unique(FIAIntstats[,get(extpar)])[order(unique(FIAIntstats[,get(extpar)]))],
              col=colboxes,pch=15,
              title=eval(extpar),cex=1.5,ncol=3)

par(plt=posPlot(numplotx = 2,idplotx = 2)+c(-0.05,-0.05,0,0),
    new=TRUE,yaxt='s',xpd=TRUE)
with(PIAIntstats,{
  plotCI(x=Interv,y=meanProb,
         ui = upIQR,li=lowIQR,
         pch=16,xlab='',ylab='',
         col=colboxes[match(get(extpar),unique(get(extpar)))],
         sfrac=0.002,cex.axis=1.3,yaxt='n')
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
  axis(side=4,cex.axis=1.3)
})

png(filename = paste(projDir,eval(extpar),".png",sep=""))

dev.off()

param[1]


  
  