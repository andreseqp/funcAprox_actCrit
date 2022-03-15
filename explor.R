# ------------------ Exploration ------------------------ #

# Directories --------------------------------------------------------------
genDir<-"multRegr"
scriptDir<-"d:/quinonesa/learning_models_c++/functAprox_actCrit/"
plotsdir<-"D:/quinonesa/Dropbox/Neuchatel/Results/functAprox/multLinReg/"


# libraries ----------------------------------------------------------------
library(here)
source('../R_files/posPlots.R')
source("aesth_par.R")
source("loadData.R")
source('../R_files/ternaryAEQP.R')
library('plotrix')
library('lme4')


# Load data ------------------------------------------------------------


(listPar<-c(rep("LenRewNumSp",4)))
(listVal<-c(1,2,3,4))


FIAraw<-rbindlist(lapply(getFilelist(
  here("Simulations",genDir),listPar,listVal)$FIA,
                         loadRawData,folder=here("Simulations",genDir)))
                  
param<-getParam(here("Simulations",genDir),
                listparam = listPar,values = listVal)

FIAraw[,idcombSps:=ifelse(test = Type_choice,
                          10*as.numeric(gsub("Sp",Species_choice,
                                             replacement = "")),
                          as.numeric(gsub("Sp",Species_choice,replacement = "")))+
         ifelse(test = Type_discard,
                10*as.numeric(gsub("Sp",Species_discard,replacement = "")),
                as.numeric(gsub("Sp",Species_discard,replacement = "")))]

FIAraw[,sizeDiff:=Height_choice-Height_discard]


# FIAagg<-FIAraw[, as.list(unlist(lapply(.SD, function(x) list(mean = mean(x),
#                                                              sd = sd(x))))),
#                by=.(Age,AlphaCri,AlphaAct,Gamma,Neta,rdNumSP), 
#                .SDcols=grep("[[:digit:]]",names(FIAraw))]


# FIAtimeInt<-do.call(
#   rbind,lapply(
#     getFilelist(genDir,listPar,listVal)$FIA,
#     file2timeInter,interV=1001))

rm(FIAtimeInt)

FIAtimeIntValue<-rbindlist(lapply(getFilelist(genDir,listPar,listVal)$FIA,
                           file2timeInterValue,interV=1001))

# PIAtimeInt<-do.call(
#   rbind,lapply(
#     getFilelist(genDir,listPar,listVal)$PIA,
#     file2timeInter,interV=1001))

# Set simulation parameters to plot --------------------------------------------

idRep<-0
GammaP<-0.8
NetaP<-0
label<-"Futur"
  
# Plot the dynamics of the clients values --------------------------------------------------------------
png(paste(plotsdir,listPar[1],"value_",label,".png",sep=""),
    width = 1000,height = 1000)
axisX<-c("n","n","s","s")
axisY<-c("s","n","s","n")
ylabs<-c("Estimated Value","","Estimated Value","")
xlabs<-c("","","trials","trials")

idPlotX<-c(1,2,1,2)
idplotY<-c(2,2,1,1)
plot.new()
for(nSp in c(1,2,3,4)){
  par(plt=posPlot(numplotx = 2,numploty = 2,
                  idplotx = idPlotX[nSp],idploty = idplotY[nSp])+c(0,0,-0.02,-0.02),
      new=TRUE,xaxt='s',yaxt='s',las=1)
  
  with(FIAraw[(Gamma==GammaP&Neta==NetaP)&LenRewNumSp==nSp],{#
    plot(value~Age,type='p',xaxt=axisX[nSp],yaxt=axisY[nSp],ylim=c(0,5),
         xlab=xlabs[nSp],ylab=ylabs[nSp],pch=20,cex=1,
         col=coloptions[match(option,sort(unique(option)))])#
    legend('topleft',col =coloptions,legend = sort(unique(option)),pch =20,
           ncol = 3,cex = 0.8)
    text(y = 0.2,x=max(Age)/10,labels =paste("num Sp= ",nSp,sep=""))
  })
}
text(y = 0.2,x=max(FIAraw[,Age])/2,labels = label)
dev.off()
# Plot the dynamics of the preference --------------------------------------------------------------

png(paste(plotsdir,listPar[1],"pref_",label,".png",sep=""),
    width = 1000,height = 1000)
ylabs<-c("Preference","","Preference","")
xlabs<-c("","","trials","trials")
plot.new()
for(nSp in c(1,2,3,4)){
  par(plt=posPlot(numplotx = 2,numploty = 2,
                  idplotx = idPlotX[nSp],idploty = idplotY[nSp])+c(0,0,-0.02,-0.02),
      new=TRUE,xaxt='s',yaxt='s',las=1)
  with(FIAraw[(Gamma==GammaP&Neta==NetaP)&LenRewNumSp==nSp],{#
    plot(preference~Age,type='p',xaxt=axisX[nSp],yaxt=axisY[nSp],
         xlab=xlabs[nSp],ylab=ylabs[nSp],pch=20,cex=1,
         col=coloptions[match(option,sort(unique(option)))])
    text(y = -8,x=max(Age)/10,labels =paste("num Sp= ",nSp,sep=""))
  })
}
legend('topleft',col =coloptions,legend = sort(unique(FIAraw$option)),pch =20,
       ncol = 3,cex = 0.8)
text(y = -8,x=max(FIAraw[,Age])/2,labels = label)
dev.off()

# Plot RV preference by species ------------------------------------------------

png(paste(plotsdir,listPar[1],"prefRV_",label,".png",sep=""),
    width = 1000,height = 1000)
axisX<-c("n","n","s","s")
axisY<-c("s","n","s","n")
ylabs<-c("preference","","preference","")
xlabs<-c("","","trials","trials")

idPlotX<-c(1,2,1,2)
idplotY<-c(2,2,1,1)
plot.new()
for(nSp in c(1,2,3,4)){
  par(plt=posPlot(numplotx = 2,numploty = 2,
                  idplotx = idPlotX[nSp],idploty = idplotY[nSp])+c(0,0,-0.02,-0.02),
      new=TRUE,xaxt='s',yaxt='s',las=1)
  with(FIAraw[(Gamma==GammaP&Neta==NetaP)&(option=="RV"&LenRewNumSp==nSp)],{#
    plot(preference~Age,type='p',xlab=xlabs[nSp],ylab=ylabs[nSp],
         pch=20,cex=1,ylim=c(-10,10),xaxt=axisX[nSp],yaxt=axisY[nSp],
         col=coloptions[match(idcombSps,sort(unique(idcombSps)))])
    legend('topleft',col =coloptions,legend = sort(unique(idcombSps)),
           pch =20,ncol = 3,cex = 0.8)
    abline(a=0.5,b=0,col="grey")
    text(y = 0.1,x=max(Age)/10,labels =paste("num Sp= ",nSp,sep=""))
  })
}
text(y = 0.2,x=max(FIAraw[,Age])/2,labels = label)
dev.off()

# Plot RV preference by difference in height -----------------------------------

png(paste(plotsdir,listPar[1],"prefRV_hei_",label,".png",sep=""),
    width = 1000,height = 1000)
plot.new()
for(nSp in c(1,2,3,4)){
  par(plt=posPlot(numplotx = 2,numploty = 2,
                  idplotx = idPlotX[nSp],idploty = idplotY[nSp]),
      new=TRUE,xaxt='s',yaxt='s',las=1)
  with(FIAraw[(Gamma==GammaP&Neta==NetaP)&(option=="RV")&LenRewNumSp==nSp],{#
    plot(preference~Age,type='p',
         pch=20,cex=1,xlab=xlabs[nSp],ylab=ylabs[nSp],
         xaxt=axisX[nSp],yaxt=axisY[nSp],
         col=paletteCont(100)[findInterval(Height_choice-Height_discard,
                                           seq(min(Height_choice-Height_discard),
                                               max(Height_choice-Height_discard),
                                               length=100))])
    color.bar.aeqp(paletteCont(100),min =round(min(Height_choice-Height_discard),2),
                   max = round(max(Height_choice-Height_discard),2),nticks = 3,
                   title = "",cex.tit = 2,numplotx = 12,
                   numploty = 6,idplotx = ifelse(idPlotX[nSp]==1,1,7),
                   idploty = ifelse(idplotY[nSp]==1,3,6),locAxis = 4)
  })
}
text(y = 0.2,x=max(FIAraw[,Age])/2,labels = label)
dev.off()
# Is preference correlated with size difference? -------------------------------

png(paste(plotsdir,listPar[1],"prefRV_hei_cor_",label,".png",sep=""),
    width = 1000,height = 1000)

xlabs<-c("","","Size diff","Size diff")
plot.new()
for(nSp in c(1,2,3,4)){
  par(plt=posPlot(numplotx = 2,numploty = 2,
                  idplotx = idPlotX[nSp],idploty = idplotY[nSp])+c(0,0,-0.02,-0.02),
      new=TRUE,xaxt='s',yaxt='s',las=1)
  with(FIAraw[(Gamma==GammaP&Neta==NetaP)&(option=="RV")
              &(LenRewNumSp==nSp&choice==0)],{#
                plot(preference~sizeDiff,type='p',
                     pch=20,cex=0.8,xlab=xlabs[nSp],ylab=ylabs[nSp],
                     xaxt=axisX[nSp],yaxt=axisY[nSp],xlim=c(-80,80),
                     col=coloptions[match(idcombSps,sort(unique(idcombSps)))])
    legend('topleft',col =coloptions,legend = sort(unique(idcombSps)),
                       pch =20,ncol = 3,cex = 0.8)
    abline(a=0,b=0,col="grey")
  })
}
text(y = 0.2,x=max(FIAraw[,Age])/2,labels = label)
dev.off()

# Is preference correlated with length difference? -------------------------------


xlabs<-c("","","Length","Length")
plot.new()
for(nSp in c(1,2,3,4)){
  par(plt=posPlot(numplotx = 2,numploty = 2,
                  idplotx = idPlotX[nSp],idploty = idplotY[nSp])+c(0,0,-0.02,-0.02),
      new=TRUE,xaxt='s',yaxt='s',las=1)
  with(FIAraw[(Gamma==GammaP&Neta==NetaP)&(option=="RV")
              &(LenRewNumSp==nSp&choice==0)],{#
                plot(preference~Length_choice,type='p',
                     pch=20,cex=0.8,xlab=xlabs[nSp],ylab=ylabs[nSp],
                     xaxt=axisX[nSp],yaxt=axisY[nSp],xlim=c(0,100),
                     col=coloptions[match(idcombSps,sort(unique(idcombSps)))])
                legend('topleft',col =coloptions,legend = sort(unique(idcombSps)),
                       pch =20,ncol = 3,cex = 0.8)
              })
  
}

# Total right choices ----------------------------------------------------------

FIAraw[(option=="RV"&Age>max(Age)*0.8),
           sum(Type_choice==1)*(1/length(Type_choice)),
       by=.(Gamma,Neta,LenRewNumSp)]


FIAraw[(option=="R0"&Age>max(Age)*0.8),
       sum(Type_choice==0)*(1/length(Type_choice)),
       by=.(Gamma,Neta,LenRewNumSp)]

FIAraw[(option=="V0"&Age>max(Age)*0.8),
       sum(Type_choice==1)*(1/length(Type_choice)),
       by=.(Gamma,Neta,LenRewNumSp)]


# percentage of cases where the visitor is smaller -----------------------------


FIAraw[(option=="RV"&Type_choice==0),
       sum(sizeDiff<0)*(1/length(sizeDiff)),by=.(Gamma,Neta,LenRewNumSp)]





# parH<-3
# 
# par(plt=posPlot(numplotx = 2,idplotx = 2)+c(-0.05,-0.05,0,0),yaxt='s',
#     las=1,xaxt='s',new=TRUE)
# with(FIAIntValstats[get(extpar)==parH&Neta==0],{
#   plotCI(x=Interv,y=meanVal,
#          ui = upIQRVal,li=lowIQRVal,
#          pch=16,xlab='',ylab='',
#          col=coloptions[match(option,unique(option))],
#          sfrac=0.002,cex.axis=1.3)
#   lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
#   legend('topleft',col =coloptions,legend = unique(option),pch =20,
#          ncol = 3,cex = 0.8)
# })







FIAIntValstats[rdNumSP==5]

par(plt=posPlot(numplotx = 2,idplotx = 2),yaxt='s',las=1,
    xaxt='s',new=TRUE)
with(FIAIntValstats[option=="RV"&Neta==1],{
  plotCI(x=Interv+posit,y=meanChoice,
         ui = upIQRChoice,li=lowIQRChoice,
         pch=16,xlab='',ylab='',yaxt='n',ylim=c(0.5,1),
         col=colboxes[match(get(extpar),unique(get(extpar)))],
         sfrac=0.002,cex.axis=1.3)
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
  legend('topleft',col =colboxes,legend = unique(get(extpar)),pch =20,
         ncol = 3,cex = 0.8)
})


# Try to understand the failures --------------------------------------------------------

tmp<-FIAraw[option=='RV'& Age>0.5*max(Age)]

tmp[,':='(length.diff=Length_choice-Length_discard,
          height.diff=Height_choice-Height_discard,
          redMain.diff=redMain_choice-redMain_discard)]

names(tmp)

npx<-3

par(plt=posPlot(numplotx = npx,idplotx = 1)-c(0.05,0.05,0,0),new=FALSE)
plot(Type_choice~length.diff,data=tmp)
lines(x = rep(0,2),y=c(0,1),col='grey')

par(plt=posPlot(numplotx = npx,idplotx = 2)-c(0.05,0.05,0,0),new=TRUE)
plot(Type_choice~height.diff,data=tmp,yaxt='n',ylab='')
lines(x = rep(0,2),y=c(0,1),col='grey')

par(plt=posPlot(numplotx = npx,idplotx = 3)-c(0.05,0.05,0,0),new=TRUE)
plot(Type_choice~redMain.diff,data=tmp,yaxt='n',ylab='')
lines(x = rep(0,2),y=c(0,1),col='grey')

Heig.mod<-glm(Type_choice~length.diff+height.diff+redMain.diff,
              family = binomial,data = tmp)
summary(Heig.mod)

Heig.mod.int<-glm(Type_choice~length.diff*height.diff*redMain.diff,
              family = binomial,data = tmp)
summary(Heig.mod.int)

Heig.mod.length<-glm(Type_choice~Length_1_0+Length_1_1+Length_2_0+Length_2_1,
              family = binomial,data = tmp)
summary(Heig.mod.length)

Heig.mod.height<-glm(Type_choice~Height_1_0+Height_1_1+Height_2_0+Height_2_1,
                     family = binomial,data = tmp)
summary(Heig.mod.height)






