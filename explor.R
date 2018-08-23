# ------------------ Exploration ------------------------ #

# Directories --------------------------------------------------------------
genDir<-"S:/quinonesa/Simulations/functionAprox/ActCrit/"
scriptDir<-"d:/quinonesa/learning_models_c++/functAprox_actCrit/"


# libraries ----------------------------------------------------------------
source('d:/quinonesa/Dropbox/R_files/posPlots.R')
source(paste(scriptDir,"aesth_par.R",sep=""))
source(paste(scriptDir,"loadData.R",sep = ""))
library('plotrix')
library('lme4')


# Load data ------------------------------------------------------------

setwd(genDir)

(listPar<-c(rep("mHeight",3),"gamma","neta"))
(listVal<-c(30,40,50,0.8,0))

FIAraw<-loadRawData(genDir,getFilelist(genDir,listPar,listVal)$FIA[1])
param<-getParam(genDir,listparam = listPar,values = listVal)



FIAagg<-FIAraw[, as.list(unlist(lapply(.SD, function(x) list(mean = mean(x),
                                                             sd = sd(x))))),
               by=.(Age,AlphaCri,AlphaAct,Gamma,Neta), 
               .SDcols=grep("[[:digit:]]",names(FIAraw))]


FIAtimeInt<-do.call(
  rbind,lapply(
    getFilelist(genDir,listPar,listVal)$FIA,
    file2timeInter,interV=1001))

PIAtimeInt<-do.call(
  rbind,lapply(
    getFilelist(genDir,listPar,listVal)$PIA,
    file2timeInter,interV=1001))

DPdataProb<-do.call(rbind,
                    lapply(getFilelist(genDir,listPar,listVal)$DP,
                           file2lastDP))


featNames<-list(first.client=list(
  choise_0=list(
    mean=grep("_1_0.mean",names(FIAagg),value = TRUE),
    sd=grep("_1_0.sd",names(FIAagg),value = TRUE)),
  choise_1=list(
    mean=grep("_1_1.mean",names(FIAagg),value = TRUE),
    sd=grep("_1_1.sd",names(FIAagg),value = TRUE))),
  second.client=list(
    choise_0=list(
      mean=grep("_2_0.mean",names(FIAagg),value = TRUE),
      sd=grep("_2_0.sd",names(FIAagg),value = TRUE)),
    choise_1=list(
      mean=grep("_2_1.mean",names(FIAagg),value = TRUE),
      sd=grep("_2_1.sd",names(FIAagg),value = TRUE))))


#  Critic feature weights dyanamics--------------------------------------------------

countR<-1
countC<-0
i<-0
par(xaxt='s',yaxt='s')
plot.new()

ylimtemp<-c(min(FIAagg[,
                       .SD,.SDcols=grep("_Crit",names(FIAagg),value = TRUE)]),
            max(FIAagg[,
                       .SD,.SDcols=grep("_Crit",names(FIAagg),value = TRUE)]))

with(FIAagg,{
  for(feat in grep("0_Crit.mean",names(FIAagg),value = TRUE)){
    i<-i+1
    countC<-countC+1
    par(plt=posPlot(numplotx = 5,numploty = 5,idplotx = countC,idploty = countR),
        new=TRUE,las=1,cex.main=0.5)
    plot(c(min(Age),max(Age)),rep(0,2),type = "l",
         xlab = '',ylab='',ylim=ylimtemp,col="grey")
    polygon(c(Age,Age[length(Age):1]),
              c(get(feat)+get(grep("0_Crit.sd",names(FIAagg),
                                   value = TRUE)[i]),
                get(feat)[length(Age):1]-
                  get(grep("0_Crit.sd",names(FIAagg),
                           value = TRUE)[i])[length(Age):1]),
            col = colours[1],border = FALSE)
    polygon(c(Age,Age[length(Age):1]),
            c(get(grep("1_Crit.mean",names(FIAagg),value = TRUE)[i])+
                    get(grep("1_Crit.sd",names(FIAagg),
                             value = TRUE)[i]),
              get(grep("1_Crit.mean",names(FIAagg),
                       value = TRUE)[i])[length(Age):1]-
                get(grep("1_Crit.sd",names(FIAagg),
                         value = TRUE)[i])[length(Age):1]),
            col = colours[2],border = FALSE);
    lines(Age,get(feat),type = "l")
    lines(Age,get(grep("1_Crit.mean",names(FIAagg),
                       value = TRUE)[i]),type = "l")
    # title(main = feat,line = -4)
    legend('bottomleft',
           legend = c(feat,grep("1_Crit.mean",names(FIAagg),value=TRUE)[i])
                                       ,col = colours,pch = 15,cex = 0.5)
    par(yaxt='n');
    if((i)%%5==0)
    {
      countR<-countR+1
      countC<-0
      par(yaxt='s',xaxt='n')
    }
  }
})



# Actor feature weights dyanamics ----------------------------------------------

countR<-1
countC<-0
i<-0
par(xaxt='s',yaxt='s')
plot.new()

ylimtemp<-c(min(FIAagg[,
                       .SD,.SDcols=grep("_Act",names(FIAagg),value = TRUE)]),
            max(FIAagg[,
                       .SD,.SDcols=grep("_Act",names(FIAagg),value = TRUE)]))

with(FIAagg,{
  for(feat in grep("0_Act.mean",names(FIAagg),value = TRUE)){
    i<-i+1
    countC<-countC+1
    par(plt=posPlot(numplotx = 5,numploty = 5,idplotx = countC,idploty = countR),
        new=TRUE,las=1,cex.main=0.5)
    plot(c(min(Age),max(Age)),rep(0,2),type = "l",
         xlab = '',ylab='',ylim=ylimtemp,col="grey")
    polygon(c(Age,Age[length(Age):1]),
            c(get(feat)+get(grep("0_Act.sd",names(FIAagg),
                                 value = TRUE)[i]),
              get(feat)[length(Age):1]-
                get(grep("0_Act.sd",names(FIAagg),
                         value = TRUE)[i])[length(Age):1]),
            col = colours[1],border = FALSE)
    polygon(c(Age,Age[length(Age):1]),
            c(get(grep("1_Act.mean",names(FIAagg),value = TRUE)[i])+
                get(grep("1_Act.sd",names(FIAagg),
                         value = TRUE)[i]),
              get(grep("1_Act.mean",names(FIAagg),
                       value = TRUE)[i])[length(Age):1]-
                get(grep("1_Act.sd",names(FIAagg),
                         value = TRUE)[i])[length(Age):1]),
            col = colours[2],border = FALSE);
    lines(Age,get(feat),type = "l")
    lines(Age,get(grep("1_Act.mean",names(FIAagg),
                       value = TRUE)[i]),type = "l")
    # title(main = feat,line = -4)
    legend('bottomleft',
           legend = c(feat,grep("1_Act.mean",names(FIAagg),value=TRUE)[i])
           ,col = colours,pch = 15,cex = 0.5)
    par(yaxt='n');
    if((i)%%5==0)
    {
      countR<-countR+1
      countC<-0
      par(yaxt='s',xaxt='n')
    }
  }
})

  
# Plot the dynamics of the clients values --------------------------------------------------------------

par(plt=posPlot(),xaxt='s',yaxt='s')
with(FIAraw[Training==0],{
  plot(value~Age,type='p',
       xlab='Trials',ylab='Estimated value',pch=20,cex=1,
       col=coloptions[match(option,unique(option))])#
legend('topleft',col =coloptions,legend = unique(option),pch =20,
       ncol = 3,cex = 0.8)
})

FIAraw[,length(Age)/dim(FIAraw)[1],by=option]

# Plot the dynamics of the preference --------------------------------------------------------------

par(plt=posPlot(numplotx = 2,idplotx = 1),xaxt='s',yaxt='s')
with(FIAraw[Training==0&option=="RV"],{
  plot(logist(preference),type='p',
       xlab='Trials',ylab='preference',pch=20,cex=1,
       col=coloptions[match(option,unique(option))])
  # legend('topright',col =coloptions,legend = unique(option),pch =20,
  #        ncol = 3,cex = 0.8)
})


par(plt=posPlot(2,idplotx = 2),xaxt='s',yaxt='s',new=TRUE)
with(FIAraw[Training==0&option=="RV"],{
  plot(Type_choice~Age,type='p',
     xlab='Trials',ylab='Choice',pch=20,cex=1,xaxt='n',
     col=coloptions[match(option,unique(option))])
  axis(4)
  # legend('topright',col =coloptions,legend = unique(option),pch =20,
  #      ncol = 3,cex = 0.8)
})

FIAraw[(Training==0&option=="R0")&Type_choice==2,.(choice,Age)]



  text(x=par('usr')[1]+(par('usr')[2]-par('usr')[1])*0.05,
       y=par('usr')[3]+(par('usr')[4]-par('usr')[3])*0.1,
       labels=bquote(gamma==.(unique(Gamma))))
  text(x=par('usr')[1]+(par('usr')[2]-par('usr')[1])*0.05,
       y=par('usr')[3]+(par('usr')[4]-par('usr')[3])*0.15,
       labels = bquote(eta==.(unique(Neta))))
  legend('topleft',col =c(1,2,3),legend = c('resident','visitor','absence'),pch =20,
         ncol = 3,cex = 0.8)
})


# Try to understand the failures --------------------------------------------------------

tmp<-FIAraw[option=='RV'& Age>0.5*max(Age)]

tmp[,':='(length.diff=Length_choice-Length_discard,
          height.diff=Height_choice-Height_discard,
          redMain.diff=redMain_choice-redMain_discard)]

names(tmp)

npx<-3

par(plt=posPlot(numplotx = npx,idplotx = 1)-c(0.05,0.05,0,0))
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






