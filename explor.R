# ------------------ Exploration ------------------------ #

# Directories --------------------------------------------------------------
genDir<-"D:\\quinonesa\\Simulation\\functionAprox\\"
scriptDir<-"d:/quinonesa/learning_models_c++/functionAprox/"


# libraries ----------------------------------------------------------------
source('d:/quinonesa/Dropbox/R_files/posPlots.R')
source(paste(scriptDir,"aesth_par.R",sep=""))
source(paste(scriptDir,"loadData.R",sep = ""))
library('plotrix')


# Load data ------------------------------------------------------------


listPar<-c("gamma","tau","neta","outb")
listVal<-c(0.8,10,0,0)

FIAraw<-loadRawData(genDir,"FIA",listparam = listPar,values = listVal)
param<-getParam(genDir,listparam = listPar,values = listVal)



FIAagg<-FIAraw[, as.list(unlist(lapply(.SD, function(x) list(mean = mean(x),
                                                             sd = sd(x))))),
               by=.(Age,Alpha,Gamma,Tau,Neta,Outbr), 
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


# Plot of the dynamics of the feature weights --------------------------------------------------

countR<-1
countC<-0
i<-0
par(xaxt='s',yaxt='s')
plot.new()
ylimtemp<-c(min(FIAagg[(Tau==10 & Gamma==0.8)&(Neta==0 & Outbr==0),
                       .SD,.SDcols=grep("_",names(FIAagg),value = TRUE)]),
            max(FIAagg[(Tau==10 & Gamma==0.8)&(Neta==0 & Outbr==0),
                       .SD,.SDcols=grep("_",names(FIAagg),value = TRUE)]))
with(FIAagg[(Tau==10 & Gamma==0.8)&(Neta==0 & Outbr==0)],{
  for(feat in grep("_0.mean",names(FIAagg),value = TRUE)){
    i<-i+1
    countC<-countC+1
    par(plt=posPlot(numplotx = 5,numploty = 5,idplotx = countC,idploty = countR),
        new=TRUE,las=1,cex.main=0.5)
    plot(c(min(Age),max(Age)),rep(0,2),type = "l",
         xlab = '',ylab='',ylim=ylimtemp,col="grey")
    polygon(c(Age,Age[length(Age):1]),
              c(get(feat)+get(grep("_0.sd",names(FIAagg),
                                   value = TRUE)[i]),
                get(feat)[length(Age):1]-
                  get(grep("_0.sd",names(FIAagg),
                           value = TRUE)[i])[length(Age):1]),
            col = colours[1],border = FALSE)
    polygon(c(Age,Age[length(Age):1]),
            c(get(grep("_1.mean",names(FIAagg),value = TRUE)[i])+
                    get(grep("_1.sd",names(FIAagg),
                             value = TRUE)[i]),
              get(grep("_1.mean",names(FIAagg),
                       value = TRUE)[i])[length(Age):1]-
                get(grep("_1.sd",names(FIAagg),
                         value = TRUE)[i])[length(Age):1]),
            col = colours[2],border = FALSE);
    lines(Age,get(feat),type = "l")
    lines(Age,get(grep("_1.mean",names(FIAagg),
                       value = TRUE)[i]),type = "l")
    # title(main = feat,line = -4)
    legend('bottomleft',
           legend = c(feat,grep("_1.mean",names(FIAagg),value=TRUE)[i])
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
with(FIAraw[((Tau==10 & Gamma==0.8)&(Neta==0 & Outbr==0.2))&option=='RV'],{
  plot(value_choice~Age,type='p',ylim=c(min(value_choice),max(value_choice)),
       xlab='Trials',ylab='Estimated value',pch=20,cex=1,col=Type_choice+1)
  points(value_discard~Age,pch=20,cex=1,col=Type_discard+1)
  text(x=par('usr')[1]+(par('usr')[2]-par('usr')[1])*0.05,
       y=par('usr')[3]+(par('usr')[4]-par('usr')[3])*0.05,
       labels = bquote(tau==.(unique(Tau))))
  text(x=par('usr')[1]+(par('usr')[2]-par('usr')[1])*0.05,
       y=par('usr')[3]+(par('usr')[4]-par('usr')[3])*0.1,
       labels=bquote(gamma==.(unique(Gamma))))
  text(x=par('usr')[1]+(par('usr')[2]-par('usr')[1])*0.05,
       y=par('usr')[3]+(par('usr')[4]-par('usr')[3])*0.15,
       labels = bquote(eta==.(unique(Neta))))
  legend('topleft',col =c(1,2,3),legend = c('resident','visitor','absence'),pch =20,
         ncol = 3,cex = 0.8)
})


# Plot the dynamics of VR choice -----------------------------------------------------------

FIAIntstats<-FIAtimeInt[,.(meanProb=mean(Type_choice.mean),
                           upIQR=fivenum(Type_choice.mean)[4],
                           lowIQR=fivenum(Type_choice.mean)[2])
                        ,by=.(Interv,Neta,Outbr,Tau,Gamma)]

PIAIntstats<-PIAtimeInt[,.(meanProb=mean(Type_choice.mean),
                           upIQR=fivenum(Type_choice.mean)[4],
                           lowIQR=fivenum(Type_choice.mean)[2])
                        ,by=.(Interv,Neta,Outbr,Tau,Gamma)]

par(plt=posPlot(numplotx = 2,idplotx = 1)+c(-0.05,-0.05,0,0),yaxt='s')
with(FIAIntstats,{
  plotCI(x=Interv,y=meanProb,
         ui = upIQR,li=lowIQR,
         pch=16,xlab='',ylab='',
         col=colboxes[match(Gamma,unique(Gamma))],
         sfrac=0.002,cex.axis=1.3)
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
})
with(DPdataProb,  
     {matlines(x = t(matrix(rep(max(FIAtimeInt$Interv)*c(0.75,1),each=3),3)),
               y=t(matrix(rep(probRV.V,2),3)),lwd=2,lty = "dashed",
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


# Try to understand the failures --------------------------------------------------------

tmp<-FIAraw[option=='RV'& Age>0.5*max(Age)]


