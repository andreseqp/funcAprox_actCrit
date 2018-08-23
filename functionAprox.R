# Load external functions and packages -------------------------------------------------------

# Laptop
# source('E:/Dropbox/R_files/posPlots.R')
# source('E:/Dropbox/R_files/vioplot.R')

# Office
source('C:/Users/quinonesa/Dropbox/R_files/posPlots.R')
source('C:/Users/quinonesa/Dropbox/R_files/vioplot.R')

# Home
source("H:\\Dropbox\\R_files\\posPlots.R")
source("H:\\Dropbox\\R_files\\vioplot.R")

# Define Colours -------------------------------------------------------

colours <- c(rgb(red = .0, green = 0, blue = 0.8, alpha = 0.5),
             rgb(red = .8, green = 0.8, blue = 0, alpha = 0.5))

colboxes<- c(rgb(red = 252, green = 141, blue = 89,maxColorValue = 255),
             rgb(red = 255, green = 255, blue = 103,maxColorValue = 255),
             rgb(red = 153,green = 213,blue = 148,maxColorValue = 255))

palette('default')

# Delete data reset environment ---------------------------------------------------------

rm(list = ls())

rm(list=ls()[grepl('data.frame', sapply(ls(), function(x) class(get(x))))])


# Get files -------------------------------------------------------------------

# Laptop
setwd('E:\\Dropbox\\Neuchatel\\prelimResults\\functionAprox')

# Office
setwd('C:\\Users\\quinonesa\\Dropbox\\Neuchatel\\prelimResults\\functionAprox')

# Home
setwd('H:\\Dropbox\\Neuchatel\\prelimResults\\functionAprox')

(listfold<-list.files())
setwd(listfold[1])
getwd()
(listfiles<-list.files())
(listfiles<-listfiles[grep('IndTrain',listfiles)])
seed<-'seed10'
(listfiles<-listfiles[grep(seed,listfiles)])


# plots directory
plotdir<-paste(getwd(),'/plots/',sep='')

# Get data -----------------------------------------------------------------------------------------------------

data1<-read.table(listfiles[1],header = TRUE)
str(data1)
for(fileN in listfiles[2:length(listfiles)])
{
  tempdata<-read.table(fileN,header = TRUE)
  data1<-rbind(data1,tempdata)
}
rm(list =c('tempdata'))

str(data1)

# Dynamic feature weights ------------------------------------------------------------------

cbind(names(data1),1:dim(data1)[2])

getwd()
pdf(paste(plotdir,seed,"feature_weights.pdf",sep=''))
for(gamma in unique(data1$Gamma)[order(unique(data1$Gamma))])
{
  for(tau in unique(data1$Tau))
  {
    tempData<-data1[data1$Gamma==gamma & data1$Tau==tau,]
    meansTemp<-aggregate(tempData[,c(6,7,35:45)],by=list(tempData$Age),FUN=mean)
    SDTemp<-aggregate(tempData[,c(6,7,35:45)],by=list(tempData$Age),FUN = sd)
    plot.new()
    countR<-1
    countC<-0
    numest<-0
    ylimtemp<-c(min(meansTemp[,4:14]),max(meansTemp[,4:14]))
    names(meansTemp)[4:14]
    par(xaxt='s',yaxt='s')
    for(i in 4:14)
    {
       # i<-4
      countC<-countC+1
      par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = countC,idploty = countR),new=TRUE,las=1,
          cex.main=0.5)
      plot(meansTemp[,i]~meansTemp$Group.1,type='l',xlab='',ylab = '',ylim=ylimtemp)
      polygon(y = c(meansTemp[,i]+SDTemp[,i],meansTemp[dim(meansTemp)[1]:1,i]-SDTemp[dim(SDTemp)[1]:1,i]),
              x = c(meansTemp[,1],meansTemp[dim(meansTemp)[1]:1,1]),col = colours[1],border = FALSE)
      lines(meansTemp[,i]~meansTemp$Group.1,col=1)
      lines(x=c(0,max(tempData$Age)),y=c(0,0),col="grey")
      title(main = names(meansTemp)[i],line = -4)
      par(yaxt='n')
      if((i-3)%%4==0)
      {
        countR<-countR+1
        countC<-0
        par(yaxt='s',xaxt='n')
      }
    }
    countC<-countC+1
    par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = countC,idploty = countR),new=TRUE,las=1)
    plot(y=0,x=0,xlab = '',ylab = '',bty='n',xaxt='n',yaxt='n',col=0)
    mtext(1,line = -3,text = as.expression(bquote(tau==.(tau))))
    mtext(1,line = -4,text = as.expression(bquote(gamma==.(gamma))))
  }
  }
dev.off()


# Features distributions ----------------------------------------------------

png(paste(plotdir,seed,"dist_features.png",sep=''),width = 1000,height = 800)

plot.new()
countR<-1
countC<-0
numest<-0
xlimtemp<-c(min(data1[,11:45]),max(data1[,11:45]))
lineText<-c(rep(2,4),rep(0.1,8))
par(xaxt='s',yaxt='s')
for(i in 11:21)
{
  if(i>18){xlimtemp<-c(-1,2)}
  countC<-countC+1
  par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = countC,idploty = countR),new=TRUE,las=1,
      cex.main=0.5)
  distrait<-strsplit(names(data1)[i],'_')[[1]][1]
  
  hist(c(data1[data1$Type_choice==0,paste(distrait,"_choice",sep = '')],
         data1[data1$Type_discard==0,paste(distrait,"_discard",sep = '')])
       ,xlim=xlimtemp,col=colours[1],xlab = '',ylab='',main = '')
  hist(c(data1[data1$Type_choice==1,paste(distrait,"_choice",sep = '')],
         data1[data1$Type_discard==1,paste(distrait,"_discard",sep = '')])
       ,add=TRUE,col = colours[2])
  mtext(text = distrait,side = 1,line = lineText[i-10])
  ylimtemp<-par('usr')[3:4]
  par(yaxt='n')
  if((i-10)%%4==0)
  {
    countR<-countR+1
    countC<-0
    par(yaxt='s',xaxt='n')
  }
}
par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = 4,idploty = 3),new=TRUE,las=1,
    cex.main=0.5)
plot(y=0,x=0,xlab = '',ylab = '',bty='n',xaxt='n',yaxt='n',col=0)
legend('center',col = colours,legend = c('Resident','Visitor'),pch = 15)


dev.off()



# define options  -------------------------------------------------------------

fullRVoptions<-(data1$Type_choice==1 & data1$Type_discard==0) | (data1$Type_choice==0 & data1$Type_discard==1)
fullRR<-(data1$Type_choice==0 & data1$Type_discard==0) 
fullVV<-(data1$Type_choice==1 & data1$Type_discard==1) 
fullRo<-(data1$Type_choice==0 & data1$Type_discard==2) | (data1$Type_choice==2 & data1$Type_discard==0)
fullVo<-(data1$Type_choice==1 & data1$Type_discard==2) | (data1$Type_choice==2 & data1$Type_discard==1)
fulloo<-(data1$Type_choice==2 & data1$Type_discard==2)
fullZeroOpt<-(data1$Type_choice==2 & data1$Type_discard!=2) | (data1$Type_discard==2 & data1$Type_choice!=2)
fullzeroChoice<-fullZeroOpt & data1$Type_choice==2
data1<-cbind(data1,fullRVoptions,fullRR,fullVV,fullRo,fullVo,fulloo,fullZeroOpt,fullzeroChoice)

rm(list=c("fullRVoptions","fullRR","fullVV","fullRo","fullVo","fulloo","fullzeroChoice","fullZeroOpt"))

option<-ifelse(data1$fullRVoptions,1,NA)
option<-ifelse(data1$fullRR,2,option)
option<-ifelse(data1$fullVV,3,option)
option<-ifelse(data1$fullRo,4,option)
option<-ifelse(data1$fullVo,5,option)
option<-ifelse(data1$fulloo,6,option)

data1<-cbind(data1,option)


lastSteps<-data1[data1$Age>max(data1$Age)/2,]
str(lastSteps)


# Value estimation -----------------------------------------------------------

gamma1 = 0; tau1 = 1
tempData<-data1[data1$Gamma==gamma1 & data1$Tau==tau1,]
str(tempData)
# cbind(names(tempData),1:length(names(tempData)))
meansTemp<-aggregate(tempData[,c(6,7,34:44)],by=list(tempData$Age),FUN=mean)
SDTemp<-aggregate(tempData[,c(6,7,34:44)],by=list(tempData$Age),FUN = sd)
str(meansTemp)
str(SDTemp)

par(plt=posPlot(),xaxt='s',yaxt='s')
histTemp1<-hist(c(tempData[tempData$Type_choice==0,"value_choice"],
                  tempData[tempData$Type_discard==0,"value_discard"])
                ,col=colours[1],xlab = '',ylab='',
                main = '')
histTemp2<-hist(c(tempData[tempData$Type_choice==1,"value_choice"],
                  tempData[tempData$Type_discard==1,"value_discard"])
                ,add=TRUE,col = colours[2])


png(paste(plotdir,seed,"dist_values.png",sep=''),width = 1000,height = 800)

plot.new()
countR<-0
countC<-0
XlimHigh<-max(data1[,c("value_choice","value_discard")])
Xlimlow<-min(data1[,c("value_choice","value_discard")])
ylims<-c(0,8000)
par(xaxt='s',las=1)
for(gamma in unique(data1$Gamma)[order(unique(data1$Gamma))])
{
  countR<-countR+1
  countC<-0
  par(yaxt='s')
  # XlimHigh<-max(data1[data1$Gamma==gamma,c("value_choice","value_discard")])
  # Xlimlow<-min(data1[data1$Gamma==gamma,c("value_choice","value_discard")])
  for(tau in unique(data1$Tau)[1:3])
  {
    countC<-countC+1
    tempData<-data1[data1$Gamma==gamma & data1$Tau==tau,]
    par(new=T,plt=posPlot(numplotx = 3,numploty = 3,idplotx = countC,idploty = countR))
    histTemp1<-hist(c(tempData[tempData$Type_choice==1 ,"value_choice"],
                      tempData[tempData$Type_discard==1,"value_discard"])
                    ,col=colours[2],xlab = '',ylab='',ylim=ylims,
                    xlim = c(Xlimlow,XlimHigh),main = '',breaks = 30)
    histTemp2<-hist(c(tempData[tempData$Type_choice==0 ,"value_choice"],
                      tempData[tempData$Type_discard==0 ,"value_discard"])
                    ,add=TRUE,col = colours[1],breaks = 30)
    ylimtemp<-par('usr')[3:4]
    if(countC==1)
    {
      
      par(xpd=T)
      text(x=Xlimlow-(XlimHigh-Xlimlow)/2,y=ylimtemp[2]-(ylimtemp[2]-ylimtemp[1])/2,labels = gamma)
      if(countR==3){text(x=Xlimlow-(XlimHigh-Xlimlow)/2,y=ylimtemp[2],labels = expression(gamma),cex=2)}
    }
    
    if(countR==1)
    {
      par(xpd=T)
      text(x=Xlimlow+(XlimHigh-Xlimlow)/2,y=ylimtemp[1]-(ylimtemp[2]-ylimtemp[1])/2,labels = tau)
      if(countC==1){text(x=Xlimlow,y=ylimtemp[1]-(ylimtemp[2]-ylimtemp[1])/2,labels = expression(tau),cex=2)}
    }
    par(yaxt='n',xpd=F)
  }
  par(xaxt='n')
}
legend('topright',col = colours,legend = c('Resident','Visitor'),pch = 15)

dev.off()


par(plt=posPlot(),xaxt='s',yaxt='s')
hist(c(tempData[tempData$Type_choice==0,"value_choice"],
       tempData[tempData$Type_discard==0,"value_discard"])
     ,col=colours[1],xlab = 'Estimated value',
xlim = c(min(tempData[,c("value_choice","value_discard")]),
         max(tempData[,c("value_choice","value_discard")])))
hist(c(tempData[tempData$Type_choice==1,"value_choice"],
       tempData[tempData$Type_discard==1,"value_discard"])
     ,add=TRUE,col = colours[2])
legend('topleft',col = colours,legend = c('Resident','Visitor'),pch = 15)

par(xaxt='s',yaxt='s')
hist(data1[,"value_choice"]-data1[,"value_discard"]
     ,col=colours[1],xlab = 'Difference in estimated value')

# Distribution of values for RV options -------------------------------------------------------

png(paste(plotdir,seed,"_dist_values_RV.png",sep = ''),width = 1000,height = 800)

plot.new()
countR<-0
countC<-0
XlimHigh<-max(data1[tempData$fullRVoptions,c("value_choice","value_discard")])
Xlimlow<-min(data1[tempData$fullRVoptions,c("value_choice","value_discard")])
ylims<-c(0,2500)
par(xaxt='s',las=1)
for(gamma in unique(data1$Gamma)[order(unique(data1$Gamma))])
{
  countR<-countR+1
  countC<-0
  par(yaxt='s')
  # XlimHigh<-max(data1[data1$Gamma==gamma,c("value_choice","value_discard")])
  # Xlimlow<-min(data1[data1$Gamma==gamma,c("value_choice","value_discard")])
  for(tau in unique(data1$Tau)[1:3])
  {
    countC<-countC+1
    tempData<-data1[data1$Gamma==gamma & data1$Tau==tau,]
    par(new=T,plt=posPlot(numplotx = 3,numploty = 3,idplotx = countC,idploty = countR))
    histTemp1<-hist(c(tempData[tempData$Type_choice==1 & tempData$fullRVoptions,"value_choice"],
                      tempData[tempData$Type_discard==1 & tempData$fullRVoptions,"value_discard"])
                    ,col=colours[2],xlab = '',ylab='',ylim = ylims,
                    xlim = c(Xlimlow,XlimHigh),main = '',breaks = 30)
    histTemp2<-hist(c(tempData[tempData$Type_choice==0 & tempData$fullRVoptions,"value_choice"],
                      tempData[tempData$Type_discard==0 & tempData$fullRVoptions,"value_discard"])
                    ,add=TRUE,col = colours[1],breaks = 30)
    ylimtemp<-par('usr')[3:4]
    if(countC==1)
    {
      
      par(xpd=T)
      text(x=Xlimlow-(XlimHigh-Xlimlow)/2,y=ylimtemp[2]-(ylimtemp[2]-ylimtemp[1])/2,labels = gamma)
      if(countR==3){text(x=Xlimlow-(XlimHigh-Xlimlow)/2,y=ylimtemp[2],labels = expression(gamma),cex=2)}
    }
    
    if(countR==1)
    {
      par(xpd=T)
      text(x=Xlimlow+(XlimHigh-Xlimlow)/2,y=ylimtemp[1]-(ylimtemp[2]-ylimtemp[1])/2,labels = tau)
      if(countC==1){text(x=Xlimlow,y=ylimtemp[1]-(ylimtemp[2]-ylimtemp[1])/2,labels = expression(tau),cex=2)}
    }
    par(yaxt='n',xpd=F)
  }
  par(xaxt='n')
}
legend('topright',col = colours,legend = c('Resident','Visitor'),pch = 15)

dev.off()



# Value dynamics --------------------------------------------------------------

palette('default')

png(paste(plotdir,seed,'value_dynamic.png',sep=''),width = 1000,height = 800)

plot.new()
countR<-0
countC<-0
XlimHigh<-max(tempData$Age)
Xlimlow<-0
cex1=0.1
par(xaxt='s',las=1)
for(gamma in unique(data1$Gamma)[order(unique(data1$Gamma))])
{
  countR<-countR+1
  countC<-0
  par(yaxt='s')
   ylimHigh<-max(data1[data1$Gamma==gamma,c("value_choice","value_discard")])
   ylimlow<-min(data1[data1$Gamma==gamma,c("value_choice","value_discard")])
  for(tau in unique(data1$Tau)[1:3])
  {
    countC<-countC+1
    tempData<-data1[data1$Gamma==gamma & data1$Tau==tau,]
    par(new=T,plt=posPlot(numplotx = 3,numploty = 3,idplotx = countC,idploty = countR))
    plot(tempData[tempData$Type_choice==0,'value_choice']~
           tempData[tempData$Type_choice==0,'Age'],type='p',ylim=c(ylimlow,ylimHigh),
         xlab='',ylab='',pch=20,cex=cex1)
    points(tempData[tempData$Type_discard==0,'value_discard']~
           tempData[tempData$Type_discard==0,'Age'],pch=20,cex=cex1)
    points(tempData[tempData$Type_discard==1,'value_discard']~
             tempData[tempData$Type_discard==1,'Age'],col='red',pch=20,cex=cex1)
    points(tempData[tempData$Type_choice==1,'value_choice']~
             tempData[tempData$Type_choice==1,'Age'],col='red',pch=20,cex=cex1)
    lines(x=c(0,XlimHigh),y=c(0,0),col="grey")
    if(countC==1)
    {
      
      par(xpd=T)
      text(x=-(XlimHigh-Xlimlow)/2,y=ylimHigh-(ylimHigh-ylimlow)/2,labels = gamma)
      if(countR==3){text(x=-(XlimHigh-Xlimlow)/2,y=ylimHigh,labels = expression(gamma),cex=2)}
    }
    
    if(countR==1)
    {
      par(xpd=T)
      text(x=(XlimHigh-Xlimlow)/2,y=ylimlow-(ylimHigh-ylimlow)/2,labels = tau)
      if(countC==1){text(x=Xlimlow,y=ylimlow-(ylimHigh-ylimlow)/2,labels = expression(tau),cex=2)}
    }
    par(yaxt='n',xpd=F)
  }
  par(xaxt='n')
}
legend('topleft',col =c(1,2),legend = c('Resident','Visitor'),pch = 19)

dev.off()

par(plt=posPlot(),xaxt='s',yaxt='s')
plot(data1[,'value_choice']-data1[,'value_discard']~
       data1[,'Age'],type='p',pch=20,cex=0.5)
lines(y=c(0,0),x=c(0,1000),col='grey')


# Value dynamics discriminated by choice --------------------------------------------------------------

png(paste(plotdir,seed,'_value_dynamic_choice.png',sep = ''),width = 1000,height = 800)


plot.new()
countR<-0
countC<-0
XlimHigh<-max(data1$Age)
Xlimlow<-0
cex1<-0.2
par(xaxt='s',las=1)
for(gamma in unique(data1$Gamma)[order(unique(data1$Gamma))])
{
  countR<-countR+1
  countC<-0
  par(yaxt='s')
  ylimHigh<-max(data1[data1$Gamma==gamma,c("value_choice","value_discard")])
  ylimlow<-min(data1[data1$Gamma==gamma,c("value_choice","value_discard")])
  for(tau in unique(data1$Tau))
  {
    countC<-countC+1
    tempData<-data1[data1$Gamma==gamma & data1$Tau==tau,]
    par(new=T,plt=posPlot(numplotx = 3,numploty = 3,idplotx = countC,idploty = countR))
    plot(tempData[,'value_choice']~
           tempData[,'Age'],type='p',ylim=c(ylimlow,ylimHigh),
         xlab='',ylab='',pch=20,cex=cex1,
         col=1+tempData[,"Type_choice"])
    points(tempData[,'value_discard']~
             tempData[,'Age'],pch=20,cex=cex1,
           col=1+tempData[,"Type_discard"])
    if(countC==1)
    {
      
      par(xpd=T)
      text(x=-(XlimHigh-Xlimlow)/2,y=ylimHigh-(ylimHigh-ylimlow)/2,labels = gamma)
      if(countR==3){text(x=-(XlimHigh-Xlimlow)/2,y=ylimHigh,labels = expression(gamma),cex=2)}
    }
    
    if(countR==1)
    {
      par(xpd=T)
      text(x=(XlimHigh-Xlimlow)/2,y=ylimlow-(ylimHigh-ylimlow)/2,labels = tau)
      if(countC==1){text(x=Xlimlow,y=ylimlow-(ylimHigh-ylimlow)/2,labels = expression(tau),cex=2)}
    }
    par(yaxt='n',xpd=F)
  }
  par(xaxt='n')
}
# par(plt=posPlot())
legend('topleft',col =c(1,2,3),legend = c('resident','visitor','absence'),pch =20,
       ncol = 3,cex = 0.8)

dev.off()



# Probability of choosing a visitor -------------------------------------------------------------

lastsRV<-lastSteps[lastSteps$fullRVoptions,]
str(lastsRV)
prob.RV.V<-aggregate(Type_choice~Training*Tau*Gamma,data = lastsRV,FUN = mean)
str(prob.RV.V)
Prob.tau.gamma<-as.data.frame(split(prob.RV.V$Type_choice,
                                    list(prob.RV.V$Tau,prob.RV.V$Gamma)))


png(paste(plotdir,seed,'probRVV_pres.png',sep = ''),width=1000,height=700)

par(plt=posPlot(),xaxt='s',yaxt='s',las=1)
vio<-do.call(vioplot,c(unname(Prob.tau.gamma),
                       col=list(rep(colboxes,each=3)),names=list(rep(unique(prob.RV.V$Tau),3))))

lines(x = c(0,20),y=c(0.5,0.5),col='grey',lwd=1)
#axis(side = 1,labels = rep(unique(cumRewlasts$Tau),3),at=seq(1:9))
par(las=1,xpd=T)
mtext(side = 2,text = 'Probability ',cex = 1.5,line = 3.5)
text(x=0.1,y=par("yaxp")[1]-(par("yaxp")[2]-par("yaxp")[1])/6,labels = expression(tau),cex=2)
legend('bottomright',legend=unique(prob.RV.V$Gamma),col=colboxes,pch=15,title=expression(gamma),cex=1)

dev.off()

# Probability of a client over nothing  -------------------------------------------------------------

lastsC0<-lastSteps[lastSteps$fullZeroOpt,]
str(lastsC0)
lastsC0<-cbind(lastsC0,lastsC0$Type_choice<2)
names(lastsC0)[55]<-'Client_chosen'
prob.C0.R<-aggregate(Client_chosen~Training*Tau*Gamma,data = lastsC0,FUN = mean)
str(prob.C0.R)
Prob.C0.tau.gamma<-as.data.frame(split(prob.C0.R$Client_chosen,
                                    list(prob.C0.R$Tau,prob.C0.R$Gamma)))


png('probC0C_pres_seed3.png',width=1000,height=700)
par(plt=posPlot(),xaxt='s',yaxt='s',las=1)
vio<-do.call(vioplot,c(unname(Prob.C0.tau.gamma),
                       col=list(rep(colboxes,each=3)),names=list(rep(unique(prob.C0.R$Tau),3))))

lines(x = c(0,10),y=c(0.5,0.5),col='grey',lwd=1)
#axis(side = 1,labels = rep(unique(cumRewlasts$Tau),3),at=seq(1:9))
par(las=1,xpd=T)
mtext(side = 2,text = 'Probability ',cex = 1.5,line = 3.5)
text(x=0.1,y=par("yaxp")[1]-(par("yaxp")[2]-par("yaxp")[1])/6,labels = expression(tau),cex=2)
legend('bottomright',legend=unique(prob.RV.V$Gamma),col=colboxes,pch=15,title=expression(gamma),cex=1)

dev.off()

 # Probability of a resident over nothing  -------------------------------------------------------------
 
 lastsR0<-lastSteps[lastSteps$fullRo,]
 str(lastsR0)
 lastsR0<-cbind(lastsR0,lastsR0$Type_choice<2)
 names(lastsR0)[55]<-'Client_chosen'
 prob.R0.R<-aggregate(Client_chosen~Training*Tau*Gamma,data = lastsR0,FUN = mean)
 str(prob.R0.R)
 Prob.R0.tau.gamma<-as.data.frame(split(prob.R0.R$Client_chosen,
                                        list(prob.R0.R$Tau,prob.R0.R$Gamma)))
 
 png('probR0R_pres_seed3.png',width=1000,height=700)
 
 par(plt=posPlot(),xaxt='s',yaxt='s',las=1)
 vio<-do.call(vioplot,c(unname(Prob.R0.tau.gamma),
                        col=list(rep(colboxes,each=3)),names=list(rep(unique(prob.R0.R$Tau),3))))
 
 lines(x = c(0,10),y=c(0.5,0.5),col='grey',lwd=1)
 #axis(side = 1,labels = rep(unique(cumRewlasts$Tau),3),at=seq(1:9))
 par(las=1,xpd=T)
 mtext(side = 2,text = 'Probability ',cex = 1.5,line = 3.5)
 text(x=0.1,y=par("yaxp")[1]-(par("yaxp")[2]-par("yaxp")[1])/6,labels = expression(tau),cex=2)
 legend('bottomright',legend=unique(prob.R0.R$Gamma),col=colboxes,pch=15,title=expression(gamma),cex=1)
 
 dev.off()
 
 # Probability of a visitor over nothing  -------------------------------------------------------------
 
 lastsV0<-lastSteps[lastSteps$fullZeroOpt,]
 str(lastsV0)
 lastsV0<-cbind(lastsV0,lastsV0$Type_choice<2)
 names(lastsV0)[55]<-'Client_chosen'
 prob.V0.V<-aggregate(Client_chosen~Training*Tau*Gamma,data = lastsV0,FUN = mean)
 str(prob.V0.V)
 Prob.V0.tau.gamma<-as.data.frame(split(prob.V0.V$Client_chosen,
                                        list(prob.V0.V$Tau,prob.V0.V$Gamma)))
 
 
 png('probV0V_pres_seed3.png',width=1000,height=700)
 par(plt=posPlot(),xaxt='s',yaxt='s',las=1)
 vio<-do.call(vioplot,c(unname(Prob.V0.tau.gamma),
                        col=list(rep(colboxes,each=3)),names=list(rep(unique(prob.V0.V$Tau),3))))
 
 lines(x = c(0,10),y=c(0.5,0.5),col='grey',lwd=1)
 #axis(side = 1,labels = rep(unique(cumRewlasts$Tau),3),at=seq(1:9))
 par(las=1,xpd=T)
 mtext(side = 2,text = 'Probability ',cex = 1.5,line = 3.5)
 text(x=0.1,y=par("yaxp")[1]-(par("yaxp")[2]-par("yaxp")[1])/6,labels = expression(tau),cex=2)
 legend('bottomright',legend=unique(prob.V0.V$Gamma),col=colboxes,pch=15,title=expression(gamma),cex=1)
 dev.off()
 
 # proportions of choice types -------------------------------------------------------------------------
 
 palette(terrain.colors(6,alpha = 0.7))
 
 str(fulldata)
 str(lastSteps)
 sumOptions<-aggregate(cbind(fullRVoptions,fullRR,fullVV,fullRo,fullVo,fulloo)
                       ~Tau*Gamma,data=lastSteps
                       ,FUN=mean)
 
 str(sumOptions)
 

 
  
 
 
 # Exploration -----------------------------------------------------------------------------------------------
 str(data1)
 
 gamma1 = 0; tau1 = 1
 
 tempData<-data1[data1$Gamma==gamma1 & data1$Tau==tau1,]
 tempData<-tempData[tempData$Training==0,]
 par(plt=posPlot(),xaxt='s')
 hist(c(tempData[,"value_choice"],tempData[,"value_discard"]))
 hist(tempData[,"value_choice"])
 hist(tempData[,"value_discard"])
 
 par(plt=posPlot(numploty = 3,idploty = 3),xaxt='n',las=1)
 plot(tempData$value_choice~tempData$Age,pch=20,cex=0.5,xlab='',ylab = 'Value Choice')
 points(tempData[tempData$value_choice>50,c("Age","value_choice")],cex=1,col="red")
 
 par(plt=posPlot(numploty = 3,idploty = 2),new=TRUE,xaxt='n')
 plot(tempData$Height_choice~tempData$Age,pch=20,cex=0.5,xlab='',ylab='Height 1')
 points(tempData[tempData$value_choice>50,c("Age","Height_choice")],cex=1,col="red")
 
 par(plt=posPlot(numploty = 3,idploty = 1),new=TRUE,xaxt='s')
 plot(tempData$Type_choice~tempData$Age,pch=20,cex=0.5,ylab='Type choice')
 points(tempData[tempData$value_choice>50,c("Age","Type_choice")],cex=1,col="red")
 
 str(tempData)
 
 
 str(data1)
 sum(data1$fullRVoptions)
 sum(data1$fullRVoptions & data1$Type_choice ==1) == 
   sum(data1[data1$fullRVoptions & data1$Type_choice ==1,"Neg.Reward"]==0)
 sum(data1$fullRVoptions & data1$Type_choice ==0) ==  
   sum(data1[data1$fullRVoptions & data1$Type_choice ==0,"Neg.Reward"]==-10)
 
 
 sum(data1$fullVo)
 sum(data1$fullVo & data1$Type_choice ==1)
 sum(data1$fullVo & data1$Type_choice ==2)
 sum(data1[data1$fullVo & data1$Type_choice ==2,"Neg.Reward"]==-10)
 sum(data1[data1$fullVo & data1$Type_choice ==1,"Neg.Reward"]==0)
 
 
 sum(data1[which(data1$fullRVoptions & data1$Type_choice ==1)+1,"fulloo"])/
   sum(data1$fullRVoptions & data1$Type_choice ==1)
 subset(data1[which(data1$fullRVoptions & data1$Type_choice ==1)+1,],subset = fulloo)
 sum(data1[which(data1$fullRVoptions & data1$Type_choice ==0)+1,"fulloo"])
 sum(data1[which(data1$fullRVoptions & data1$Type_choice ==1)+1,"fullRo"])
 sum(data1[which(data1$fullRVoptions & data1$Type_choice ==1)+1,"fullVo"])
 sum(data1[which(data1$fullRVoptions & data1$Type_choice ==1)+1,"fullRR"])
 sum(data1[which(data1$fullRVoptions & data1$Type_choice ==1)+1,"fullVV"])
 
 sum(data1[which(data1$fullRo & data1$Type_choice ==0)+1,"fulloo"])
 
 
 
 sum(data1$fullRo)
 sum(data1$fullRo & data1$Type_choice ==0)
 sum(data1$fullRo & data1$Type_choice ==2)
 sum(data1[data1$fullRo & data1$Type_choice ==2,"Neg.Reward"]==0)
 sum(data1[data1$fullRo & data1$Type_choice ==0,"Neg.Reward"]==0)
 
 sum(data1$fullRR)
 sum(data1$fullRR & data1$Type_choice ==0)
 sum(data1$fullRR & data1$Type_choice ==2)
 sum(data1[data1$fullRR & data1$Type_choice ==0,"Neg.Reward"]==0)
 
 sum(data1$fullVV)
 sum(data1$fullVV & data1$Type_choice ==0)
 sum(data1$fullVV & data1$Type_choice ==1)
 sum(data1[data1$fullVV & data1$Type_choice ==1,"Neg.Reward"]==-10)
 
 
 str(tempData)
 cbind(names(tempData),1:length(names(tempData)))
 meansTemp<-aggregate(tempData[,c(6,7,34:56)],by=list(tempData$Age),FUN=mean)
 SDTemp<-aggregate(tempData[,c(6,7,34:56)],by=list(tempData$Age),FUN = sd)
 str(meansTemp)
 str(SDTemp)
 
