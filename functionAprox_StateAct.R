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

optionCol<-terrain.colors(6,alpha = 0.7)

# Delete data reset environment ---------------------------------------------------------

rm(list = ls())

rm(list=ls()[grepl('data.frame', sapply(ls(), function(x) class(get(x))))])


# Get files -------------------------------------------------------------------

# Laptop
setwd('E:\\Dropbox\\Neuchatel\\prelimResults\\functionAprox')

# Home
setwd('H:\\Dropbox\\Neuchatel\\prelimResults\\functionAprox')

# Office
setwd('C:\\Users\\quinonesa\\prelimResults\\functionAprox')


(listfold<-list.files())
(listfold<-listfold[grep('Ty',listfold)])
setwd(listfold[7])
getwd()
(listfiles<-list.files())
(listfiles<-listfiles[grep('IndTrain',listfiles)])
(listfiles<-listfiles[grep('bias',listfiles)])
seed<-'seed12'
(listfiles<-listfiles[grep(seed,listfiles)])
(listfiles<-listfiles[grep('test',listfiles)])

# plots directory
plotdirbase<-'C:\\Users\\quinonesa\\Dropbox\\Neuchatel\\Results\\'


# Get data -----------------------------------------------------------------------------------------------------

data1<-do.call(rbind,lapply(listfiles,read.table,header=TRUE))

# data1<-read.table(listfiles[1],header = TRUE)
# str(data1)
# for(fileN in listfiles[2:length(listfiles)])
# {
#   tempdata<-read.table(fileN,header = TRUE)
#   data1<-rbind(data1,tempdata)
# }
# rm(list =c('tempdata'))

# nameData<-read.table(listfiles[1],nrows = 1)
# str(nameData)
# colnames(data1)[1:5]<-as.matrix(nameData[1:5])
# names(data1)[6]<-'bias'
# names(data1)[7:58]<-as.matrix(nameData[6:57])


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

data1$Estimation<-rep(12,dim(data1)[1])

# Dynamic feature weights ------------------------------------------------------------------

est<-12

for(est in unique(data1$Estimation))
{

  
  seed<-paste('seed',est,sep = '')
# cbind(names(data1),1:dim(data1)[2])

getwd()
pdf(paste(plotdir,seed,"_feature_weights_.pdf",sep = ''))
for(gamma in unique(data1$Gamma)[order(unique(data1$Gamma))])
{
  for(bias in unique(data1$bias)[order(unique(data1$bias))])
  {
    tempData<-data1[(data1$Gamma==gamma & data1$bias==bias) & data1$Estimation==est,]
    meansTemp<-aggregate(tempData[,c(7,8,36:58)],by=list(tempData$Age),FUN=mean)
    SDTemp<-aggregate(tempData[,c(7,8,36:58)],by=list(tempData$Age),FUN = sd)
    plot.new()
    countR<-1
    countC<-0
    numest<-0
    ylimtemp<-c(min(meansTemp[,4:26]),max(meansTemp[,4:26]))
    names(meansTemp)[4:26]
    par(xaxt='s',yaxt='s')
    for(i in 4:26)
    {
      # i<-4
      countC<-countC+1
      par(plt=posPlot(numplotx = 5,numploty = 5,idplotx = countC,idploty = countR),new=TRUE,las=1,
          cex.main=0.5)
      plot(meansTemp[,i]~meansTemp$Group.1,type='l',xlab='',ylab = '',ylim=ylimtemp)
      polygon(y = c(meansTemp[,i]+SDTemp[,i],meansTemp[dim(meansTemp)[1]:1,i]-SDTemp[dim(SDTemp)[1]:1,i]),
              x = c(meansTemp[,1],meansTemp[dim(meansTemp)[1]:1,1]),col = colours[1],border = FALSE)
      lines(meansTemp[,i]~meansTemp$Group.1,col=1)
      lines(x=c(0,max(tempData$Age)),y=c(0,0),col="grey")
      title(main = names(meansTemp)[i],line = -4)
      par(yaxt='n')
      if((i-3)%%5==0)
      {
        countR<-countR+1
        countC<-0
        par(yaxt='s',xaxt='n')
      }
    }
    countC<-countC+1
    par(plt=posPlot(numplotx = 5,numploty = 5,idplotx = countC,idploty = countR),new=TRUE,las=1)
    plot(y=0,x=0,xlab = '',ylab = '',bty='n',xaxt='n',yaxt='n',col=0)
    mtext(1,line = -3,text = as.expression(bquote(bias==.(bias))))
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
xlimtemp<-c(min(data1[,12:46]),max(data1[,12:46]))
lineText<-c(rep(2,4),rep(0.1,8))
par(xaxt='s',yaxt='s')
for(i in 12:22)
{
  if(i>19){xlimtemp<-c(-1,2)}
  countC<-countC+1
  par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = countC,idploty = countR),new=TRUE,las=1,
      cex.main=0.5)
  distrait<-strsplit(names(data1)[i],'_')[[1]][1]
  
  hist(c(data1[data1$Type_choice==0 & data1$Estimation==est ,paste(distrait,"_choice",sep = '')],
         data1[data1$Type_discard==0 & data1$Estimation==est,paste(distrait,"_discard",sep = '')])
       ,xlim=xlimtemp,col=colours[1],xlab = '',ylab='',main = '')
  hist(c(data1[data1$Type_choice==1 & data1$Estimation==est,paste(distrait,"_choice",sep = '')],
         data1[data1$Type_discard==1 & data1$Estimation==est,paste(distrait,"_discard",sep = '')])
       ,add=TRUE,col = colours[2])
  mtext(text = distrait,side = 1,line = lineText[i-11])
  ylimtemp<-par('usr')[3:4]
  par(yaxt='n')
  if((i-11)%%4==0)
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

# Value estimation -----------------------------------------------------------


png(paste(plotdir,seed,"_dist_values.png",sep = ''),width = 1000,height = 800)

plot.new()
countR<-0
countC<-0
XlimHigh<-max(data1[,c("value_choice","value_discard")])
Xlimlow<-min(data1[,c("value_choice","value_discard")])
par(xaxt='s',las=1)
for(gamma in unique(data1$Gamma)[order(unique(data1$Gamma))])
{
  countR<-countR+1
  countC<-0
  par(yaxt='s')
  # XlimHigh<-max(data1[data1$Gamma==gamma,c("value_choice","value_discard")])
  # Xlimlow<-min(data1[data1$Gamma==gamma,c("value_choice","value_discard")])
  for(bias in unique(data1$bias)[order(unique(data1$bias))])
  {
    countC<-countC+1
    tempData<-data1[(data1$Gamma==gamma & data1$bias==bias) & data1$Estimation==est,]
    par(new=T,plt=posPlot(numplotx = 3,numploty = 3,idplotx = countC,idploty = countR))
    histTemp1<-hist(c(tempData[tempData$Type_choice==0 ,"value_choice"],
                      tempData[tempData$Type_discard==0 ,"value_discard"])
                    ,col=colours[1],xlab = '',ylab='',
                    xlim = c(Xlimlow,XlimHigh),main = '')
    histTemp2<-hist(c(tempData[tempData$Type_choice==1 ,"value_choice"],
                      tempData[tempData$Type_discard==1 ,"value_discard"])
                    ,add=TRUE,col = colours[2])
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
      text(x=Xlimlow+(XlimHigh-Xlimlow)/2,y=ylimtemp[1]-(ylimtemp[2]-ylimtemp[1])/2,labels = bias)
      if(countC==1){text(x=Xlimlow,y=ylimtemp[1]-(ylimtemp[2]-ylimtemp[1])/2,labels = 'bias',cex=2)}
    }
    par(yaxt='n',xpd=F)
  }
  par(xaxt='n')
}
legend('topleft',col = colours,legend = c('Resident','Visitor'),pch = 15)

dev.off()


# par(plt=posPlot(),xaxt='s',yaxt='s')
# hist(c(tempData[tempData$Type_choice==0,"value_choice"],
#        tempData[tempData$Type_discard==0,"value_discard"])
#      ,col=colours[1],xlab = 'Estimated value',
#      xlim = c(min(tempData[,c("value_choice","value_discard")]),
#               max(tempData[,c("value_choice","value_discard")])))
# hist(c(tempData[tempData$Type_choice==1,"value_choice"],
#        tempData[tempData$Type_discard==1,"value_discard"])
#      ,add=TRUE,col = colours[2])
# legend('topleft',col = colours,legend = c('Resident','Visitor'),pch = 15)
# 
# par(xaxt='s',yaxt='s')
# hist(data1[,"value_choice"]-data1[,"value_discard"]
#      ,col=colours[1],xlab = 'Difference in estimated value')

# Distribution of values for RV options -------------------------------------------------------

png(paste(plotdir,seed,"_dist_values_RV.png",sep = ''),width = 1000,height = 800)


plot.new()
countR<-0
countC<-0
XlimHigh<-max(data1[tempData$fullRVoptions,c("value_choice","value_discard")])
Xlimlow<-min(data1[tempData$fullRVoptions,c("value_choice","value_discard")])
par(xaxt='s',las=1)
for(gamma in unique(data1$Gamma)[order(unique(data1$Gamma))])
{
  countR<-countR+1
  countC<-0
  par(yaxt='s')
  # XlimHigh<-max(data1[data1$Gamma==gamma,c("value_choice","value_discard")])
  # Xlimlow<-min(data1[data1$Gamma==gamma,c("value_choice","value_discard")])
  for(bias in unique(data1$bias)[order(unique(data1$bias))])
  {
    countC<-countC+1
    tempData<-data1[(data1$Gamma==gamma & data1$bias==bias) & data1$Estimation == est,]
    par(new=T,plt=posPlot(numplotx = 3,numploty = 3,idplotx = countC,idploty = countR))
    histTemp1<-hist(c(tempData[tempData$Type_choice==0 & tempData$fullRVoptions,"value_choice"],
                      tempData[tempData$Type_discard==0 & tempData$fullRVoptions,"value_discard"])
                    ,col=colours[1],xlab = '',ylab='',
                    xlim = c(Xlimlow,XlimHigh),main = '')
    histTemp2<-hist(c(tempData[tempData$Type_choice==1 & tempData$fullRVoptions,"value_choice"],
                      tempData[tempData$Type_discard==1 & tempData$fullRVoptions,"value_discard"])
                    ,add=TRUE,col = colours[2])
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
      text(x=Xlimlow+(XlimHigh-Xlimlow)/2,y=ylimtemp[1]-(ylimtemp[2]-ylimtemp[1])/2,labels = bias)
      if(countC==1){text(x=Xlimlow,y=ylimtemp[1]-(ylimtemp[2]-ylimtemp[1])/2,labels = 'bias',cex=2)}
    }
    par(yaxt='n',xpd=F)
  }
  par(xaxt='n')
}
legend('topright',col = colours,legend = c('Resident','Visitor'),pch = 15)

dev.off()



# Value dynamics discriminated by choice --------------------------------------------------------------

png(paste(plotdir,seed,'_value_dynamic_choice.png',sep = ''),width = 1000,height = 800)

palette('default')
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
  ylimHigh<-max(data1[data1$Gamma==gamma & data1$Estimation==est,c("value_choice","value_discard")])
  ylimlow<-min(data1[data1$Gamma==gamma & data1$Estimation==est,c("value_choice","value_discard")])
  for(bias in unique(data1$Bias)[order(unique(data1$Bias))])
  {
    countC<-countC+1
    tempData<-data1[(data1$Gamma==gamma & data1$Bias==bias) & data1$Estimation==est,]
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
      text(x=(XlimHigh-Xlimlow)/2,y=ylimlow-(ylimHigh-ylimlow)/2,labels = bias)
      if(countC==1){text(x=Xlimlow,y=ylimlow-(ylimHigh-ylimlow)/2,labels = 'bias',cex=2)}
    }
    par(yaxt='n',xpd=F)
  }
  par(xaxt='n')
}
# par(plt=posPlot())
legend('topleft',col =c(1,2,3),legend = c('resident','visitor','absence'),pch =20,
       ncol = 3,cex = 0.8)

dev.off()

# # Value dynamics disc. by choice -------------------------------------------------------------------
# 
# png(paste('value_dynamic_CD_',seed,'.png',sep = ''),width = 1000,height = 800)
# 
# plot.new()
# countR<-0
# countC<-0
# XlimHigh<-max(data1$Age)
# Xlimlow<-0
# cex1<-0.2
# par(xaxt='s',las=1)
# for(gamma in unique(data1$Gamma)[order(unique(data1$Gamma))])
# {
#   countR<-countR+1
#   countC<-0
#   par(yaxt='s')
#   ylimHigh<-max(data1[data1$Gamma==gamma,c("value_choice","value_discard")])
#   ylimlow<-min(data1[data1$Gamma==gamma,c("value_choice","value_discard")])
#   for(bias in unique(data1$bias)[order(unique(data1$bias))])
#   {
#     countC<-countC+1
#     tempData<-data1[data1$Gamma==gamma & data1$Tau==tau,]
#     par(new=T,plt=posPlot(numplotx = 3,numploty = 3,idplotx = countC,idploty = countR))
#     plot(tempData[,'value_choice']~
#            tempData[,'Age'],type='p',ylim=c(ylimlow,ylimHigh),
#          xlab='',ylab='',pch=20,cex=cex1,
#          col=1)
#     points(tempData[,'value_discard']~
#              tempData[,'Age'],pch=20,cex=cex1,
#            col=2)
#     if(countC==1)
#     {
#       
#       par(xpd=T)
#       text(x=-(XlimHigh-Xlimlow)/2,y=ylimHigh-(ylimHigh-ylimlow)/2,labels = gamma)
#       if(countR==3){text(x=-(XlimHigh-Xlimlow)/2,y=ylimHigh,labels = expression(gamma),cex=2)}
#     }
#     
#     if(countR==1)
#     {
#       par(xpd=T)
#       text(x=(XlimHigh-Xlimlow)/2,y=ylimlow-(ylimHigh-ylimlow)/2,labels = bias)
#       if(countC==1){text(x=Xlimlow,y=ylimlow-(ylimHigh-ylimlow)/2,labels = 'bias',cex=2)}
#     }
#     par(yaxt='n',xpd=F)
#   }
#   par(xaxt='n')
# }
# # par(plt=posPlot())
# legend('topleft',col =c(1,2,3),legend = c('Choice','discard'),pch =20,
#        ncol = 3,cex = 0.8)
# 
# dev.off()

# # Value dynamics discriminated by option --------------------------------------------------------------

png(paste(plotdir,seed,'_value_dynamic_option.png',sep = ''),width = 1000,height = 800)

  replicate<-1
  plot.new()
  countR<-0
  countC<-0
  genshow<-1
  XlimHigh<-max(data1$Age)
  Xlimlow<-0
  cex1<-0.1
  par(xaxt='s',las=1)
  for(gamma in unique(data1$Gamma)[order(unique(data1$Gamma))])
  {
    countR<-countR+1
    countC<-0
    par(yaxt='s')
    ylimHigh<-max(data1[data1$Gamma==gamma,c("value_choice","value_discard")])
    ylimlow<-min(data1[data1$Gamma==gamma,c("value_choice","value_discard")])
    for(bias in unique(data1$Bias)[order(unique(data1$Bias))])
    {
      countC<-countC+1
      tempData<-data1[(data1$Gamma==gamma & data1$Bias==bias) ,]
      par(new=T,plt=posPlot(numplotx = 3,numploty = 3,idplotx = countC,idploty = countR))
      plot(tempData[tempData$Age%%genshow==0,'value_choice']~
             tempData[tempData$Age%%genshow==0,'Age'],type='p',ylim=c(ylimlow,ylimHigh),
           xlab='',ylab='',pch=tempData[tempData$Age%%genshow==0,'Type_choice']+15,cex=cex1,
           col=tempData[tempData$Age%%genshow==0,'option'])
      lines(y=c(0,0),x=c(0,XlimHigh),col='grey')
      points(tempData[tempData$Age%%genshow==0,'value_discard']~
               tempData[tempData$Age%%genshow==0,'Age'],
             pch=tempData[tempData$Age%%genshow==0,"Type_discard"]+15,cex=cex1,
             col=tempData[tempData$Age%%genshow==0,'option'])
      points(tempData[tempData$option==1 & tempData$Age%%genshow==0 ,'value_discard']~
               tempData[tempData$option==1 & tempData$Age%%genshow==0,'Age'],
             pch=tempData[tempData$Age%%genshow==0,"Type_discard"]+15,cex=cex1,
             col=tempData[tempData$option==1 & tempData$Age%%genshow==0,'option']+
               6*tempData[tempData$option==1 &tempData$Age%%genshow==0,"Type_discard"])
      points(tempData[tempData$option==1 & tempData$Age%%genshow==0,'value_choice']~
               tempData[tempData$option==1 & tempData$Age%%genshow==0,'Age'],
             pch=tempData$Type_discard+15,cex=cex1,
             col=tempData[tempData$option==1 & tempData$Age%%genshow==0,'option']+
               6*tempData[tempData$option==1 &tempData$Age%%genshow==0 ,"Type_choice"])
      lines(tempData[(tempData$option==1 & tempData$Type_choice==1) & tempData$Training==3,
                     "DPUpdate"]~tempData[(tempData$option==1 & tempData$Type_choice==1)
                      & tempData$Training==3,"Age"],col='red',lwd=2)
      lines(tempData[(tempData$option==1 & tempData$Type_choice==0) & tempData$Training==3,
                     "DPUpdate"]~tempData[(tempData$option==1 & tempData$Type_choice==0)
                                          & tempData$Training==3,"Age",]
            ,col='red',lwd=2)
      if(countC==1)
      {
  
        par(xpd=T)
        text(x=-(XlimHigh-Xlimlow)/2,y=ylimHigh-(ylimHigh-ylimlow)/2,labels = gamma)
        if(countR==3){text(x=-(XlimHigh-Xlimlow)/2,y=ylimHigh,labels = expression(gamma),cex=2)}
      }
  
      if(countR==1)
      {
        par(xpd=T)
        text(x=(XlimHigh-Xlimlow)/2,y=ylimlow-(ylimHigh-ylimlow)/2,labels = bias)
        if(countC==1){text(x=Xlimlow,y=ylimlow-(ylimHigh-ylimlow)/2,labels = 'bias',cex=2)}
      }
      par(yaxt='n',xpd=F)
    }
    par(xaxt='n')
  }
  
  legend('topleft',col =c(1,7,2:6),legend = c('RV.R','RV.V','RR','VV','R0','V0','00'),pch = 15,ncol = 4,cex = 0.8)

dev.off()




par(plt=posPlot(),xaxt='s',yaxt='s')
plot(data1[,'value_choice']-data1[,'value_discard']~
       data1[,'Age'],type='p',pch=20,cex=0.5)
lines(y=c(0,0),x=c(0,1000),col='grey')




# Probability of choosing a visitor -------------------------------------------------------------

lastsRV<-lastSteps[lastSteps$fullRVoptions ,]
str(lastsRV)
prob.RV.V<-aggregate(Type_choice~Training*bias*Gamma,data = lastsRV,FUN = mean)
str(prob.RV.V)
Prob.bias.gamma<-as.data.frame(split(prob.RV.V$Type_choice,
                                    list(prob.RV.V$bias,prob.RV.V$Gamma)))


png(paste(plotdir,seed,'_probRVV.png',sep = ''),width=1000,height=700)

par(plt=posPlot(),xaxt='s',yaxt='s',las=1)
vio<-do.call(vioplot,c(unname(Prob.bias.gamma),
                       col=list(rep(colboxes,each=3)),names=list(rep(unique(prob.RV.V$bias),3))))

lines(x = c(0,10),y=c(0.5,0.5),col='grey',lwd=1)
#axis(side = 1,labels = rep(unique(cumRewlasts$Tau),3),at=seq(1:9))
par(las=1,xpd=T)
mtext(side = 2,text = 'Probability ',cex = 1.5,line = 3.5)
text(x=0.1,y=par("yaxp")[1]-(par("yaxp")[2]-par("yaxp")[1])/6,labels = 'bias',cex=2)
legend('bottomright',legend=unique(prob.RV.V$Gamma),col=colboxes,pch=15,title=expression(gamma),cex=1)

dev.off()

# Probability of a client over nothing  -------------------------------------------------------------

lastsC0<-lastSteps[lastSteps$fullZeroOpt ,]
str(lastsC0)
unique(lastsC0$Estimation)
lastsC0<-cbind(lastsC0,lastsC0$Type_choice<2)
names(lastsC0)[90]<-'Client_chosen'
prob.C0.R<-aggregate(Client_chosen~Training*Bias*Gamma,data = lastsC0,FUN = mean)
str(prob.C0.R)
Prob.C0.bias.gamma<-as.data.frame(split(prob.C0.R$Client_chosen,
                                       list(prob.C0.R$Bias,prob.C0.R$Gamma)))


png(paste(plotdir,seed,'_probC0C.png',sep = ''),width=1000,height=700)

par(plt=posPlot(),xaxt='s',yaxt='s',las=1)
vio<-do.call(vioplot,c(unname(Prob.C0.bias.gamma),
                       col=list(rep(colboxes,each=3)),names=list(rep(unique(prob.C0.R$Bias),3))))

lines(x = c(0,10),y=c(0.5,0.5),col='grey',lwd=1)
#axis(side = 1,labels = rep(unique(cumRewlasts$Tau),3),at=seq(1:9))
par(las=1,xpd=T)
mtext(side = 2,text = 'Probability ',cex = 1.5,line = 3.5)
text(x=0.1,y=par("yaxp")[1]-(par("yaxp")[2]-par("yaxp")[1])/6,labels = 'bias',cex=2)
legend('bottomright',legend=unique(prob.RV.V$Gamma),col=colboxes,pch=15,title=expression(gamma),cex=1)

dev.off()

# # Probability of a resident over nothing  -------------------------------------------------------------

lastsR0<-lastSteps[lastSteps$fullRo,]
str(lastsR0)
lastsR0<-cbind(lastsR0,lastsR0$Type_choice<2)
names(lastsR0)[66]<-'Client_chosen'
prob.R0.R<-aggregate(Client_chosen~Training*Tau*Gamma,data = lastsR0,FUN = mean)
str(prob.R0.R)
Prob.R0.tau.gamma<-as.data.frame(split(prob.R0.R$Client_chosen,
                                       list(prob.R0.R$Tau,prob.R0.R$Gamma)))

png(paste(plotdir,seed,'_probR0R.png'),width=1000,height=700)

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

# # Probability of a visitor over nothing  -------------------------------------------------------------
# 
# lastsV0<-lastSteps[lastSteps$fullZeroOpt,]
# str(lastsV0)
# lastsV0<-cbind(lastsV0,lastsV0$Type_choice<2)
# names(lastsV0)[66]<-'Client_chosen'
# prob.V0.V<-aggregate(Client_chosen~Training*Tau*Gamma,data = lastsV0,FUN = mean)
# str(prob.V0.V)
# Prob.V0.tau.gamma<-as.data.frame(split(prob.V0.V$Client_chosen,
#                                        list(prob.V0.V$Tau,prob.V0.V$Gamma)))
# 
# 
# png(paste(plotdir,seed,'_probV0V.png'),width=1000,height=700)
# 
# par(plt=posPlot(),xaxt='s',yaxt='s',las=1)
# vio<-do.call(vioplot,c(unname(Prob.V0.tau.gamma),
#                        col=list(rep(colboxes,each=3)),names=list(rep(unique(prob.V0.V$Tau),3))))
# 
# lines(x = c(0,10),y=c(0.5,0.5),col='grey',lwd=1)
# #axis(side = 1,labels = rep(unique(cumRewlasts$Tau),3),at=seq(1:9))
# par(las=1,xpd=T)
# mtext(side = 2,text = 'Probability ',cex = 1.5,line = 3.5)
# text(x=0.1,y=par("yaxp")[1]-(par("yaxp")[2]-par("yaxp")[1])/6,labels = expression(tau),cex=2)
# legend('bottomright',legend=unique(prob.V0.V$Gamma),col=colboxes,pch=15,title=expression(gamma),cex=1)
# 
# dev.off()

# proportions of choice types -------------------------------------------------------------------------


str(lastSteps)
sumOptions<-aggregate(cbind(fullRVoptions,fullRR,fullVV,fullRo,fullVo,fulloo)~Bias*Gamma,data=lastSteps
                      ,FUN=mean)

str(sumOptions)

# library('plotrix')
# library('reshape')
png(paste(plotdir,seed,'_options.png',sep = ''),width=1000,height=800)

barHeights<-rbind(rep(0,9),rep(0,9),t(as.matrix(sumOptions))[3:8,])
par(plt=posPlot(),xaxt='s',yaxt='n',xpd=T,las=1)
barP<-barplot(barHeights[3:8,],col=optionCol[c(2:6,1)])
# barP<-barplot(t(as.matrix(sumOptions))[3:4,],col=c(2:3),beside = FALSE)
# dispersion(as.vector(barP),as.vector(t(as.matrix(sumOptionsMean))[3:6,]),as.vector(t(as.matrix(sumOptionsMean))[3:6,])
#            ,intervals = TRUE)
for(i in c(3,4,6,7,8))
{
  text(x = barP,y=(apply(barHeights[1:i,],MARGIN=2,FUN = sum)+
                     apply(barHeights[1:(i-1),],MARGIN=2,FUN = sum))/2,
       labels = round(barHeights[i,]*100,1))
}
loctau<-par('usr')[3] - 1 * diff(grconvertY(0:1, 'inches', 'user'))* 
  par('cin')[1] * 2 * par('lheight')
locgamma<-par('usr')[3] - 2 * diff(grconvertY(0:1, 'inches', 'user'))* 
  par('cin')[1] * 2 * par('lheight')
axis(side = 1,labels = t(as.matrix(sumOptions))[1,],at=barP)
axis(side = 1,labels = t(as.matrix(sumOptions))[2,],at=barP,line = 2,lwd = 0)
text(x=barP[1]-(barP[2]-barP[1]),y=loctau,labels = 'bias',cex=2)
text(x=barP[1]-(barP[2]-barP[1]),y=locgamma,labels = expression(gamma),cex=2)
legend(x=-barP[2],y=0.65,legend=c('RV','RR','VV','R0','V0','00')[6:1],col=optionCol[c(1,6:2)],pch=15,bg = 'gray83',cex=1)

dev.off()

}

# Dynamical programming to estimate value ---------------------------------------------------------

gamma1 <- 0.8; bias <- 0.2;train <-3
tempdata<-data1[data1$Gamma==gamma1 & data1$Bias == bias,]
par(xaxt='s',yaxt='s',plt=posPlot())
plot(DPUpdate~Age,type='l',col=Type_choice+1,
     data=tempdata[tempdata$Training==1 & (tempdata$fullRVoptions & tempdata$Type_choice==0),]
     ,ylim=c(0,35))
lines(DPUpdate~Age,col=Type_choice+1,
      data=tempdata[tempdata$Training==train & (tempdata$fullRVoptions & tempdata$Type_choice==1),])

plot(DPUpdate~Age,type='l',col=Type_choice+1,
     data=tempdata[tempdata$Training==train & (tempdata$fullVo & tempdata$Type_choice==1),]
     ,ylim=c(0,35))
lines(DPUpdate~Age,col=Type_choice+1,
      data=tempdata[tempdata$Training==train & (tempdata$fullVo & tempdata$Type_choice==2),])

plot(DPUpdate~Age,type='l',col=Type_choice+1,
     data=tempdata[tempdata$Training==train & (tempdata$fullRo & tempdata$Type_choice==0),]
     ,ylim=c(0,35))
lines(DPUpdate~Age,col=Type_choice+1,
      data=tempdata[tempdata$Training==train & (tempdata$fullRo & tempdata$Type_choice==2),])



# Exploration -----------------------------------------------------------------------------------------------
str(data1)

gamma1 = 0.8; tau1 = 5

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


sum(tempData$value_choice>50)
sum(tempData$value_discard>50)
which(tempData$value_choice>50)
which(tempData$fullVV)
tempData[tempData$fullVV,c("value_choice","value_discard")]




str(tempData)
cbind(names(tempData),1:length(names(tempData)))
meansTemp<-aggregate(tempData[,c(6,7,34:56)],by=list(tempData$Age),FUN=mean)
SDTemp<-aggregate(tempData[,c(6,7,34:56)],by=list(tempData$Age),FUN = sd)
str(meansTemp)
str(SDTemp)

par(plt=posPlot(),xaxt='s',yaxt='s')
hist(tempData[tempData$fullVo,"value_choice"],col = colours[1])
hist(tempData[tempData$fullVo,"value_discard"],add=TRUE,col = colours[2])

grpHei<-tempData[tempData$fullVo & tempData$value_choice>15,]

