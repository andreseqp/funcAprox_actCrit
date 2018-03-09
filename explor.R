# ------------------ Exploration ------------------------ #

genDir<-"D:\\quinonesa\\Simulation\\functionAprox\\"

source('d:/quinonesa/Dropbox/R_files/posPlots.R')
source("loadData.R")




FIAraw<-loadRawData(genDir,"FIA",listparam = NULL,values = NULL)

FIAagg<-FIAraw[, as.list(unlist(lapply(.SD, function(x) list(mean = mean(x),
                                                        sd = sd(x))))),
               by=.(Age,Alpha,Gamma,Tau,Neta,Outbr), 
               .SDcols=grep("[[:digit:]]",names(FIAraw))]

length(grep("[[:digit:]]",names(FIAagg),value = TRUE))

with(FIAagg,get(grep("mean",names(FIAagg),value = TRUE)[1]))



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
    plot(c(min(Age),max(Age)),rep(0,2),type = "l",xlab = '',ylab='',ylim=ylimtemp,col="grey")
    
    polygon(c(Age,Age[length(Age):1]),
              c(get(feat)+get(grep("_0.sd",names(FIAagg),value = TRUE)[i]),
                get(feat)[length(Age):1]-
                  get(grep("_0.sd",names(FIAagg),value = TRUE)[i])[length(Age):1]),
            col = colours[1],border = FALSE)
    polygon(c(Age,Age[length(Age):1]),
            c(get(grep("_1.mean",names(FIAagg),value = TRUE)[i])+
                    get(grep("_1.sd",names(FIAagg),value = TRUE)[i]),
              get(grep("_1.mean",names(FIAagg),value = TRUE)[i])[length(Age):1]-
                get(grep("_1.sd",names(FIAagg),value = TRUE)[i])[length(Age):1]),
            col = colours[2],border = FALSE);
    lines(Age,get(feat),type = "l")
    lines(Age,get(grep("_1.mean",names(FIAagg),value = TRUE)[i]),type = "l")
    # title(main = feat,line = -4)
    legend('bottomleft',legend = c(feat,
                                   grep("_1.mean",names(FIAagg),value=TRUE)[i])
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
  
par(plt=posPlot(),xaxt='s',yaxt='s')
with(FIAraw[((Tau==2 & Gamma==0)&(Neta==0 & Outbr==0))&option=="RV"],{
  plot(value_choice~Age,type='p',ylim=c(min(value_choice),max(value_choice)),
       xlab='',ylab='',pch=20,cex=1,col=Type_choice+1)
})
  
