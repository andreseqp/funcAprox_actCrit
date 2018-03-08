# ------------------ Exploration ------------------------ #

genDir<-"D:\\quinonesa\\Simulation\\functionAprox\\"

source('d:/quinonesa/Dropbox/R_files/posPlots.R')




FIAraw<-loadRawData(genDir,"FIA",listparam = NULL,values = NULL)

FIAagg<-FIAraw[, as.list(unlist(lapply(.SD, function(x) list(mean = mean(x),
                                                        sd = sd(x))))),
               by=.(Age,Alpha,Gamma,Tau,Neta,Outbr), 
               .SDcols=grep("[[:digit:]]",names(FIAraw))]

grep("[[:digit:]]",names(FIAagg),value = TRUE)

with(FIAagg,get(grep("mean",names(FIAagg),value = TRUE)[1]))

countR<-1
countC<-0
numest<-0
plot.new()
with(FIAagg[(Tau==10 & Gamma==0.8)&(Neta==0 & Outbr==0)],{
  lapply(grep("mean",names(FIAagg),value = TRUE),
         function(x){
          countC<-countC+1;
          par(plt=posPlot(numplotx = 4,numploty = 3,idplotx = countC,idploty = countR),new=TRUE);
          plot(Age,get(x),type = "l");
          title(main = x,line = -4);
          par(yaxt='n');
          if((i-3)%%4==0)
          {
            countR<-countR+1
            countC<-0
            par(yaxt='s',xaxt='n')
          }
      # polygon(c(Age,Age[length(Age):1]),
      #           c(Height_1_0.mean+Height_1_0.sd,
      #             Height_1_0.mean[length(Age):1]-Height_1_0.sd[length(Age):1]),
      #         col = colours[1],border = FALSE);
      # polygon(c(Age,Age[length(Age):1]),
      #         c(Height_1_1.mean+Height_1_1.sd,
      #           Height_1_1.mean[length(Age):1]-Height_1_0.sd[length(Age):1]),
      #         col = colours[2],border = FALSE);
      # lines(Age,Height_1_0.mean,type = "l");
      # lines(Age,Height_1_1.mean,type = "l")
         })
})
  
with(FIAraw[((Tau==10 & Gamma==0.8)&(Neta==0.5 & Outbr==0.2))&option=="RV"],{
  plot(value_choice~Age,type='p',ylim=c(min(value_choice),max(value_choice)),
       xlab='',ylab='',pch=20,cex=1,col=Type_choice+1)
})
  
