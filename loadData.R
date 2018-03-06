# ------------------------ Load data ------------------------------------------#

library("data.table")

genDir<-"D:\\quinonesa\\Simulation\\functionAprox\\"

setwd(genDir)

listgen<-list.files(recursive = TRUE)



rawdata<-do.call(rbind,lapply(listTrain, fread))

getFilelist<-function(folder,agent,listparam,values)
{
  if(length(listparam)!=length(values)){
    warning("Parameter list and values don't match")
  }
  else{
  listRaw<-list.files(folder,recursive = TRUE)
  listAgent<-grep(agent,listRaw,value = TRUE)
  regExpList<-paste0(listparam,values,"_",sep="")
  finalList<-do.call(c,lapply(regExpList,grep,listAgent,value=TRUE))
  return(finalList)
  }
}

with(rawdata[(Gamma==0.8 & Tau==10) & Training==1],{
  plot(x=Age,y=Height_1)  
})



