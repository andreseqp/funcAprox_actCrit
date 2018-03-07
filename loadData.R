# ------------------------ Load data ------------------------------------------#

library("data.table")

genDir<-"D:\\quinonesa\\Simulation\\functionAprox\\"

setwd(genDir)

listgen<-list.files(recursive = TRUE)

getFilelist<-function(folder,agent,listparam,values)
{
  posAgen<-c("PIA","FIA","DP")
  if(sum(agent==posAgen)==0)
  {warning("Incorrect agent name",immediate. = TRUE)}
  else{
    if(length(listparam)!=length(values)){
      warning("Parameter list and values don't match",immediate. = TRUE)
    }
    else{
    listRaw<-list.files(folder,recursive = TRUE)
    listAgent<-grep(agent,listRaw,value = TRUE)
    regExpList<-paste0(listparam,values,"_",sep="")
    finalList<-do.call(c,lapply(regExpList,grep,listAgent,value=TRUE))
    return(finalList)
    }
  }
}


getFilelist(folder = genDir,agent = "FIA","gamma",0.8)

rawdata<-do.call(rbind,lapply(listTrain, fread))


