# ------------------------ Load data ------------------------------------------#

library("data.table")
library("jsonlite")


getFilelist<-function(folder,listparam=NULL,values=NULL)
{
  posAgen<-c("PIA","FIA","DP")
  listRaw<-list.files(folder,recursive = TRUE)
  fullList<-vector("list",3)
  names(fullList)<-posAgen
  if(length(listparam)!=length(values)){
    warning("Parameter list and values don't match",immediate. = TRUE)
  }
  else{
      if(is.null(listparam)){
        paramList<-listRaw
      }
    else{
      regExpList<-paste0(listparam,values,"_",sep="")
      paramList<-do.call(c,lapply(regExpList,grep,listRaw,value=TRUE))
    }
      for(agent in posAgen){
        listAgent<-grep(agent,paramList,value = TRUE)
        fullList[[agent]]<-listAgent
      }
      return(fullList)
    }
}

loadRawData<-function(folder,agent,listparam,values)
{
  setwd(folder)
  fullList<-getFilelist(folder,listparam,values)
  DT<-do.call(rbind,lapply(fullList[[agent]],fread))
  DT$option<-ifelse((DT$Type_choice==1 & DT$Type_discard==0) | 
                   (DT$Type_choice==0 & DT$Type_discard==1),"RV",NA)
  DT$option<-ifelse((DT$Type_choice==0 & DT$Type_discard==0),"RR",DT$option)
  DT$option<-ifelse((DT$Type_choice==1 & DT$Type_discard==1),"VV",DT$option)
  DT$option<-ifelse((DT$Type_choice==0 & DT$Type_discard==2) | 
                   (DT$Type_choice==2 & DT$Type_discard==0),"R0",DT$option)
  DT$option<-ifelse((DT$Type_choice==1 & DT$Type_discard==2) | 
                   (DT$Type_choice==2 & DT$Type_discard==1),"V0",DT$option)
  DT$option<-ifelse((DT$Type_choice==2 & DT$Type_discard==2),"00",DT$option)
  
  return(DT)
}


getParam<-function(folder,agent,listparam,values)
{
  setwd(folder)
  irrelPar<-c("Gamma","Tau","Neta")
  listRaw<-list.files(folder,recursive = TRUE)
  jsonsList<-grep(".json",listRaw,value = TRUE)
  indRelPar<-do.call(c,lapply(irrelPar,grep,listparam,invert=TRUE))
  listparam<-listparam[indRelPar]
  values<-values[indRelPar]
  if(length(listparam)!=length(values)){
    warning("Parameter list and values don't match",immediate. = TRUE)
  }
  else{
    if(is.null(listparam)){
      finalList<-jsonsList
    }
    else{
      regExpList<-paste0(listparam,values,"_",sep="")
      finalList<-do.call(c,lapply(regExpList,grep,jsonsList,value=TRUE))
    }
  }
  jsons<-do.call(list,lapply(finalList,fromJSON))
  return(jsons)
}

file2timeInter<-function(filename,interV)
{
  tmp<-fread(filename)
  tmp$fullRVoptions<-(tmp$Type_choice==1 & tmp$Type_discard==0) | 
    (tmp$Type_choice==0 & tmp$Type_discard==1)
  tmptimeInter<-
    tmp[fullRVoptions==TRUE,as.list(
      unlist(lapply(
        .SD,function(x) list(mean = mean(x),sd = sd(x))))),
      by=.(Interv=floor(Age/interV),Training,Age,Alpha,Gamma,Tau,Neta,Outbr),
                    .SDcols=c("Type_choice",
                              grep("[[:digit:]]",names(tmp),value=TRUE))]
  return(tmptimeInter)
}

tmptimeInter<-
  tmp[fullRVoptions==TRUE,as.list(
    unlist(lapply(
      .SD,function(x) list(mean = mean(x),sd = sd(x))))),
    by=.(Interv=floor(Age/1001),Training,Age,Alpha,Gamma,Tau,Neta,Outbr),
    .SDcols=c("Type_choice",grep("[[:digit:]]",names(tmp),value = TRUE))]

filename<-getFilelist(genDir)$FIA[1]

# loadInterData<-function(folder,agent,listparam,values)
# {
#   fullList<-getFilelist(folder,listparam,values)
#   return(do.call(rbind,lapply(fullList[[agent]],fread)))
# }

