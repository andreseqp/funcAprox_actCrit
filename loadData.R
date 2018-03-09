# ------------------------ Load data ------------------------------------------#

library("data.table")


getFilelist<-function(folder,listparam,values)
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

# loadInterData<-function(folder,agent,listparam,values)
# {
#   fullList<-getFilelist(folder,listparam,values)
#   return(do.call(rbind,lapply(fullList[[agent]],fread)))
# }

