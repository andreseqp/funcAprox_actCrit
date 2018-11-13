# ------------------------ Load data ------------------------------------------#

library("data.table")
library("jsonlite")
library("rlist")

check_create.dir<-function(folder,param,values){
  listfolders<-paste(param,values,"_",sep = "")  
  currFolders<-lapply(listfolders,dir.exists)
  if(sum(currFolders>0)){
    warning("At least one of the folders already exists \n Please check",
            immediate. = TRUE)
    print(cbind(listfolders,currFolders))
    ans<-readline("Want to continue?")
    if(substr(ans, 1, 1) == "y"){
      lapply(listfolders,dir.create)
      return(listfolders)
    }
    else{
      return(listfolders)
    }
  }else{
    lapply(listfolders,dir.create)
    return(listfolders)
  }
}

getFilelist<-# reads de list of files and filters it according to a list of parameters
             # and values of interest
  function(folder, # folder where the files are
           listparam=NULL, # list strings providing the parameters 
                           #of interest
           values=NULL # list of values matching the list in 
                      # listparam
           ){
  posAgen<-c("PIA","FIA")
  listRaw<-list.files(folder,recursive = TRUE)
  fullList<-vector("list",2)
  names(fullList)<-posAgen
  if(length(listparam)!=length(values)){
    # parameter and value lists must be the same length
    warning("Parameter list and values don't match",immediate. = TRUE)
  }
  else{
      if(is.null(listparam)){
        # if there is no list, use all the data
        paramList<-listRaw
  }
    else{
      # if there is list, filter the data
      #regExpList<-paste0(listparam,values,"_",sep="")
      for (param in unique(listparam)){
        valsparam<-values[grep(param,listparam)]
        listRaw<-do.call(list.append,lapply(paste(param,valsparam,"_",sep=""),
                                           grep,x=listRaw,value=TRUE))
      }
      
      paramList<-listRaw
    }
      for(agent in posAgen){
        # Create a list with the lists separated by type of agents
        listAgent<-grep(agent,paramList,value = TRUE)
        fullList[[agent]]<-listAgent
      }
      return(fullList)
    }
}




loadRawData<-function(filename,folder)
{
  setwd(folder)
  extPar<-strsplit(filename,split ="_/")[[1]][1]
  parVal<-as.numeric(gsub("[[:alpha:]]",extPar,replacement = ''))
  extPar<-gsub(parVal,extPar,replacement = '')
  DT<-fread(filename)
    # do.call(rbind,lapply(fullList,fread))
  DT$option<-ifelse((DT$Type_choice==1 & DT$Type_discard==0) | 
                   (DT$Type_choice==0 & DT$Type_discard==1),"RV",NA)
  DT$option<-ifelse((DT$Type_choice==0 & DT$Type_discard==0),"RR",DT$option)
  DT$option<-ifelse((DT$Type_choice==1 & DT$Type_discard==1),"VV",DT$option)
  DT$option<-ifelse((DT$Type_choice==0 & DT$Type_discard==2) | 
                   (DT$Type_choice==2 & DT$Type_discard==0),"R0",DT$option)
  DT$option<-ifelse((DT$Type_choice==1 & DT$Type_discard==2) | 
                   (DT$Type_choice==2 & DT$Type_discard==1),"V0",DT$option)
  DT$option<-ifelse((DT$Type_choice==2 & DT$Type_discard==2),"00",DT$option)
  if(length(extPar)>0){
    DT[,eval(extPar):=parVal]
  }
  return(DT)
}


getParam<-function(folder,agent,listparam=NULL,values=NULL)
{
  setwd(folder)
  irrelPar<-c("gamma","tau","neta")
  listRaw<-list.files(folder,recursive = TRUE)
  jsonsList<-grep(".json",listRaw,value = TRUE)
  indRelPar<-seq(length(listparam))
  for(param in irrelPar){
      indRelPar<-grep(param,listparam,invert = TRUE)
      listparam<-listparam[indRelPar]
      values<-values[indRelPar]
  }
  if(length(listparam)!=length(values)){
    warning("Parameter list and values don't match",immediate. = TRUE)
  }
  else{
    if(is.null(listparam)){
      finalList<-jsonsList
    }
    else{
      for (param in unique(listparam)){
        valsparam<-values[grep(param,listparam)]
        jsonsList<-do.call(list.append,lapply(paste(param,valsparam,"_",sep=""),
                                            grep,x=jsonsList,value=TRUE))
      }
    }
  }
  jsons<-do.call(list,lapply(jsonsList,fromJSON))
  return(jsons)
}

file2timeInter<-function(filename,interV,maxAge=-2)
{
  extPar<-strsplit(filename,split ="_/")[[1]][1]
  parVal<-as.numeric(gsub("[[:alpha:]]",extPar,replacement = ''))
  extPar<-gsub(parVal,extPar,replacement = '')
  tmp<-fread(filename,nrows = maxAge+1)
  tmp$fullRVoptions<-(tmp$Type_choice==1 & tmp$Type_discard==0) | 
    (tmp$Type_choice==0 & tmp$Type_discard==1)
  tmptimeInter<-
    tmp[fullRVoptions==TRUE,as.list(
      unlist(lapply(
        .SD,function(x) list(mean = mean(x),sd = sd(x))))),
      by=.(Interv=floor(Age/interV),Training,AlphaCri,AlphaAct,Gamma,Neta),
                    .SDcols=c("Type_choice",
                              grep("[[:digit:]]",names(tmp),value=TRUE))]
  if(length(extPar)>0){
    tmptimeInter[,eval(extPar):=parVal]
  }
  
  return(tmptimeInter)
}


file2timeInterValue<-function(filename,interV,maxAge=-2)
{
  extPar<-strsplit(filename,split ="_/")[[1]][1]
  parVal<-as.numeric(gsub("[[:alpha:]]",extPar,replacement = ''))
  extPar<-gsub(parVal,extPar,replacement = '')
  tmp<-fread(filename,nrows = maxAge+1)
  tmp$option<-ifelse((tmp$Type_choice==1 & tmp$Type_discard==0) | 
                      (tmp$Type_choice==0 & tmp$Type_discard==1),"RV",NA)
  tmp$option<-ifelse((tmp$Type_choice==0 & tmp$Type_discard==0),"RR",tmp$option)
  tmp$option<-ifelse((tmp$Type_choice==1 & tmp$Type_discard==1),"VV",tmp$option)
  tmp$option<-ifelse((tmp$Type_choice==0 & tmp$Type_discard==2) | 
                      (tmp$Type_choice==2 & tmp$Type_discard==0),"R0",tmp$option)
  tmp$option<-ifelse((tmp$Type_choice==1 & tmp$Type_discard==2) | 
                      (tmp$Type_choice==2 & tmp$Type_discard==1),"V0",tmp$option)
  tmp$option<-ifelse((tmp$Type_choice==2 & tmp$Type_discard==2),"00",tmp$option)
  tmptimeInter<-
    tmp[,.(value.m=mean(value),
           upIQRVal=fivenum(value)[4],
           lowIQRVal=fivenum(value)[2],
           preference.m=mean(preference),
           type_choice.m=mean(Type_choice),
           upIQRPref=fivenum(preference)[4],
           lowIQRPref=fivenum(preference)[2]),
        by=.(Interv=floor(Age/interV),
           AlphaCri,AlphaAct,Gamma,Neta,option),]
  if(length(extPar)>0){
    tmptimeInter[,eval(extPar):=parVal]
  }
  return(tmptimeInter)
}


logist<-function(x){
  return(1/(1+exp(-x)))
}

diffJsons<-function(json1,json2){
  print("JSON.1")
  print(unlist(json1)[unlist(json1)!=unlist(json2)])
  print("JSON.2")
  print(unlist(json2)[unlist(json1)!=unlist(json2)])
}


