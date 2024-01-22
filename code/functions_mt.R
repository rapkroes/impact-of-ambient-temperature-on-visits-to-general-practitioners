#############functions#############functions#############functions#############functions#############functions#############functions#############functions#############functions#############functions#############functions#############functions#############functions#############functions

#icd10.to.class: finds the disease/ injury classification of an icd10 vector as listed in the preregistration
#suspicious.Diag.entries: Since the icd10 field is a text field in the GP's software, the data might contain typing errors or atypical notation of diseases. This function returns a vector that filters out whose entries are =1 if they are 'suspicious' and 0 otherwise. An entry is considered suspicious if it does not have a length of 4, 6, or 7 characters. If it has 4, 6, or 7 characters it is suspicious if it does not follow either of the patterns 
  #1. capital letter, two numbers, capital letter
  #2. capital letter, two numbers, fullstop, one or two numbers, capital letter
#


icd10.to.class<- function(icd10vec){
  #finds the disease/ injury classification of an icd10 vector as listed in the preregistration
  first<- substr(icd10vec,1,1)
  no<- as.numeric(substr(icd10vec,2,3))
  out<- numeric(length(icd10vec))
  
  for(i in seq_along(out)){
    selector<- which(first[i]== classification.matrix$letter & no[i]>=classification.matrix$start_no& no[i]<=classification.matrix$end_no)
    if(length(selector)==0){
      out[i]<- 11
    }else if(length(selector)==1){
      out[i]<- classification.matrix$class[selector]
    }else{
      stop(paste("For entry",i, "selector has a length longer than 1."))
    }
  }
  
  return(out)
}


suspicious.Diag.entries<- function(icd10vec){
  #Since the icd10 field is a text field in the GP's software, the data might contain typing errors or atypical notation of diseases. This function returns a vector that filters out whose entries are =1 if they are 'suspicious' and 0 otherwise. An entry is considered suspicious if it does not have a length of 3, 5, or 6 characters. If it has 3, 5, or 6 characters it is suspicious if it does not follow either of the patterns 
  #1. capital letter, two numbers, capital letter
  #2. capital letter, two numbers, fullstop, one or two numbers, capital letter
  first<- substr(icd10vec,1,1)
  no<- as.numeric(substr(icd10vec,2,3))
  out<- numeric(length(icd10vec))
  
  for(i in seq_along(icd10vec)){
    entry<- icd10vec[i]
    nc<- nchar(entry)
    
    pattern_1<- grepl("^[A-Z]\\d{2}[A-Z]$", entry)
    pattern_2<- grepl("^[A-Z]\\d{2}\\.\\d{1,2}[A-Z]$", entry)
    has.comma.or.minus<- grepl("[,\\-]", entry)
    
    if(!(nc %in% c(3,5,6))){
      out[i]<- 1
    }else{
      if(!(pattern_1|pattern_2|has.comma.or.minus)){
        out[i]<- 1
      }
    }
  }
  return(out)
}


TG_DateNum2date<- function(TG_DateNum){
  #takes a vector of TG_DateNum dates and returns them as yyyy-mm-dd dates
  as.Date(TG_DateNum, origin = "0000-01-01")
}


TG_DateNum2dow<- function(tgdatenumvec){
  #takes a vector of TG_DateNum dates and returns them as their respective days of the week as ordered factor variable
  out<- wday(TG_DateNum2date(tgdatenumvec), label = TRUE)
}

TG_DateNum2week<- function(tgdatenumvec){
  #takes a vector of TG_DateNum dates and returns them as their respective week within the year according to ISO 8601 as factor variable
  out<- as.factor(isoweek(TG_DateNum2date(tgdatenumvec)))
}

TG_DateNum2month<- function(tgdatenumvec){
  #takes a vector of TG_DateNum dates and returns them as their respective month as ordered factor variable
  out<- month(TG_DateNum2date(tgdatenumvec), label = TRUE)
}

TG_DateNum2year<- function(tgdatenumvec){
  #takes a vector of TG_DateNum dates and returns them as their respective month as ordered factor variable
  out<- as.factor(isoyear(TG_DateNum2date(tgdatenumvec)))
}

date2TG_DateNum<- function(date) {
  #transforms a date (yyyy-mm-dd) into a Matlab Serial time code (like TG_DateNum)
  as.numeric(as.Date(date)) +719528
}

chunk.data<- function(df,  no.splits){
  # splits a data frame into several data frames. They are returned as a list. The data is split by the uniPatID variable.
  # no.splits is the number in how many separate data frames the original data frame df is split.
  v<- as.factor(df$uniPatID)
  v_2<- levels(v)
  l<- length(v_2)
  
  f<- ceiling(l/no.splits)
  allocation<- rep(seq(1,no.splits),f)[1:l]
  
  out<- list()
  for (i in seq(1,no.splits)) {
    out[[i]]<- df[v %in% v_2[allocation==i],]
  }
  return(out)
}

chunk.adddata<- function(chunkeddl, adddata){
  #Takes the result of the chunk.data-function and another dataset and chunks the other dataset in the same way as the chunked dataset.
  l<- length(chunkeddl)
  adddata$uniPatID<- as.character(adddata$uniPatID)
  chunked.adddata<- list()
  for(i in seq(1,l)){
    ids<- as.character(chunkeddl[[i]]$uniPatID)
    chunked.adddata[[i]]<- adddata|>
      filter(uniPatID %in% ids)
  }
  return(chunked.adddata)
}

add.stamm.new.par<- function(edf,sdf, no.splits,no.workers){
  #parallelised version of add.stamm.new
  dl_e<- chunk.data(edf,no.splits)
  dl_stamm<- chunk.adddata(dl_e,sdf)
  par.cl<- makeCluster(no.workers)
  dist.env<- environment()
  clusterExport(par.cl,varlist = c("dl_e","dl_stamm","filter","arrange"), envir = dist.env)
  
  result<- parLapply(par.cl,1:no.splits,function(k){
    episodedf<- dl_e[[k]]
    stamm<- dl_stamm[[k]]
    n<- nrow(episodedf)
    im<- as.data.frame(matrix(NA,nrow = n, ncol = ncol(stamm)))
    colnames(im)<- colnames(stamm)
    stamm.is.there<- numeric(n)
    for(i in seq(1,n)){
      pat<- episodedf$uniPatID[i]
      start_date<- episodedf$start_date[i]
      patient.stamm<- stamm|>
        filter(uniPatID==pat)|>
        arrange(TG_DateNum)
      if(nrow(patient.stamm)==1){
        stamm.is.there[i]<- 1
        im[i,]<- patient.stamm
      }else if(nrow(patient.stamm)>1){
        stamm.is.there[i]<- 1
        before.episode.start<- patient.stamm$TG_DateNum<=start_date
        if(all(before.episode.start)){
          im[i,]<- patient.stamm[nrow(patient.stamm),]
        }else if(!all(before.episode.start)){
          im[i,]<- patient.stamm[1,]
        }else{
          im[i,]<- patient.stamm[max(which(before.episode.start)),]
        }
      }
    }
    col.selector<- colnames(im)!="uniPatID" & colnames(im)!="TG_DateNum"
    im<- im[,col.selector]
    out<- cbind(episodedf,im,stamm.is.there)
    colnames(out)<- c(colnames(episodedf),colnames(im),"stamm.is.there")
    
    return(out)
  })
  out<- as.data.frame(matrix(NA, nrow = nrow(edf), ncol = length(colnames(result[[1]]))))
  colnames(out)<- colnames(result[[1]])
  ticker<- 1
  for(j in seq(1,length(result))){
    out[seq(ticker,ticker-1+nrow(result[[j]])),]<- result[[j]]
    ticker<- ticker+nrow(result[[j]])
  }
  return(out)
}

add.konsul<- function(edf,sdf, no.splits,no.workers){
  #slightly adjusted version of add.stamm.new.par
  dl_e<- chunk.data(edf,no.splits)
  dl_stamm<- chunk.adddata(dl_e,sdf)
  par.cl<- makeCluster(no.workers)
  dist.env<- environment()
  clusterExport(par.cl,varlist = c("dl_e","dl_stamm","filter","arrange"), envir = dist.env)
  
  result<- parLapply(par.cl,1:no.splits,function(k){
    episodedf<- dl_e[[k]]
    stamm<- dl_stamm[[k]]
    n<- nrow(episodedf)
    im<- as.data.frame(matrix(NA,nrow = n, ncol = ncol(stamm)))
    colnames(im)<- colnames(stamm)
    stamm.is.there<- numeric(n)
    for(i in seq(1,n)){
      pat<- episodedf$uniPatID[i]
      start_date<- episodedf$start_date[i]
      patient.stamm<- stamm|>
        filter(uniPatID==pat)|>
        arrange(TG_DateNum)
      if(nrow(patient.stamm)==1){
        stamm.is.there[i]<- 1
        im[i,]<- patient.stamm
      }else if(nrow(patient.stamm)>1){
        stamm.is.there[i]<- 1
        before.episode.start<- patient.stamm$TG_DateNum<=start_date
        if(all(before.episode.start)){
          im[i,]<- patient.stamm[nrow(patient.stamm),]
        }else if(!all(before.episode.start)){
          im[i,]<- patient.stamm[1,]
        }else{
          im[i,]<- patient.stamm[max(which(before.episode.start)),]
        }
      }
    }
    col.selector<- colnames(im)!="uniPatID" & colnames(im)!="TG_DateNum"
    im<- im[,col.selector]
    out<- cbind(episodedf,im,stamm.is.there)
    colnames(out)<- c(colnames(episodedf),colnames(im),"konsul.is.there")
    
    return(out)
  })
  out<- as.data.frame(matrix(NA, nrow = nrow(edf), ncol = length(colnames(result[[1]]))))
  colnames(out)<- colnames(result[[1]])
  ticker<- 1
  for(j in seq(1,length(result))){
    out[seq(ticker,ticker-1+nrow(result[[j]])),]<- result[[j]]
    ticker<- ticker+nrow(result[[j]])
  }
  return(out)
}

praxisID2location<- function(praxisID){
  l<- length(praxisID)
  out<- numeric(length = l)
  for(i in seq_along(praxisID)){
    if(praxisID[i]==1 | praxisID[i]==2){
      out[i]<- "baiersbronn"
    }else if(praxisID[i]==3){
      out[i]<- "aalen"
    }else if(praxisID[i]==4){
      out[i]<- "waldachtal"
    }else if(praxisID[i]==5){
      out[i]<- "boeblingen"
    }else if(praxisID[i]==6){
      out[i]<- "schluchsee"
    }else if(praxisID[i]==8){
      out[i]<- "wendlingen"
    }
  }
  return(out)
}

praxisID2location_id<- function(praxisID){
  l<- length(praxisID)
  out<- numeric(length = l)
  for(i in seq_along(praxisID)){
    if(praxisID[i]==1 | praxisID[i]==2){
      out[i]<- 8237
    }else if(praxisID[i]==3){
      out[i]<- 8136
    }else if(praxisID[i]==4){
      out[i]<- 8237
    }else if(praxisID[i]==5){
      out[i]<- 8115
    }else if(praxisID[i]==6){
      out[i]<- 8315
    }else if(praxisID[i]==8){
      out[i]<- 8116
    }
  }
  return(out)
}

add.weather<- function(fdf, locationvec, no.workers){
  praxes<- as.numeric(levels(as.factor(fdf$PraxisID)))
  fdl<- list()
  for(i in seq_along(praxes)){
    fdl[[i]]<- fdf|>
      filter(PraxisID==praxes[i])
  }
  wdl<- list()
  wdf.names<- paste0("wetter_",praxisID2location(praxes))
  for(i in seq_along(praxes)){
    wdl[[i]]<- get(wdf.names, envir = .GlobalEnv)
  }
  
  wcl<- makeCluster(no.workers)
  dist.env<- environment()
  clusterExport(wcl, varlist = c("fdl","wdl"), envir = dist.env)
  result<- parLapply(wcl,seq_along(praxes),fun = function(k){
    fdf_loc<- fdl[[k]]
    wdf<- wdl[[k]]
    
  })
  
}

ThomsDiscomfortIndex<- function(PraxisID, date){
  wdf<- get(paste0("wetter_",praxisID2location(PraxisID)))|>
    filter(TG_DateNum==date)
  out<- mean(wdf$temperature_kelvin)-273.16-0.55*(1-0.01*mean(wdf$relative_humidity))*(mean(wdf$temperature_kelvin)-273.16-14.5)
  return(out)
}

weatherdata.transformation<- function(wdf, sel.quantile=NA, sel.temperature_kelvin=NA, loc, dr){
  #takes a raw, hourly weather data frame and returns a cleaned, daily weather data frame with additional columns. Within these additional columns, Thom's discomfort index, the length of how long a heatwave lasted up to this day, and the data for the suggested discomfort index can be found.
  #sel.quantile OR sel.temperature have to be specified. They are the quantile or absolute temperature (in Kelvin) used to determine whether a day is part of a heatwave.
  #loc is a character string which is used to specify the location where the wdf data were recorded
  #dr is the date range of the diagnosis data
  browser()
  full.wdf<- wdf
  n_before<- sum(full.wdf$TG_DateNum<dr[1])/24
  wdf<- wdf|>
    filter(TG_DateNum>=dr[1] & TG_DateNum<=dr[2])
  if(is.na(sel.quantile) & is.na(sel.temperature_kelvin)) stop("Either sel.quantile or sel.temperature has to be a numeric number. The other one has to be NA.")
  daily.mean.temperature_kelvin<- colMeans(matrix(wdf$temperature_kelvin, nrow = 24))
  daily.mean.relative.humidity<- colMeans(matrix(wdf$relative_humidity, nrow = 24))
  dates<- unique(wdf$TG_DateNum)
  length.heatwave<- numeric(length = length(dates))
  if(is.na(sel.temperature_kelvin)){
    threshold<- quantile(daily.mean.temperature_kelvin, probs=sel.quantile)
    above.threshold<- daily.mean.temperature_kelvin>=threshold
  }else{
    above.threshold<- daily.mean.temperature_kelvin>=sel.temperature_kelvin
  }
  
  above.threshold<- as.numeric(above.threshold)
  for(i in seq_along(length.heatwave)){
    if(above.threshold[i]==1){
      length.heatwave[i]<- length.heatwave[i-1]+1
    }
  }
  out<- as.data.frame(cbind(dates,daily.mean.temperature_kelvin,daily.mean.relative.humidity, length.heatwave))
  colnames(out)<- c("TG_DateNum", "daily_mean_temperature_kelvin", "daily_mean_relative_humidity", "length_heatwave")
  out$thoms_discomfort_index<- daily.mean.temperature_kelvin-273.16-0.55*(1-0.01*daily.mean.relative.humidity)*(daily.mean.temperature_kelvin-273.16-14.5)
  
  #add lags for suggested discomfort index
  lagged.data<- as.data.frame(matrix(NA,nrow = nrow(out), ncol = 42))
  for(i in 1:21){
    lagged.data[,i]<- full.wdf$temperature_kelvin[seq(1,nrow(out))+n_before-i]
    lagged.data[,i+21]<- full.wdf$relative_humidity[seq(1,nrow(out))+n_before-i]
  }
  cn.vec<- c(rep("temperature_kelvin_l",21), rep("relative_humidity_l",21))
  colnames(lagged.data)<- paste0(cn.vec,c(seq(1,21),seq(1,21)))
  out<- cbind(out,lagged.data)
  
  assign(paste0("transformed_weather_",loc),out)
}

SuggestedDiscomfortIndex<- function(date,loc,w,theta,rho,tau){
  df<- get(paste0("transformed_weather_",loc), envir = .GlobalEnv)
  
}
