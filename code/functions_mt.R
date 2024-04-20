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
    selector<- which(first[i] == classification.matrix$letter  &
                       no[i] >= classification.matrix$start_no & 
                       no[i] <= classification.matrix$end_no)
    if(length(selector)==0){
      out[i]<- 11
    }else if(length(selector)==1){
      out[i]<- classification.matrix$class[selector]
    }else{
      stop(paste("For entry",i, "selector has a length longer than 1."))
    }
  }
  out[grepl("P81.0", icd10vec)]<- 2
  
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

TG_DateNum2week.of.month<- function(tgdatenumvec){
  #takes a vector of TG_DateNum dates and returns them as their respective week within the year according to ISO 8601 as factor variable
  out<- as.factor(day(TG_DateNum2date(tgdatenumvec)))
}

TG_DateNum2month<- function(tgdatenumvec){
  #takes a vector of TG_DateNum dates and returns them as their respective month as ordered factor variable
  out<- month(TG_DateNum2date(tgdatenumvec), label = TRUE)
}

TG_DateNum2year<- function(tgdatenumvec){
  #takes a vector of TG_DateNum dates and returns them as their respective month as ordered factor variable
  out<- isoyear(TG_DateNum2date(tgdatenumvec))
}

date2TG_DateNum<- function(date) {
  #transforms a date (yyyy-mm-dd) into a Matlab Serial time code (like TG_DateNum)
  as.numeric(as.Date(date)) +719528
}

TG_DateNum2holiday<- function(tgdatenumvec){
  #transforms a vector of TG_DateNum dates to a data frame with two dummy variables: school holiday and public holiday
  school_holiday<- numeric(length(tgdatenumvec))
  public_holiday<- numeric(length(tgdatenumvec))
  for(i in seq_along(tgdatenumvec)){
    selector<- which(holidays$start<=tgdatenumvec[i] & holidays$end>=tgdatenumvec[i])
    if(length(selector)==1){
      if(holidays$public.holiday[selector]==1){
        public_holiday[i]<- 1
      }else{
        school_holiday[i]<- 1
      }
    }else if(length(selector)==2){
      public_holiday[i]<- 1
      school_holiday[i]<- 1
    }else if(length(selector>2)){
      stop(paste0("Entry ",i,", which is TG_DateNum date ",tgdatenumvec[i],", is found more than twice in the holiday data frame."))
    }
  }
  out<- data.frame(school_holiday=school_holiday, public_holiday=public_holiday)
  return(out)
}

IK2PKV<- function(IK.vector){
  # IK.vector is a vector of IK codes; 0=publich health insurance, 1=private health insurance or no health insurance
  # depends on ListeKrankenkassen
  out<- ListeKrankenkassen$PKV[match(IK.vector,ListeKrankenkassen$IK)]
  out[is.na(out)]<- 1
  out[out==1]<- 1
  return(out)
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
  #parallelised version of add.stamm.new (created for a different project). Merges Stamm-data to the incident data frame.
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
  #slightly adjusted version of add.stamm.new.par. Merges Konsul-data to incident data frame.
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
  # takes an atomic vector of PraxisIDs with length >=1 and returns a vector of the same length with the name of the practices' municipalities.
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
  # takes an atomic vector of PraxisIDs with length >=1 and returns a vector of the same length with the name of the practices' location IDs
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



weatherdata.transformation<- function(wdf, sel.quantile=NA, sel.temperature_kelvin=NA, loc, dr){
  #takes a raw, hourly weather data frame and returns a cleaned, daily weather data frame with additional columns. Within these additional columns, Thom's discomfort index, the length of how long a heatwave lasted up to this day, and the data for the suggested discomfort index can be found.
  #sel.quantile OR sel.temperature have to be specified. They are the quantile or absolute temperature (in Kelvin) used to determine whether a day is part of a heatwave.
  #loc is a character string which is used to specify the location where the wdf data were recorded
  #dr is the date range of the diagnosis data
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
  temp<- colMeans(matrix(full.wdf$temperature_kelvin, nrow = 24))
  hum<- colMeans(matrix(full.wdf$relative_humidity, nrow = 24))
  for(i in 1:21){
    lagged.data[,i]<- temp[seq(1,nrow(out))+n_before-i]
    lagged.data[,i+21]<- full.wdf$relative_humidity[seq(1,nrow(out))+n_before-i]
  }
  cn.vec<- c(rep("temperature_kelvin_l",21), rep("relative_humidity_l",21))
  colnames(lagged.data)<- paste0(cn.vec,c(seq(1,21),seq(1,21)))
  out<- cbind(out,lagged.data)
  
  assign(paste0("transformed_weather_",loc),out, envir = .GlobalEnv)
}

add.weather<- function(fdf,no.workers){
  # merges weather data to the incident data frame.
  locations<- unique(fdf$landkreis)
  fl<- list()
  twl<- list()
  for(i in seq_along(locations)){
    fl[[i]]<- fdf|>
      filter(landkreis==locations[i])
    twl[[i]]<- get(paste0("transformed_weather_",locations[i]), envir = .GlobalEnv)
  }
  dist.env<- environment()
  w.cl<- makeCluster(no.workers)
  clusterExport(cl = w.cl, varlist = c("fl","twl"), envir = dist.env)
  
  result<- parLapply(cl = w.cl, seq_along(locations), fun = function(k){
    patient.data<- fl[[k]]
    dates<- patient.data$TG_DateNum
    weather.data<- twl[[k]]
    out<- as.data.frame(matrix(NA,nrow = length(dates), ncol = ncol(weather.data)))
    for(i in seq_along(dates)){
      out[i,]<- weather.data[which(weather.data$TG_DateNum==dates[i]),]
    }
    colnames(out)<- colnames(weather.data)
    out<- out[,-1]
    out<- cbind(patient.data,out)
  })
  
  out<- result[[1]]
  for(i in seq(2,length(locations))){
    out<- rbind(out,result[[i]])
  }
  return(out)
}

SuggestedDiscomfortIndex_global<- function(loc,w,theta,rho,tau){
  #uses loc to find a transformed weather data frame and calculates vis-à-vis a vector of the suggested discomfort index
  df<- get(paste0("transformed_weather_",loc), envir = .GlobalEnv)
  T<- df$daily_mean_temperature_kelvin
  RH<- df$daily_mean_relative_humidity
  out<- w[1]*(theta[1]*T + theta[2]*T^2 + theta[3]*T^3 + rho[1]*RH + rho[2]*RH + rho[3]*RH + tau* T*RH)
  if(length(w)>1){
    for(i in seq(2,length(w))){
      #T<- get(paste0("df$temperature_kelvin_l",i))
      T<- df[paste0("temperature_kelvin_l",i)]
      #RH<- get(paste0("df$relative_humidity_l",i))
      RH<- df[paste0("relative_humidity_l",i)]
      out<- out+ w[i]*(theta[1]*T + theta[2]*T^2 + theta[3]*T^3 + rho[1]*RH + rho[2]*RH + rho[3]*RH + tau* T*RH)
    }
  }
  return(out)
}

SDI<- function(df,w,theta,rho,tau){
  #similar to SuggestedDiscomfortIndex. Only difference: instead off adding data to a transformed_weather data frame, it uses any data frame which contains the necessary data (temperatures and relative humidity)
  T<- df$daily_mean_temperature_kelvin
  RH<- df$daily_mean_relative_humidity
  out<- w[1]*(theta[1]*T + theta[2]*T^2 + theta[3]*T^3 + rho[1]*RH + rho[2]*RH + rho[3]*RH + tau* T*RH)
  if(length(w)>1){
    for(i in seq(2,length(w))){
      T<- df[paste0("temperature_kelvin_l",i-1)]
      RH<- df[paste0("relative_humidity_l",i-1)]
      out<- out+ w[i]*(theta[1]*T + theta[2]*T^2 + theta[3]*T^3 + rho[1]*RH + rho[2]*RH + rho[3]*RH + tau* T*RH)
    }
  }
  colnames(out)<- "SDI"
  return(out$SDI)
}

add.chronic<- function(diagdf,chronicdf){
  # Creates an incident data frame from a diagnosis (Diag) data frame by merging data about chronic diseases.
  chronicdf<- chronicdf|>
    group_by(uniPatID, diag_category)|>
    mutate(counts = n())|>
    distinct()|>
    ungroup()
  pb<- txtProgressBar(min = 0, max = nrow(chronicdf), initial = 0)
  row.list<- list()
  column.list<- list()
  value.list<- list()
  for(i in seq(1,nrow(chronicdf))){
    row.list[[i]]<- which(diagdf$uniPatID==as.numeric(chronicdf[i,1]))
    column.list[[i]]<- rep(as.numeric(chronicdf[i,2]), length(row.list[[i]]))
    value.list[[i]]<- rep(as.numeric(chronicdf[i,3]), length(row.list[[i]]))
    setTxtProgressBar(pb,i)
  }
  addage<- as.data.frame(as.matrix(sparseMatrix(i = unlist(row.list),
                                                j = unlist(column.list),
                                                x = unlist(value.list))))
  if(nrow(addage)<nrow(diagdf)){
    addage<- rbind(addage, matrix(0,nrow = nrow(diagdf)-nrow(addage), ncol = ncol(addage)))
  }
  colnames(addage)<- paste0(rep("chronic_",11),1:11)
  addage$no_all_chronic_diseases<- rowSums(addage)
  out<- cbind(diagdf,addage)
  return(out)
}

praxis_id2landkreis_id<- function(practiceids){
  # transforms an atomic vector of practiceIDs into a vector of Landkreis IDs.
  practiceids<- as.character(practiceids)
  out<- numeric(length(practiceids))
  for(i in seq_along(practiceids)){
    out[i]<- location_information$landkreis_id[grepl(practiceids[i],location_information$praxis_ids)]
  }
  return(out)
}

add.covid<- function(df,cdf, no.workers){
  # merges Covid-19 data to an incident data frame.
  all.location.ids<- unique(df$Landkreis_id)
  dl<- list()
  cl<- list()
  for(i in seq_along(all.location.ids)){
    dl[[i]]<- df|>
      filter(Landkreis_id==all.location.ids[i])
    cl[[i]]<- cdf|>
      filter(Landkreis_id==all.location.ids[i])
  }
  
  dist.env<- environment()
  covid.cluster<- makeCluster(no.workers)
  clusterExport(cl = covid.cluster, varlist = c("dl","cl"), envir = dist.env)
  result<- parLapply(covid.cluster,seq_along(all.location.ids),fun = function(k){
    base.data<- dl[[k]]
    covid.addage<- cl[[k]]
    
    out<- numeric(nrow(base.data))
    dates<- unique(base.data$TG_DateNum)
    for(i in seq_along(dates)){
      selector<- base.data$TG_DateNum==dates[i]
      if(any(covid.addage$TG_DateNum==dates[i])){
        out[selector]<- covid.addage$Inzidenz_7.Tage[covid.addage$TG_DateNum==dates[i]]
      }
    }
    out<- cbind(base.data,out)
    colnames(out)<- c(colnames(base.data),"covid_7_day_incidence")
    return(out)
  })
  
  out<- result[[1]]
  for(i in seq(2,length(result))){
    out<- rbind(out, result[[i]])
  }
  
  return(out)
}

add.daylight<- function(fdf,no.workers){
  # merges daylight data to an incident data frame
  fdl<- list()
  dldl<- list()
  ids<- unique(fdf$PraxisID)
  for(i in seq_along(ids)){
    fdl[[i]]<- fdf|>
      filter(PraxisID==ids[i])
    dldl[[i]]<- get(paste0("daylight_",
                           location_information$location.name[grepl(ids[i],location_information$praxis_ids)]),
                    envir = .GlobalEnv)
  }
  
  dist.env<- environment()
  dayl.cluster<- makeCluster(no.workers)
  clusterExport(cl = dayl.cluster, varlist = c("fdl","dldl"), envir = dist.env)
  result<- parLapply(dayl.cluster,seq_along(ids),fun = function(k){
    base.data<- fdl[[k]]
    daylight.addage<- dldl[[k]]
    dates<- base.data$TG_DateNum
    unique.dates<- unique(dates)
    out<- numeric(length(dates))
    for(i in seq_along(unique.dates)){
      selector<- dates==unique.dates[i]
      out[selector]<- daylight.addage$daylight_hours[daylight.addage$TG_DateNum==unique.dates[i]]
    }
    out<- cbind(base.data,out)
    colnames(out)<- c(colnames(base.data),"daylight_hours")
    return(out)
  })
  
  out<- result[[1]]
  for(i in seq(2,length(result))){
    out<- rbind(out, result[[i]])
  }
  return(out)
}

risk.factor.merger<- function(vec_1, vec_2){
  # takes two numeric hot encoded vectors of risk factors and turns them into a single vector of risk factors, using the rules laid out in the paper.
  out<- numeric(length(vec_1))
  for(i in seq_along(vec_1)){
    if(vec_1[i]==vec_2[i]){
      out[i]<- vec_1[i]
    }else{
      if((is.na(vec_1[i])& !is.na(vec_2[i]))){
        out[i]<- vec_1[i]
      }else if(is.na(vec_2[i])& !is.na(vec_1[i])){
        out[i]<- vec_2[i]
      }else{
        if((vec_1[i]==1 & vec_2[i]==0)|(vec_1[i]==0 & vec_2[i]==1)){
          out[i]<- 1
        }else{
          stop(paste("There is an issue with the values",vec_1[i],"and",vec_2[i]))
        }
      }
    }
  }
  return(out)
}

add.last.visit<-function(fdf, no.splits, no.workers){
  # finds the last visit for a disease of the same category and merges it to the incident data frame
  part.dl<- chunk.data(fdf,no.splits)
  
  visit.cluster<- makeCluster(no.workers)
  dist.env<- environment()
  clusterExport(cl = visit.cluster, varlist = c("part.dl","arrange"), envir = dist.env)
  results<- parLapply(cl = visit.cluster, seq(1,no.splits), fun = function(k){
    df<- part.dl[[k]]|>
      arrange(uniPatID,diag_class,TG_DateNum)
    n<- nrow(df)
    selector<- df$uniPatID[1:(n-1)]==df$uniPatID[2:n] & df$diag_class[1:(n-1)]==df$diag_class[2:n]
    out<- rep(NA,n)
    for(i in seq(2,n)[selector]){
      out[i]<- df$TG_DateNum[i]-df$TG_DateNum[i-1]
    }
    return(out)
  })
  
  for(i in seq_along(part.dl)){
    part.dl[[i]]<- part.dl[[i]]|>
      arrange(uniPatID,diag_class,TG_DateNum)
    part.dl[[i]]$last_visit<- results[[i]]
  }
  out<- bind_rows(part.dl)
  return(out)
}

df_qx<- function(inputdf = full.df_7, di, q){
  # creates an input data frame for research question 1 or 2. With customisation for the desired discomfort index.
  # inputdf is the data frame from which the variables are extracted
  # di is the discomfort index. It is either "TDI" for Thom's discomfort index, "HW" for the length of heatwave, or "SDI" for the suggested discomfort index
  # q specifies the research question- either it takes the value 1 or 2.
  if(di=="TDI"){
    out<- data.frame(thoms_discomfort_index = inputdf$thoms_discomfort_index)
  }else if(di=="HW"){
    out<- data.frame(length_heatwave = inputdf$length_heatwave)
  }else if(di=="SDI"){
    out<- inputdf[,grepl("temperature", colnames(inputdf)) | grepl("humidity", colnames(inputdf))]
  }
  
  out$PraxisID<- inputdf$PraxisID
  out$dow<- inputdf$dow
  out$public_holiday<- inputdf$public_holiday
  out$school_holiday<- inputdf$school_holiday
  out$week_of_month<- inputdf$week_of_month
  out$month<- inputdf$month
  out$year<- inputdf$year
  out$daylight_hours<- inputdf$daylight_hours
  out$covid_7_day_incidence<- inputdf$covid_7_day_incidence
  
  if(q==2){
    out$age<- full.df_7$age
    out$female<- full.df_7$female
    out$PKV<- full.df_7$PKV
    out$smoking<- full.df_7$smoking
    out$alcohol<- full.df_7$alcohol
    out$sport<- full.df_7$sport
    
    chronic.selector<- grepl("chronic", colnames(inputdf))
    addage<- inputdf[,chronic.selector]
    colnames(addage)<- colnames(inputdf)[chronic.selector]
    out<- cbind(out,addage)
  }
  
  return(out)
}

stoch.round<- function(x){
  # implementation of stochastic rounding to integer, for purposes of hyperparameter tuning. It can handle atomic vector inputs.
  floor(x)+ as.numeric(runif(length(x))<=(x-floor(x)))
}

wrapper_interior<- function(sdi = FALSE, lr, no.leaves, max.depth, 
                            min.data.in.leaf, feature.fraction, cat.l2,
                            extra.trees = FALSE, top.rate, other.rate,
                            cat.smooth, path.smooth,
                            inputdf, y, est.type, alpha = 0.5, cv = 5L,
                            no.trees = 100L, no.threads = 4L, 
                            early.stopping = 10L, seed = NA, 
                            error.rate = FALSE){
  # Interior wrapper for cross-validation of the model. Returns the mean of the error estimated for each fold. If cv<=1, instead of cross validation, a model trained on the entire input data is returned (to be precise, a lgbBooster).
  # sdi is FALSE by default. If the suggested discomfort index is to be used, sdi has to be an atomic vector.
  # if sdi = TRUE:
  # sdi[1] number of lags/ size for the weights of sdi
  # sdi[2] alpha/ shape parameter 1 for the weights of sdi
  # sdi[3] beta/ shape parameter 2 for the weights of sdi
  # sdi[4:6] theta values for the daily temperature of the sdi
  # sdi[7:9] rho values for the daily humidity of the sdi
  # sdi[10] tau value for the daily interaction between temperature and humidity
  # lr is the learning rate (double, >=0)
  # no.leaves is the maximum number of leaves a single tree may have (int, >=2)
  # max.depth is the maximum depth an individual tree may have (int, >=1)
  # min.data.in.leaf is the minimum number of training observations that are found in every leaf (int, >=1)
  # feature.fraction is the fraction of features/ variables considered for each split (0< double <=1)
  # cat.l2 is a l2 smoothing parameter applied to categorical variables (double, >=0)
  # extra.trees: from the parameter description, "if set to true, when evaluating node splits LightGBM will check only one randomly-chosen threshold for each feature" (Boolean)
  # top.rate: from the parameter description, "the retain ratio of large gradient data" used in GOSS (0< double <1).
  # other.rate: from the parameter description, "the retain ratio of small gradient data" used in GOSS (0< double <1).
  # cat.smooth is a parameter that can be used to deal with noisy observations in categorical data (double, >=0).
  # path.smooth is a smoothing parameter applied to tree nodes (double, >=0)
  # inputdf is the input data frame which contains all explanatory variables
  # y is the vector of outcomes/ labels
  # est.type specifies the loss function. It may take the values "quantile", "binary", "multiclass", or "cross_entropy".
  # alpha is in this context the quantile to be estimated during quantile regression.
  # cv is the number of folds for cross-validation (2<= integer >= number of observations - 1)
  # seed is a seed that may be deployed to the wrapper, but it is not needed. Due to the parallelisation, exact replication is not possible.
  
  if(est.type=="quantile"){
    lossfct<- "quantile"
  }else if(est.type=="binary"){
    lossfct<- "binary_logloss"
  }else if(est.type=="multiclass"){
    lossfct<- "multi_logloss"
  }else if(est.type=="cross_entropy"){
    lossfct<- "xentropy"
  }
  
  blacklist<- c("thoms_discomfort_index", "length_heatwave", "sdi", 
                "daylight_hours", "covid_7_day_incidence", "age", 
                colnames(inputdf)[grep("chronic", colnames(inputdf))])
  if(length(sdi)>1){
    sdi.weights<- dbetabinom.ab(x = seq(0,sdi[1]), size = sdi[1],
                                shape1 = sdi[2], shape2 = sdi[3])
    sdi.vec<- SDI(df = inputdf, w = sdi.weights, theta = sdi[4:6],
                  rho = sdi[7:9], tau = sdi[10])
    
    col.selector<- grepl("temperature", colnames(inputdf)) | grepl("humidity", colnames(inputdf))
    df<- inputdf[,!col.selector]
    df$sdi<- sdi.vec
    factor.vars<- colnames(df)[!colnames(df) %in% blacklist]
    df<- data.matrix(df)
  }else if(isFALSE(sdi)){
    factor.vars<- colnames(inputdf)[!colnames(inputdf) %in% blacklist]
    df<- data.matrix(inputdf)
  }else{
    stop(paste("wrapper_interior: sdi has to be either FALSE or a vector of sdi hyperparameters."))
  }
  
  if(!is.na(seed)){
    set.seed(seed)
  }
  
  parameters<- list(objective = est.type, data_sample_strategy = "goss", 
                    num_trees = no.trees, num_threads = no.threads,
                    learning_rate = lr, num_leaves = no.leaves, 
                    use_missing = TRUE, zero_as_missing = FALSE,
                    max_depth = max.depth, min_data_in_leaf = min.data.in.leaf,
                    feature_fraction = feature.fraction, 
                    extra_trees = extra.trees, top_rate = top.rate,
                    other_rate = other.rate, cat_l2 = cat.l2, 
                    cat_smooth = cat.smooth, path_smooth = path.smooth,
                    alpha = alpha)
  if(est.type == "binary"){
    eval.metric<- list()
    eval.metric[[1]]<- lossfct
    eval.metric[[2]]<- "binary_error"
  }else if(est.type == "multiclass"){
    parameters$num_class <- length(levels(as.factor(y)))
    eval.metric<- list()
    eval.metric[[1]]<- lossfct
    eval.metric[[2]]<- "multi_error"
  }
  
  if(cv>1){
    results<- list()
    score.vec<- numeric(cv)
    error.rate.vec<- numeric(cv)
    for(i in seq(1,cv)){
      valid.selector<- seq(i, nrow(inputdf), by= cv)
      train.df<- lgb.Dataset(df[-valid.selector,],
                             categorical_feature = factor.vars,
                             label = y[-valid.selector])
      valid.df<- lgb.Dataset.create.valid(dataset = train.df,
                                          data = df[valid.selector,],
                                          label = y[valid.selector])
      results[[i]]<- lgb.train(params = parameters, data = train.df, 
                               nrounds = no.trees, 
                               valids = list(my_validation = valid.df), 
                               obj = est.type, verbose = -1, record = TRUE,
                               categorical_feature = factor.vars, 
                               early_stopping_rounds = early.stopping,
                               eval = eval.metric)
      score.vec[i]<- results[[i]]$best_score
      if(isTRUE(error.rate)){
        error.rate.vec[i]<- results[[i]]$eval_valid()[[2]]$value
      }
    }
    if(isFALSE(error.rate)){
      out<- mean(score.vec)
      return(out)
    }else{
      out<- colMeans(data.frame(score = score.vec, error.pct = error.rate.vec))
      return(out)
    }
  }else{
    train.df<- lgb.Dataset(df, categorical_feature = factor.vars, label = y)
    out<- lgb.train(params = parameters, data = train.df, nrounds = no.trees, 
                    obj = est.type, verbose = 1, record = TRUE,
                    categorical_feature = factor.vars, eval = eval.metric)
    return(out)
  }
}

genetic.algorithm<- function(optim.seed, n = 50, pcrossover = 0.8,
                             pmutation = 0.1, maxiter = 10, elitism = 4,
                             digits,
                             sdi = FALSE, lr, no.leaves, max.depth, 
                             min.data.in.leaf, feature.fraction, cat.l2,
                             extra.trees = FALSE, top.rate, other.rate,
                             cat.smooth, path.smooth,
                             inputdf, y, est_type, alpha = 0.5, cv = 5L,
                             no_trees = 100L, no_threads = 4L, 
                             early_stopping = 10L, seed = NA){
  
  # A genetic algorithm that adapted from the ga function from the GA-package by Luca Scrucca (doi:10.18637/jss.v053.i04). The loss function is rounded to two significant digits; as secondary fitness evaluation speed is used.
  # optim.seed: start seed for optimization
  # n: number of members in the population
  # pcrossover: probability for chromosomes to merge
  # pmutation: probability for a mutation to occur
  # maxiter: integer number of generations that are trained
  # elitism: integer number of parent models that are permitted to procreate. Sorted best to worst, the first to elitism-th model form the parent model pool.
  # digits is an integer vector of length maxiter. The i-th entry in digits gives the number of significant digits to be considered during evolution 
  # sdi to path.smooth: instead of individual values (like wrapper_interior), takes atomic vectors of length 2 with the lower and upper limit that the respective hyperparameter can take.
  # all others: vis-à-vis wrapper_interior
  results<- list()
  limits<- rbind(lr, no.leaves, max.depth, min.data.in.leaf, feature.fraction,
                 cat.l2, extra.trees, top.rate, other.rate, cat.smooth, 
                 path.smooth)
  colnames(limits)<- c("lower","upper")
  X<- matrix(NA, nrow = n, ncol = 11)
  colnames(X)<- c("lr", "no.leaves", "max.depth", "min.data.in.leaf", 
                  "feature.fraction", "cat.l2", "extra.trees", "top.rate",
                  "other.rate", "cat.smooth", "path.smooth")
  
  set.seed(optim.seed)
  if(isTRUE(sdi)){
    sdi_init<- c(NA, NA, NA, 0.45, 0, 0, -0.07975, 0, 0, 0.0055)
    X_2<- matrix(NA,nrow = n, ncol = 10)
    for(i in seq(1,10)){
      X_2[,i]<- sdi.sampler(i, n, sdi_init)
    }
    colnames(X_2)<- c("no.lags","sdi.alpha","sdi.beta", "theta_1",  "theta_2",
                      "theta_3", "rho_1", "rho_2", "rho_3","tau")
    X<- cbind(X,X_2)
  }
  
  for(i in seq(1,11)){
    X[,i]<- runif(n, min = limits[i,1], max = limits[i,2])
  }
  
  # Generation 1
  Y<- numeric(n)
  delta_t<- numeric(n)
  for(i in seq(1,n)){
    if(isTRUE(sdi)){
      sdi.pars<- X[i,12:21]
    }else{
      sdi.pars<- FALSE
    }
    tic()
    Y[i]<- wrapper_interior(sdi = sdi.pars, X[i,1], stoch.round(X[i,2]), 
                            stoch.round(X[i,3]), stoch.round(X[i,4]), X[i,5], 
                            X[i,6], as.logical(stoch.round(X[i,7])), X[i,8], 
                            X[i,9], X[i,10], X[i,11],
                            inputdf = inputdf, y = y, est.type = est_type, 
                            alpha = alpha, cv = cv, no.trees = no_trees,
                            no.threads = no_threads, 
                            early.stopping = early_stopping, seed = seed)
    im<- toc()
    delta_t[i]<- im$toc - im$tic
  }
  Y_round<- signif(Y, digits = digits[1])
  generation.results<- as.data.frame(cbind(X, Y, Y_round, delta_t))|>
    arrange(Y_round,delta_t)
  results[[1]]<- generation.results
  best.in.class<- generation.results[1,]
  
  # Generation 2+
  for(s in seq(2,maxiter)){
    set.seed(optim.seed + s)
    X_new<- X
    mutation.matrix<- matrix(runif(n * ncol(X)), nrow = n) <= pmutation
    for(i in seq(1,n)){
      parents<- X[sample(1:elitism, 2, replace = FALSE),]
      for(j in seq(1,ncol(X_new))){
        if(mutation.matrix[i,j]){
          if(j <= 11){
            X_new[i,j]<- runif(1, min = limits[j,1], max = limits[j,2])
          }else{
            X_new[i,j]<- sdi.sampler(j - 11, 1, sdi_init)
          }
        }else{
          X_new[i,j]<- sample(c(parents[1,j],parents[2,j], mean(parents[,j])), 
                              1, prob = c(rep(0.5 * (1 - pcrossover), 2),
                                          pcrossover))
        }
      }
    }
    X<- X_new
    Y<- numeric(n)
    delta_t<- numeric(n)
    for(i in seq(1,n)){
      if(isTRUE(sdi)){
        sdi.pars<- X[i,12:21]
        sdi.pars[1]<- stoch.round(sdi.pars[1])
      }else{
        sdi.pars<- FALSE
      }
      tic()
      Y[i]<- wrapper_interior(sdi = sdi.pars, X[i,1], stoch.round(X[i,2]), 
                              stoch.round(X[i,3]), stoch.round(X[i,4]), X[i,5], 
                              X[i,6], as.logical(stoch.round(X[i,7])), X[i,8], 
                              X[i,9], X[i,10], X[i,11],
                              inputdf = inputdf, y = y, est.type = est_type, 
                              alpha = alpha, cv = cv, no.trees = no_trees,
                              no.threads = no_threads, 
                              early.stopping = early_stopping, seed = seed)
      im<- toc()
      delta_t[i]<- im$toc - im$tic
    }
    Y_round<- signif(Y, digits = digits[s])
    generation.results<- as.data.frame(cbind(X, Y, Y_round, delta_t))|>
      arrange(Y_round,delta_t)
    results[[s]]<- generation.results
    if(generation.results$Y_round[1] <= best.in.class$Y_round[1] & 
       generation.results$delta_t[1] < best.in.class$delta_t[1]){
      best.in.class<- generation.results[1,]
    }
  }
  
  results$best_in_class<- best.in.class
  return(results)
}

sdi.sampler<- function(i, no.samples, sdi_init){
  if(i==1){
    1 + rbetabinom.ab(no.samples, 20, 1.2, 1.2 * 13/7) #no. lags
  }else if(i==2){
    runif(no.samples, min = 0, max = 3) #shape1
  }else if(i==3){
    runif(no.samples, min = 1, max = 8) #shape2
  }else if(i>3){
    rnorm(no.samples, mean = sdi_init[i], sd = 0.25) #theta1-3, rho1-3, tau
  }
}

ga2model<- function(ga.list, inputdf, y, est.type, alpha = 0.5,
                    no.trees = 100L, no.threads = 4L, seed = NA){
  # trains the best model specified by the "genetic.algorithm" function. It is based on wrapper_interior, similar to genetic.algorithm.
  # ga.list is an output list extracted from the function "genetic.algorithm".
  # other parameters are equal to wrapper_interior parameters.
  params<- ga.list$best_in_class
  if(colnames(inputdf)[1] %in% c("thoms_discomfort_index", "length_heatwave")){
    sdi.params<- FALSE
  }else{
    sdi.params<- c(params$no.lags, params$sdi.alpha, params$sdi.beta, 
                   params$theta_1, params$theta_2, params$theta_3, params$rho_1,
                   params$rho_2, params$rho_3, params$tau)
  }
  
  out<- wrapper_interior(sdi = sdi.params, lr = params$lr, 
                         no.leaves = round(params$no.leaves), 
                         max.depth = round(params$max.depth),
                         min.data.in.leaf = round(params$min.data.in.leaf),
                         feature.fraction = params$feature.fraction,
                         cat.l2 = params$cat.l2,
                         extra.trees = as.logical(round(params$extra.trees)),
                         top.rate = params$top.rate, 
                         other.rate = params$other.rate, 
                         cat.smooth = params$cat.smooth, 
                         path.smooth = params$path.smooth,
                         inputdf = inputdf, y = y, est.type = est.type, 
                         alpha = alpha, cv = 1L, no.trees = no.trees, 
                         no.threads = no.threads)
  return(out)
}

ga2performance.eval<- function(ga.list, inputdf, y, est.type, no.trees = 100L, 
                               no.threads = 4L, seed = NA, cv = 10){
  # Measures the performance of the best model specified by the "genetic.algorithm" function.
  # ga.list is an output list extracted from the function "genetic.algorithm".
  # other parameters are equal to wrapper_interior parameters.
  params<- ga.list$best_in_class
  if(any(colnames(inputdf) %in% c("thoms_discomfort_index", "length_heatwave"))){
    sdi.params<- FALSE
  }else{
    sdi.params<- c(params$no.lags, params$sdi.alpha, params$sdi.beta, 
                   params$theta_1, params$theta_2, params$theta_3, params$rho_1,
                   params$rho_2, params$rho_3, params$tau)
  }
  if(est.type %in% c("binary", "multiclass")){
    error_rate<- TRUE
  }else{
    error_rate<- FALSE
  }
  
  out<- wrapper_interior(sdi = sdi.params, lr = params$lr, 
                         no.leaves = round(params$no.leaves), 
                         max.depth = round(params$max.depth),
                         min.data.in.leaf = round(params$min.data.in.leaf),
                         feature.fraction = params$feature.fraction,
                         cat.l2 = params$cat.l2,
                         extra.trees = as.logical(round(params$extra.trees)),
                         top.rate = params$top.rate, 
                         other.rate = params$other.rate, 
                         cat.smooth = params$cat.smooth, 
                         path.smooth = params$path.smooth,
                         inputdf = inputdf, y = y, est.type = est.type, 
                         alpha = alpha, cv = cv, no.trees = no.trees, 
                         no.threads = no.threads, error.rate = error_rate)
  return(out)
}

booster.eval<- function(booster, DI, sdi = FALSE, Q, no.draws, eval.var, eval.seq, seed, eval.type = "response"){
  # Evaluation algorithm for an lgbBooster. Draws entries from the training dataset and "twists" one selected variable, creating a ceteris paribus comparison. Thus, it can be seen how a variable changes with different inputs. At the end, a list of length no.draws is returned. Each element of the list is a data frame with two columns each: One is the "twisted" variable, the other one is the prediction of the booster.
  # booster is an lgbBooster object
  # DI is the discomfort index that the booster has been trained on, either "TDI", "HW", or "SDI"
  # Q is the research question: either 1 or 2
  # no.draws is the number of entries drawn from inputdf. 
  # eval.var is the character name of the variable to be evaluated (the "X" variable) 
  # eval.seq is the vector of different values that are to be evaluated (the different values "X" may take)
  # seed is the random seed
  # eval.type prediction output type. Should be "response". For further details, check https://lightgbm.readthedocs.io/en/latest/R/reference/predict.lgb.Booster.html
  browser()
  library(lightgbm)
  set.seed(seed)
  
  inputdf<- df_qx(di = DI, q = Q)
  blacklist<- c("thoms_discomfort_index", "length_heatwave", "sdi", 
                "daylight_hours", "covid_7_day_incidence", "age", 
                colnames(inputdf)[grep("chronic", colnames(inputdf))])
  if(length(sdi)>1){
    sdi.weights<- dbetabinom.ab(x = seq(0,sdi[1]), size = sdi[1],
                                shape1 = sdi[2], shape2 = sdi[3])
    sdi.vec<- SDI(df = inputdf, w = sdi.weights, theta = sdi[4:6],
                  rho = sdi[7:9], tau = sdi[10])
    
    col.selector<- grepl("temperature", colnames(inputdf)) | grepl("humidity", colnames(inputdf))
    df<- inputdf[,!col.selector]
    df$sdi<- sdi.vec
    factor.vars<- colnames(df)[!colnames(df) %in% blacklist]
    df<- data.matrix(df)
  }else if(isFALSE(sdi)){
    factor.vars<- colnames(inputdf)[!colnames(inputdf) %in% blacklist]
    df<- data.matrix(inputdf)
  }else{
    stop(paste("booster.eval: sdi has to be either FALSE or a vector of sdi hyperparameters."))
  }
  
  initial.data<- df[sample(seq(1, nrow(inputdf)), no.draws, replace = FALSE),]
  eval.data<- as.data.frame(matrix(NA, nrow = no.draws * length(eval.seq),
                                   ncol = ncol(df)))
  colnames(eval.data)<- colnames(df)
  eval.var.col<- which(colnames(eval.data)==eval.var)
  for(i in seq_along(eval.seq)){
    sel.rows<- seq(1, no.draws) + (i - 1) * no.draws
    eval.data[sel.rows,]<- initial.data
    eval.data[sel.rows,eval.var.col]<- eval.seq[i]
  }
  
  prediction.output<- matrix(predict(object = booster, 
                                     newdata = data.matrix(eval.data), 
                                     type = eval.type),
                             nrow = nrow(eval.data))
  
  out<- list()
  for(i in seq(1,no.draws)){
    selection<- seq(1, nrow(eval.data), by = no.draws)
    im<- cbind(eval.data[selection, eval.var.col], prediction.output[selection,])
    colnames(im)<- c(eval.var, paste(rep("pred_class", ncol(prediction.output)), 
                                     seq(1, ncol(prediction.output)), 
                                     sep = "_"))
    out[[i]]<- im
  }
  return(out)
}

eval.plot<- function(eval_list, y.max, plot.title, y.names = NA, 
                     y.label = "density") {
  # eval.plot takes list of simulated outcomes from booster.eval and plots the outcome on the y-axis.
  # eval_list is a list of simulations created through the booster.eval function.
  # y.max is the upper limit of the y-range of the plot
  # plot.title is a character string which serves as the name of the plot
  # y.names is a vector of names for the classes of the variable whose estimated density is shown on the y-axis. y.names is used in the creation of the plot title. If set to NA, the y.names argument is not considered in the creation of the plot title
  # y.label is the label attached to the y-axis
  k <- length(eval_list)
  x.seq<- eval_list[[1]][,1]
  for (i in 1:(ncol(eval_list[[1]]) - 1)) {
    x<- eval_list[[1]][,1]
    y<- eval_list[[1]][,i + 1]
    if(any(is.na(y.names))){
      plot.name<- plot.title
    }else{
      plot.name<- paste(plot.title, y.names[i], sep = ": ")
    }
    if(any(is.na(y.names))){
      file.name<- paste0(plot.title, ".png")
    }else{
      file.name<- paste0(plot.title, "_", y.names[i], ".png")
    }
    png(file.name)
    plot(y~x, type = 'l', xlab = colnames(eval_list[[1]])[1], ylab = y.label, 
         main = plot.name, xlim = range(x.seq), ylim = c(0,y.max))
    for (j in 2:k) {
      lines(x = x, y = eval_list[[j]][,i + 1])
    }
    dev.off()
  }
}


