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

SuggestedDiscomfortIndex<- function(loc,w,theta,rho,tau){
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

add.chronic<- function(diagdf,chronicdf){
  addage<- as.data.frame(matrix(0,nrow = nrow(diagdf),ncol = 11))
  colnames(addage)<- paste0(rep("chronic_",11),1:11)
  for(i in seq(1,nrow(chronicdf))){
    row.selector<- diagdf$uniPatID==chronicdf[i,1]
    addage[row.selector,chronicdf[i,2]]<- addage[row.selector,chronicdf[i,2]]+1
  }
  no_all_chronic_diseases<- rowSums(addage)
  addage$no_all_chronic_diseases<- no_all_chronic_diseases
  out<- cbind(diagdf,addage)
  return(out)
}

praxis_id2landkreis_id<- function(practiceids){
  practiceids<- as.character(practiceids)
  out<- numeric(length(practiceids))
  for(i in seq_along(practiceids)){
    out[i]<- location_information$landkreis_id[grepl(practiceids[i],location_information$praxis_ids)]
  }
  return(out)
}

add.covid<- function(df,cdf, no.workers){
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

elastic.net<- function(inputdf, y, standardize.y=FALSE, spline.pos=NULL, spline.knots=NULL, sel.loss.function, sel.quantile=NULL, alpha, lambda, no.starts=1, no.workers=2, max.iter=1000, step.size=1, lc.rho=1.5, tol=1e-6){
  #A function which calculates the elastic net estimate of a regression problem with splines using coordinate descent. The regression problems that can be solved are quantile regression and logistic regression (multiclass cross-entropy is currently not supported)
  #inputdf is a dataframe of variables used to estimate y.
  #y is the 'dependent' variable
  #standardize.y can be used to standardize y
  #spline.pos is a vector which specifies which columns of inputdf are to be estimated with a spline.
  #spline.knots are the number of knots that are used for each spline. It is a vector, too; the entry on spline.knots corresponds to the entry on spline.pos.
  #sel.loss.function is the loss function: either "quantile" or "proportion"
  #alpha is the parameter that shifts between lasso (alpha=0) and Ridge (alpha=1) penalty.
  #lambda is the regularization parameter: the higher lambda, the more the parameters are shrunk towards 0.
  #no.starts is the number of starts of the estimation. Usually should be set to 1. For a single start, no. starts will use the ordinary least squares estimates as start values. For more starts, the start values are proportionally shrunk towards 0.
  #no.workers is the numbers of workers used to calculate the coefficients if more than one start is chosen.
  #max.iter is the maximum number of iterations.
  #step.size is the starting length of the step.
  #tol is a tolerance threshold used in several places: convergence, shrinkage to values close to zero, and step lengths. In these places, numbers below the threshold are considered zero.
  
  
  #standardize data
  mean.vec<- colMeans(inputdf)
  var.vec<- apply(inputdf,2,var)
  df<- (inputdf-matrix(mean.vec,nrow = nrow(inputdf), ncol = ncol(inputdf), byrow = TRUE))/matrix(sqrt(var.vec),nrow = nrow(inputdf), ncol = ncol(inputdf), byrow = TRUE)
  colnames(df)<- colnames(inputdf)
  if(standardize.y==TRUE){
    mean.y<- mean(y)
    sd.y<- sd(y)
    y<- (y-mean.y)/sd.y
  }
  
  #create new spline data and contraints
  spline.list<- list()
  constraint.list<- list()
  cut.list<- list()
  if(!is.null(spline.pos)){
    for(i in seq_along(spline.pos)){
      #prepare splines as columns
      spline.data<- df[,spline.pos[i]]
      no.knots<- spline.knots[i]
      cuts<- quantile(spline.data, probs=seq(0,1,length.out=(2+no.knots)))
      addage<- as.data.frame(matrix(0, nrow = nrow(df), ncol = no.knots+1))
      for(j in seq(1,no.knots+1)){
        selector<- spline.data>=cuts[j] & spline.data<=cuts[j+1]
        addage[selector,j]<- spline.data[selector]
      }
      colnames(addage)<- paste0(colnames(df)[spline.pos[i]], seq(1,no.knots+1))
      cut.list[[i]]<- cuts
      
      #spline constraints
      part.constraint.matrix<- matrix(0,nrow = no.knots*3, ncol = no.knots*4)
      colnames(part.constraint.matrix)<- c(
        paste0("a_",seq(1,no.knots+1)),
        paste0("b_",seq(1,no.knots+1)),
        paste0("c_",seq(2,no.knots)),
        paste0("d_",seq(2,no.knots))
      )
      
      acc<- function(string){
        which(string==colnames(part.constraint.matrix))
      }
      
      k<- cuts[-c(1,length(cuts))]
      
      part.constraint.matrix[1,acc("a_1")]<- -1
      part.constraint.matrix[1,acc("b_1")]<- -1
      part.constraint.matrix[1,acc("a_2")]<- k[1]^3
      part.constraint.matrix[1,acc("b_2")]<- k[1]^2
      part.constraint.matrix[1,acc("c_2")]<- k[1]
      part.constraint.matrix[1,acc("d_2")]<- 1
      
      part.constraint.matrix[2,acc(paste0("a_",no.knots))]<- -(k[no.knots])^3
      part.constraint.matrix[2,acc(paste0("b_",no.knots))]<- -(k[no.knots])^2
      part.constraint.matrix[2,acc(paste0("c_",no.knots))]<- -(k[no.knots])
      part.constraint.matrix[2,acc(paste0("d_",no.knots))]<- -1
      part.constraint.matrix[2,acc(paste0("a_",no.knots+1))]<- k[no.knots]
      part.constraint.matrix[2,acc(paste0("b_",no.knots+1))]<- 1
      
      part.constraint.matrix[3,acc("a_1")]<- -1
      part.constraint.matrix[3,acc("a_2")]<- 3*(k[1])^2
      part.constraint.matrix[3,acc("b_2")]<- 2*k[1]
      part.constraint.matrix[3,acc("c_2")]<- 1
      
      part.constraint.matrix[4,acc(paste0("a_",no.knots+1))]<- -1
      part.constraint.matrix[4,acc(paste0("a_",no.knots))]<- 3*(k[no.knots])^2
      part.constraint.matrix[4,acc(paste0("b_",no.knots))]<- 2*(k[no.knots])
      part.constraint.matrix[4,acc(paste0("c_",no.knots))]<- 1
      
      part.constraint.matrix[5,acc("a_2")]<- 6*k[1]
      part.constraint.matrix[5,acc("b_2")]<- 2
      
      part.constraint.matrix[6,acc(paste0("a_",no.knots))]<- 6*k[no.knots]
      part.constraint.matrix[6,acc(paste0("b_",no.knots))]<- 2
      
      if(no.knots>2){
        ticker<- 7
        for(j in seq(1,no.knots-2)){
          part.constraint.matrix[ticker,acc(paste0("a_",j+1))]<- -(k[j+1])^3
          part.constraint.matrix[ticker,acc(paste0("b_",j+1))]<- -(k[j+1])^2
          part.constraint.matrix[ticker,acc(paste0("c_",j+1))]<- -(k[j+1])
          part.constraint.matrix[ticker,acc(paste0("d_",j+1))]<- -1
          part.constraint.matrix[ticker,acc(paste0("a_",j+2))]<- (k[j+1])^3
          part.constraint.matrix[ticker,acc(paste0("b_",j+2))]<- (k[j+1])^2
          part.constraint.matrix[ticker,acc(paste0("c_",j+2))]<- (k[j+1])
          part.constraint.matrix[ticker,acc(paste0("d_",j+2))]<- 1
          
          ticker<- ticker+1
          
          part.constraint.matrix[ticker,acc(paste0("a_",j+1))]<- 3*(k[j+1])^2
          part.constraint.matrix[ticker,acc(paste0("b_",j+1))]<- 2*k[j+1]
          part.constraint.matrix[ticker,acc(paste0("c_",j+1))]<- 1
          part.constraint.matrix[ticker,acc(paste0("a_",j+2))]<- -3*(k[j+1])^2
          part.constraint.matrix[ticker,acc(paste0("b_",j+2))]<- -2*k[j+1]
          part.constraint.matrix[ticker,acc(paste0("c_",j+2))]<- -1
          
          ticker<- ticker+1
          
          part.constraint.matrix[ticker,acc(paste0("a_",j+1))]<- 6*k[j+1]
          part.constraint.matrix[ticker,acc(paste0("b_",j+1))]<- 2
          part.constraint.matrix[ticker,acc(paste0("a_",j+2))]<- -6*k[j+1]
          part.constraint.matrix[ticker,acc(paste0("b_",j+2))]<- -2
          
          ticker<- ticker+1
        }
      }
      
      
      
      #create data to be used with constraint matrix
      
      addage_2<- as.data.frame(matrix(NA, nrow = nrow(df), ncol = ncol(part.constraint.matrix)))
      for(j in seq(1,ncol(part.constraint.matrix))){
        var.name<- colnames(part.constraint.matrix)[j]
        var.letter<- substr(var.name,1,1)
        var.index<- as.numeric(substr(var.name,3,3))
        intercept<- FALSE
        if(var.letter=="a"){
          if(var.index==1){
            exponent<- 1
          }else{
            exponent<- 3
          }
        }else if(var.letter=="b"){
          if(var.index==1){
            intercept<- TRUE
          }else{
            exponent<- 2
          }
        }else if(var.letter=="c"){
          exponent<- 1
        }else if(var.letter=="d"){
          intercept<- TRUE
        }
        
        if(intercept){
          addage_2[,j]<- as.numeric(addage[,var.index]!=0)
        }else{
          addage_2[,j]<- addage[,var.index]^exponent
        }
        colnames(addage_2)[j]<- paste0(colnames(addage)[var.index],"_", var.name)
      }
      colnames(part.constraint.matrix)<- paste0("s",i,"_",colnames(part.constraint.matrix))
      constraint.list[[i]]<- part.constraint.matrix
      spline.list[[i]]<- addage_2
    }
    
    df<- df[,-spline.pos]
  }
  
  df$intercept<- 1
  no.not.spline.params<- ncol(df)
  
  for (i in seq_along(spline.list)) {
    df<- cbind(df,spline.list[[i]])
  }
  
  constraint.matrix<- matrix(0,ncol = no.not.spline.params+sum(spline.knots)*4, nrow = sum(spline.knots)*3)
  start.col<- no.not.spline.params+1
  start.row<- 1
  for(i in seq_along(constraint.list)){
    n<- nrow(constraint.list[[i]])
    p<- ncol(constraint.list[[i]])
    constraint.matrix[seq(start.row,start.row+n-1),seq(start.col,start.col+p-1)]<- constraint.list[[i]]
    start.row<- start.row+n
    start.col<- start.col+p
  }
  
  #loss functions and their gradients
  if(sel.loss.function=="quantile"){
    loss.function<- function(beta.vec){
      n<- nrow(df)
      residual<- y-as.matrix(df)%*%beta.vec
      positive<- residual>0
      likelihood<- (sum(sel.quantile*residual[positive])+sum((sel.quantile-1)*residual[!positive]))/n
      linear.constraint<- ticker^lc.rho *sqrt(0.5*sum((constraint.matrix%*%beta.vec)^2))/nrow(constraint.matrix)
      regularization<- lambda*(alpha*sum(abs(beta.vec))+(1-alpha)*sqrt(sum(beta.vec^2)))
      return(likelihood + linear.constraint+ regularization)
    }
    loss.gradient<- function(ind,beta.vec){
      dataframe<- as.matrix(df)
      likelihood<- (sel.quantile-1+as.numeric(dataframe%*%beta.vec))%*%dataframe[,ind]/nrow(dataframe)
      linear.constraint<- ticker^lc.rho *constraint.matrix[,ind]%*% (constraint.matrix%*%beta.vec) /(tol+nrow(constraint.matrix)*sqrt(2*sum((constraint.matrix%*%beta.vec)^2)))
      regularization<- lambda*(alpha*sign(beta.vec[ind])+(1-alpha)*2*beta.vec[ind]/sqrt(sum((beta.vec)^2)))
      out<- as.numeric(likelihood +linear.constraint +regularization)
      return(out)
    }
  }else if(sel.loss.function=="proportion"){
    loss.function<- function(beta.vec){
      fitted<- as.numeric(as.matrix(df)%*%beta.vec)
      likelihood<- ((1-y)%*%fitted+sum(log(1+exp(-fitted))))/nrow(df)
      linear.constraint<- ticker^lc.rho *sqrt(0.5*sum((constraint.matrix%*%beta.vec)^2))/nrow(constraint.matrix)
      regularization<- lambda*(alpha*sum(abs(beta.vec[1:no.not.spline.params]))+(1-alpha)*sqrt(sum(beta.vec[1:no.not.spline.params]^2)))
      return(likelihood + linear.constraint+ regularization)
    }
    loss.gradient<- function(ind, beta.vec){
      likelihood<- as.matrix(df)[,ind]%*%((1+exp(-as.matrix(df)%*%beta.vec))^(-1)-y) /nrow(df)
      linear.constraint<- ticker^lc.rho *constraint.matrix[,ind]%*% (constraint.matrix%*%beta.vec) /(tol+nrow(constraint.matrix)*sqrt(2*sum((constraint.matrix%*%beta.vec)^2)))
      regularization<- lambda*(alpha*sign(beta.vec[ind])+(1-alpha)*2*beta.vec[ind]/sqrt(sum((beta.vec)^2)))
      out<- as.numeric(likelihood +linear.constraint +regularization)
      return(out)
    }
  # }else if(sel.loss.function=="cross-entropy"){
  #   loss.function<- function(beta.vec){
  #     beta.matrix<- matrix(beta.vec,nrow = ncol(df))
  #     fitted<- exp(as.matrix(df)%*%beta.matrix)
  #     q<- diag(rowSums(fitted)^(-1))%*%fitted
  #     y_dummy<- model.matrix(~as.factor(y)+0)
  #     likelihood<- sum(y_dummy*q) #sum?
  #     linear.constraint<- 0.5*sum((constraint.matrix%*%beta.matrix)^2)
  #     regularization<- lambda*(alpha*sum(abs(beta.vec))+(1-alpha)*sqrt(sum(beta.vec^2)))
  #     return(likelihood + linear.constraint+ regularization)
  #   }
  #   loss.gradient<- function(ind, beta.vec){
  #     k<- (ind-1) %% ncol(df)+1
  #     m<- ((ind-1) %/% ncol(df))+1
  #     beta.matrix<- matrix(beta.vec,nrow = ncol(df))
  #     fitted<- exp(as.matrix(df)%*%beta.matrix)
  #     y_dummy<- model.matrix(~as.factor(y)+0)
  #     likelihood_1<- y_dummy[,m]%*%as.matrix(df)[,k]
  #     likelihood_2<- sum(t(y_dummy)%*%diag((rowSums(fitted))^(-1))%*%(as.matrix(df)[,k]*fitted[,m]))
  #     likelihood<- likelihood_1-likelihood_2
  #     linear.constraint<- constraint.matrix[,k]%*% as.numeric(constraint.matrix%*%beta.matrix[,m])
  #     regularization<- lambda*(alpha*sign(beta.vec[ind])+(1-alpha)*2*beta.vec[ind]/sqrt(sum((beta.vec)^2)))
  #     out<- as.numeric(likelihood +linear.constraint +regularization)
  #     return(out)
  #   }
  }
  
  #start values
  beta.list<- list()
  if(sel.loss.function=="cross-entropy"){
    no.params<- length(levels(as.factor(y)))*ncol(constraint.matrix)
  }else{
    no.params<- ncol(constraint.matrix)
  }
  
  if(ncol(df)>nrow(df)){
    base.data<- rbind(df,as.matrix(df)[sample(1:nrow(df),ncol(df)-nrow(df)),])
  }else{
    base.data<- df
  }
  
  if(sel.loss.function=="quantile"){
    form<- as.formula(paste("y~0+",paste(colnames(df), sep = "", collapse = "+ ")))
    beta_start<- numeric(no.params)
    beta_start[1:no.not.spline.params]<- rq(formula = form, tau = sel.quantile, data = base.data, method = "fn")$coefficients[1:no.not.spline.params]
  }else if(sel.loss.function=="proportion"){
    form<- as.formula(paste("y~0+",paste(colnames(df), sep = "", collapse = "+ ")))
    beta_start<- numeric(no.params)
    beta_start[1:no.not.spline.params]<- glm(formula = form, family = binomial, data = base.data)$coefficients[1:no.not.spline.params]
  # }else if(sel.loss.function=="cross-entropy"){
  #   beta_start<- optim(rnorm(no.params), fn = function(beta.vec){
  #     beta.matrix<- matrix(beta.vec,nrow = ncol(df))
  #     fitted<- exp(as.matrix(df)%*%beta.matrix)
  #     q<- diag(rowSums(fitted)^(-1))%*%fitted
  #     y_dummy<- model.matrix(~as.factor(y)+0)
  #     likelihood<- sum(y_dummy*q)
  #     return(likelihood)
  #   },
  #   gr = function(beta.vec){
  #     k<- (seq(1,length(beta.vec))-1) %% ncol(df)+1
  #     m<- ((seq(1,length(beta.vec))-1) %/% ncol(df))+1
  #     beta.matrix<- matrix(beta.vec,nrow = ncol(df))
  #     fitted<- exp(as.matrix(df)%*%beta.matrix)
  #     y_dummy<- model.matrix(~as.factor(y)+0)
  #     likelihood_1<- y_dummy[,m]%*%as.matrix(df)[,k]
  #     likelihood_2<- sum(t(y_dummy)%*%diag((rowSums(fitted))^(-1))%*%(as.matrix(df)[,k]*fitted[,m]))
  #     likelihood<- likelihood_1-likelihood_2
  #     return(likelihood)
  #   },
  #   method = "BFGS")
  }
  
  beta.list<- list()
  for(i in seq(1,no.starts)){
    beta.list[[i]]<- beta_start*seq(1,0, length.out = no.starts+1)[i]
  }
  
  #optimization process
  
  z_fun<- function(z){
    beta_start<- beta.list[[z]]
    ticker<- 1
    convergence<- FALSE
    beta_new<- beta_start
    beta_sugg<- beta_new
    step.vec<- rep(step.size,length(beta_start))
    while(ticker<=max.iter && !convergence && any(step.vec>tol)){
      beta_baseline<- beta_new
      selected.vars<- seq_along(beta_start)[step.vec>=tol]
      for(i in selected.vars){
        beta_sugg[i]<- beta_new[i]-step.vec[i]*loss.gradient(i, beta_new)
        while(loss.function(beta_sugg)>loss.function(beta_new)){
          step.vec[i]<- 0.5*step.vec[i]
          beta_sugg[i]<- beta_new[i]-step.vec[i]*loss.gradient(i, beta_new)
        }
        if(abs(beta_sugg[i])<=tol){
          # beta_round<- beta_sugg
          # beta_round[i]<- 0
          # if(loss.function(beta_round)<=loss.function(beta_sugg)){
          #   beta_sugg<- beta_round
          # }
          beta_sugg[i]<- 0
        }
        beta_new<- beta_sugg
      }
      ticker<- ticker+1
      convergence<- loss.function(beta_baseline)-loss.function(beta_new) <=tol
    }
    out.list<- list()
    out.list$converged<- convergence
    out.list$beta<- beta_new
    out.list$loss_old<- loss.function(beta_start)
    out.list$loss_new<- loss.function(beta_new)
    out.list$final_step_length<- step.vec
    out.list$iterations<- ticker-1
    return(out.list)
  }
  
  if(no.starts>1){
    coordinate.cluster<- makeCluster(no.workers)
    clusterExport(cl = coordinate.cluster, varlist =  c("beta.list", "loss.function", "loss.gradient", "df", "y", "constraint.matrix", "max.iter", "step.size", "tol"), envir = environment())
    results<- parLapply(cl = coordinate.cluster,seq_along(beta.list), fun = z_fun)
  }else{
    results<- lapply(seq_along(beta.list), FUN = z_fun)
  }
  
  out<- list()
  out$converged<- numeric(no.starts)
  out$loss_old<- numeric(no.starts)
  out$loss_new<- numeric(no.starts)
  out$final_step_length<- matrix(NA,ncol = no.params, nrow = no.starts)
  out$iterations<- numeric(no.starts)
  out$beta<- matrix(NA,ncol = no.params, nrow = no.starts)
  
  for(i in seq(1,no.starts)){
    out$converged[i]<- results[[i]]$converged
    out$beta[i,]<- results[[i]]$beta
    out$loss_old[i]<- results[[i]]$loss_old
    out$loss_new[i]<- results[[i]]$loss_new
    out$final_step_length[i,]<- results[[i]]$final_step_length
    out$iterations[i]<- results[[i]]$iterations
  }
  colnames(out$beta)<- colnames(df)
  
  for(i in seq_along(cut.list)){
    old.names<- names(out)
    out$im<- cut.list[[i]]
    names(out)<- c(old.names,paste0("cuts_",i))
  }
  
  return(out)
}

