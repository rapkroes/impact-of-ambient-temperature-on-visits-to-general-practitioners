#evaluation performance plots: quantile

performance.plots(predicted.var, di, practiceID, no.threads = 4){
  # Creates plots to evaluate the peformance of the estimated models. 
  poss.vars<- c("age", "gender", "phi", "chronic")
  mapping<- c("age", "female", "PKV", "no_all_chronic_diseases")
  
  if(predicted.var %in% c("age", "chronic")){
    quants<- c(".05", ".25", ".50", ".75", ".95")
    ref.df<- full.df_7|>
      filter(PraxisID == practiceID)|>
      arrange(TG_DateNum)
    
    #predictive df
    pred.df<- df_qx(ref.df, di = di, q = 1)
    pred.df<- pred.df[!duplicated(ref.df$TG_DateNum),]
    pred.matrix<- matrix(NA, ncol = 5, nrow = nrow(pred.df))
    for(i in seq(1, 5)){
      booster<- get(paste0("model_", predicted.var, quants[i], "_", di), 
                    envir = .GlobalEnv)
      pred.matrix[,i]<- predict(object = booster, newdata = pred.df)
    }
    
    #real df
    pred.mapping<- mapping[predicted.var == poss.vars]
    ref.df<- ref.df[c("TG_DateNum", pred.mapping)]
    ref.dates<- unique(ref.df$TG_DateNum)
    perf.cl<- makeCluster(no.threads)
    dist.env<- environment()
    clusterExport(cl = perf.cl, varlist = c("ref.dates", "ref.df", "quantile"),
                  envir = dist.env)
    raw.list<- parLapply(cl = perf.cl, X = seq_along(ref.dates), function(k){
      ref.date<- ref.dates[k]
      ref.vec<- ref.df[ref.df$TG_DateNum == ref.date, 2]
      out<- quantile(ref.vec, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), 
                     na.rm = TRUE)
      out<- c(ref.date, out)
      return(out)
    })
    ref.matrix<- matrix(unlist(raw.list), ncol = 6, byrow = TRUE)
    colnames(ref.matrix)<- c("TG_DateNum", paste0("q", quants))
    ref.matrix<- as.data.frame(ref.matrix)|>
      arrange(TG_DateNum)
  }
  #generate plots
  
}

im<- list()
im[[1]]<- 1:4
im[[2]]<- 5:8
matrix(unlist(im), ncol = 4, byrow = TRUE)
rm(im)
