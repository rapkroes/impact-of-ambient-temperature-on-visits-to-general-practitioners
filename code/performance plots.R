#evaluation performance plots: quantile

performance.plots<- function(predicted.var, di, practiceID, y.name, y.range, no.threads = 4){
  # Creates plots to evaluate the peformance of the estimated models. 
  poss.vars<- c("age", "gender", "phi", "chronic")
  mapping<- c("age", "female", "PKV", "no_all_chronic_diseases")
  
  if(predicted.var %in% c("age", "chronic")){
    quants<- c(".05", ".25", ".50", ".75", ".95")
    ref.df<- full.df_7|>
      filter(PraxisID == practiceID)|>
      arrange(TG_DateNum)
    
    #predictive df
    pred.df<- data.matrix(df_qx(ref.df, di = di, q = 1))
    pred.df<- pred.df[!duplicated(ref.df$TG_DateNum),]
    pred.matrix<- matrix(NA, ncol = 5, nrow = nrow(pred.df))
    for(i in seq(1, 5)){
      booster<- get(paste0("model_", predicted.var, quants[i], "_", di), 
                    envir = .GlobalEnv)
      pred.matrix[,i]<- predict(object = booster, newdata = pred.df)
    }
    
    #real data df
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
    
    prefix<- paste0("Practice ", practiceID, ", ", predicted.var, " quantiles")
    quants_2<- c("5%", "20%", "50%", "75%", "95%")
    quants_3<- c("5pct", "20pct", "50pct", "75pct", "95pct")
    plot.names<- paste0(prefix, " (", quants_2, ")")
    file.names<- paste0(prefix, " ", quants_3, "_", di, ".png")
  }else if(predicted.var %in% c("gender", "phi")){
    ref.df<- full.df_7|>
      filter(PraxisID == practiceID)|>
      arrange(TG_DateNum)
    
    #predictive df
    pred.df<- data.matrix(df_qx(ref.df, di = di, q = 1))
    pred.df<- pred.df[!duplicated(ref.df$TG_DateNum),]
    booster<- get(paste0("model_", predicted.var, "_", di), envir = .GlobalEnv)
    pred.matrix<- matrix(predict(object = booster, newdata = pred.df), ncol = 1)
    
    #real data df
    pred.mapping<- mapping[predicted.var == poss.vars]
    ref.df<- ref.df[c("TG_DateNum", pred.mapping)]
    ref.dates<- unique(ref.df$TG_DateNum)
    perf.cl<- makeCluster(no.threads)
    dist.env<- environment()
    clusterExport(cl = perf.cl, varlist = c("ref.dates", "ref.df", "mean"),
                  envir = dist.env)
    raw.list<- parLapply(cl = perf.cl, X = seq_along(ref.dates), function(k){
      ref.date<- ref.dates[k]
      ref.vec<- ref.df[ref.df$TG_DateNum == ref.date, 2]
      out<- mean(ref.vec, na.rm = TRUE)
      out<- c(ref.date, out)
      return(out)
    })
    ref.matrix<- matrix(unlist(raw.list), ncol = 2, byrow = TRUE)
    colnames(ref.matrix)<- c("TG_DateNum", "proportion")
    ref.matrix<- as.data.frame(ref.matrix)|>
      arrange(TG_DateNum)
    
    if(predicted.var == "phi"){
      plot.names<- paste0("Practice ", practiceID, 
                          ", proportion privately insured")
      file.names<- paste0("Practice ", practiceID, 
                          ", proportion privately insured", "_", di, ".png")
    }else{
      plot.names<- paste0("Practice ", practiceID, 
                          ", proportion female patients")
      file.names<- paste0("Practice ", practiceID, 
                          ", proportion female patients", "_", di, ".png")
    }
    
  }
  
  #generate plots
  no.plots<- ncol(pred.matrix)
  for(i in seq(1, no.plots)){
    #x axis configuration
    data.start.month<- month(TG_DateNum2date(ref.matrix[1, 1]))
    data.end.month<- month(TG_DateNum2date(ref.matrix[nrow(ref.matrix), 1]))
    if(data.start.month == 1){
      axis.start.month<- 1
    }else{
      axis.start.month<- data.start.month - (data.start.month - 1) %% 3
    }
    if(data.end.month == 12){
      axis.end.month<- 12
    }else{
      axis.end.month<- data.end.month + 3 - (data.end.month - 1) %% 3
    }
    data.start.year<- isoyear(TG_DateNum2date(ref.matrix[1, 1]))
    data.end.year<- isoyear(TG_DateNum2date(ref.matrix[nrow(ref.matrix), 1]))
    if(data.end.month >= 10){
      axis.end.year<- data.end.year + 1
    }else{
      axis.end.year<- data.end.year
    }
    axis.start.date<- as.Date(paste(data.start.year, axis.start.month, "01",
                                    sep = "-"))
    axis.end.date<- as.Date(paste(axis.end.year, axis.end.month, "01", 
                                  sep = "-"))
    no.years<- axis.end.year - data.start.year + 1
    tick.df<- data.frame(
      month= rep(c(1, 4, 7, 10), no.years),
      year= sort(rep(seq(data.start.year, axis.end.year), 4))
    )
    tick.df<- tick.df|>
      filter((month >= axis.start.month ) | (year != data.start.year))|>
      filter((month <= axis.end.month) | (year != axis.end.year))
    tick.df$date<- as.Date(paste(tick.df$year, tick.df$month, "01", sep = "-"))
    tick.df$print_date<- paste(tick.df$month, tick.df$year, sep = "/")
    tick.df$TG_DateNum<- date2TG_DateNum(tick.df$date)
    
    png(file.names[i])
    plot(ref.matrix[,i + 1] ~ ref.matrix[,1], type = "l", xlab = "time", 
         ylab = y.name, main = plot.names[i], ylim = y.range, las = 1, 
         xaxt = "n", xlim = range(tick.df$TG_DateNum), 
         sub = paste("black solid line: real data,", 
                     "red dotted line: prediction based on", di))
    axis(side = 1, at = tick.df$TG_DateNum, labels = tick.df$print_date)
    lines(x = ref.matrix[,1], y = pred.matrix[,i], col = "red", lty = 3)
    dev.off()
  }
}

performance.plots(predicted.var = "age", di = "TDI", practiceID = 6, y.name = "age", y.range = c(0, 80), no.threads = 4)
performance.plots(predicted.var = "gender", di = "TDI", practiceID = 6, y.name = "proportion", y.range = c(0, 1), no.threads = 4)
performance.plots(predicted.var = "phi", di = "TDI", practiceID = 6, y.name = "proportion", y.range = c(0, 1), no.threads = 4)
performance.plots(predicted.var = "chronic", di = "TDI", practiceID = 6, y.name = "no. chronic diseases", y.range = c(0, 5), no.threads = 4)
# find way to estimate proportins with LightGBM (!= binary classification!)-> cross-entropy?!

