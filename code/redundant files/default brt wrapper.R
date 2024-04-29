#####default wrapper

wrapper_interior<- function(sdi = FALSE, lr, no.leaves, max.depth, 
                            min.data.in.leaf, feature.fraction, cat.l2,
                            extra.trees = FALSE, top.rate, other.rate,
                            cat.smooth, path.smooth,
                            inputdf, y, est.type, alpha = 0.5, cv = 5L,
                            no.trees = 100L, no.threads = 4L, 
                            early.stopping = 10L, seed = NA){
  # interior wrapper for cross-validation of the model.
  # hyperpars:
  # 1. learning rate (>0)
  # 2. num_leaves (integer >0)
  # if sdi = TRUE
  # sdi[1] number of lags/ size for the weights of sdi
  # sdi[2] alpha/ shape parameter 1 for the weights of sdi
  # sdi[3] beta/ shape parameter 2 for the weights of sdi
  # sdi[4:6] theta values for the daily temperature of the sdi
  # sdi[7:9] rho values for the daily humidity of the sdi
  # sdi[10] tau value for the daily interaction between temperature and humidity
  
  if(est.type=="quantile"){
    lossfct<- "quantile"
  }else if(est.type=="binary"){
    lossfct<- "binary_logloss"
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
    im<- cbind(inputdf[,!col.selector],sdi.vec)
    colnames(im)<- c(colnames(inputdf)[!col.selector],"sdi")
    im<- data.matrix(im)
    factor.vars<- colnames(im)[!colnames(im) %in% blacklist]
    df<- lgb.Dataset(data = im,
                     categorical_feature = factor.vars)
  }else{
    factor.vars<- colnames(inputdf)[!colnames(inputdf) %in% blacklist]
    df<- lgb.Dataset(data.matrix(inputdf),
                     categorical_feature = factor.vars)
  }

  if(!is.na(seed)){
    set.seed(seed)
  }
  
  parameters<- list(objective = est.type, data_sample_strategy = "goss", 
                    num_trees = no.trees,
                    num_threads = no.threads,
                    learning_rate = lr, num_leaves = no.leaves,
                    use_missing = TRUE,
                    zero_as_missing = FALSE,
                    max_depth = max.depth,
                    min_data_in_leaf = min.data.in.leaf,
                    feature_fraction = feature.fraction,
                    extra_trees = extra.trees,
                    top_rate = top.rate,
                    other_rate = other.rate,
                    cat_l2 = cat.l2,
                    cat_smooth = cat.smooth,
                    path_smooth = path.smooth,
                    alpha = alpha)

  results<- list()
  out<- numeric(cv)
  for(i in seq(1,cv)){
    valid.selector<- seq(i, nrow(inputdf), by= cv)
    df<- lgb.Dataset(data.matrix(inputdf)[-valid.selector,],
                     label = y[-valid.selector])
    valid.df<- lgb.Dataset(data.matrix(inputdf)[valid.selector,],
                           categorical_feature = factor.vars,
                           label = y[valid.selector],
                           reference = df)
    results[[i]]<- lgb.train(params = parameters, data = df,nrounds = no.trees,
                             valids = list(my_validation = valid.df), obj = est.type,verbose = 1, record = TRUE,
                             categorical_feature = factor.vars, 
                             early_stopping_rounds = early.stopping,
                             eval = lossfct)
    out[i]<- results[[i]]$best_score
  }
  return(mean(out))
}
library(tictoc)
n_max<- 10^7
set.seed(1)
enlarger<- sample(1:nrow(full.df_7),n_max, replace = TRUE)
tic()
sim_1<- wrapper_interior(sdi = FALSE, lr = 0.1, no.leaves = 10, max.depth =4, 
                         min.data.in.leaf = 50, feature.fraction = 0.8, 
                         extra.trees = FALSE, top.rate = 0.2, other.rate = 0.1,
                         cat.smooth = 10, path.smooth = 0.1, cat.l2 = 10,
                         inputdf = df_qx(di = "TDI", q = 1)[enlarger,], y = full.df_7$age[enlarger],
                         est.type = "quantile", 
                         cv = 2L, no.trees = 100L, no.threads = 4L, 
                         early.stopping = 10L,  seed = NA)
toc() #10^7 data points, total 500 iterations, 195 sec; 10^7 data points, total 200 iterations, 70 sec
tic()
sim_1<- wrapper_interior(sdi = FALSE, lr = 0.1, no.leaves = 10, max.depth =4, 
                         min.data.in.leaf = 1e8, feature.fraction = 0.8, 
                         extra.trees = FALSE, top.rate = 0.2, other.rate = 0.1,
                         cat.smooth = 10, path.smooth = 0.1, cat.l2 = 10,
                         inputdf = df_qx(di = "TDI", q = 1), y = full.df_7$age,
                         est.type = "quantile", 
                         cv = 1L, no.trees = 100L, no.threads = 4L, seed = NA)
toc()
library(GA)
# Optimisation Age
ga_wrapper_1<- function(x){
  
  wrapper_interior(sdi = FALSE, lr = x[1], no.leaves = round(x[2]), max.depth =round(x[3]), 
                   min.data.in.leaf = round(x[4]), feature.fraction = x[5], 
                   extra.trees = as.logical(round(x[6])), top.rate = x[7], other.rate = x[8],
                   cat.smooth = x[9], path.smooth = x[10], cat.l2 = x[11],
                   inputdf = df_qx(di = "TDI", q = 1), y = full.df_7$age,
                   est.type = "quantile", 
                   cv = 2L, no.trees = 100L, no.threads = 4L, 
                   early.stopping = 10L,  seed = NA)
}
sim_ga_1<- ga(type = "real-valued", fitness = ga_wrapper_1,
              lower = c(0,2,0,
                        100,0,
                        0,0,0,
                        0,0,0),
              upper = c(5,31,31,
                        1e10,1,
                        1,0.5,0.5,
                        10,1e10,1e10),
              maxiter = 4,
              keepBest = TRUE
              )

tic()
im<- wrapper_default(hyperpars = c(0.1,10), inputdf = df_qx(di = "TDI", q = 1),
                     y = full.df_7$age, est.type = "quantile", seed = 12345)
toc()

sim_gena_self_1<- genetic.algorithm(optim.seed = 12336, n = 50, 
                                    pcrossover = 0.8, pmutation = 0.1, 
                                    maxiter = 10, elitism = 4,
                                    sdi = FALSE, lr = c(0,1), 
                                    no.leaves = c(2,40), max.depth = c(1,10),
                                    min.data.in.leaf = c(2,1e7), 
                                    feature.fraction = c(0,1), cat.l2 = c(0,99),
                                    extra.trees = c(0,1), top.rate = c(0,0.5),
                                    other.rate = c(0,0.5), cat.smooth = c(0,99),
                                    path.smooth = c(0,99),
                                    inputdf = df_qx(di = "TDI", q = 1),
                                    y = full.df_7$age, est_type = "quantile",
                                    alpha = 0.5, cv = 2L, no_trees = 100L,
                                    no_threads = 4L, early_stopping = 10L,
                                    seed = NA
                                    )


genetic.algorithm<- function(optim.seed, n, pcrossover, pmutation, maxiter, 
                             elitism,
                             sdi = FALSE, lr, no.leaves, max.depth, 
                             min.data.in.leaf, feature.fraction, cat.l2,
                             extra.trees = FALSE, top.rate, other.rate,
                             cat.smooth, path.smooth,
                             inputdf, y, est_type, alpha = 0.5, cv = 5L,
                             no_trees = 100L, no_threads = 4L, 
                             early_stopping = 10L, seed = NA){
  
  # optim.seed: start seed for optimization
  # n: number of members in the population
  # pcrossover: probability for chromosomes to merge
  # pmutation: probability for a mutation to occur
  # maxiter: integer number of generations that are trained
  # elitism: integer number of parent models that are permitted to procreate. Sorted best to worst, the first to elitism-th model form the parent model pool.
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
    X_2<- as_tibble(matrix(rt(10*n), ncol = 10))
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
  Y_round<- signif(Y, digits = 2)
  generation.results<- as.data.frame(cbind(X, Y, Y_round, delta_t))|>
    arrange(Y_round,delta_t)
  results[[1]]<- generation.results
  best.in.class<- generation.results[1,]
  
  # Generation 2
  for(s in seq(2,maxiter)){
    set.seed(optim.seed + s)
    X_new<- X
    mutation.matrix<- matrix(runif(n * ncol(X)), nrow = n) <= pmutation
    for(i in seq(1,n)){
      parents<- X[sample(1:elitism, 2, replace = FALSE),]
      for(j in seq(1,ncol(X))){
        if(mutation.matrix[i,j]){
          if(j <= 11){
            X_new[i,j]<- runif(1, min = limits[j,1], max = limits[j,2])
          }else{
            X_new[i,j]<- rt(1, 8)
          }
        }else{
          X_new[i,j]<- sample(c(parents[1,j],parents[2,j], mean(parents[,j])), 1,
                              prob = c(rep(0.5*(1 - pcrossover), 2), pcrossover))
        }
      }
    }
    X<- X_new
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
    Y_round<- signif(Y, digits = 2)
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


wrapper_default<- function(hyperpars,inputdf = full.df_7, y = full.df_7$age,
                               est.type  ="quantile", cv = 5, no.trees = 200,
                               sdi = FALSE, gpu = FALSE, seed = NA){
  # default wrapper for cross-validation of the model.
  # hyperpars:
  # 1. learning rate (>0)
  # 2. num_leaves (integer >0)
  # if sdi = TRUE
  # 3. number of lags/ size for the weights of sdi
  # 4. alpha/ shape parameter 1 for the weights of sdi
  # 5. beta/ shape parameter 2 for the weights of sdi
  # 6.-8. theta values for the daily temperature of the sdi
  # 9.-11. rho values for the daily humidity of the sdi
  # 12. tau value for the daily interaction between temperature and humidity
  
  if(est.type=="quantile"){
    lossfct<- "quantile"
  }else if(est.type=="binary"){
    lossfct<- "binary_logloss"
  }else if(est.type=="cross_entropy"){
    lossfct<- "xentropy"
  }
  
  if(isTRUE(sdi)){
    sdi.weights<- dbetabinom.ab(x = seq(0,hyperpars[3]), size = hyperpars[3],
                                shape1 = hyperpars[4], shape2 = hyperpars[5])
    sdi.vec<- SDI(df = inputdf, w = sdi.weights, theta = hyperpars[6:8],
                  rho = hyperpars[9:11], tau = hyperpars[12])
    
    col.selector<- grepl("temperature", colnames(inputdf)) | grepl("humidity", colnames(inputdf))
    im<- cbind(inputdf[,!col.selector],sdi.vec)
    colnames(im)<- c(colnames(inputdf)[!col.selector],"sdi")
    df<- lgb.Dataset(im)
  }else{
    df<- lgb.Dataset(as.matrix(inputdf))
  }
  
  processing.unit<- "cpu"
  if(isTRUE(gpu)){
    processing.unit<- "gpu"
  }
  
  if(!is.na(seed)){
    set.seed(seed)
  }
  
  parameters<- list(objective = est.type, num_trees = no.trees,
                    learning_rate = hyperpars[1], num_leaves = hyperpars[2],
                    device_type = processing.unit)
  
  results<- lgb.cv(params = parameters, data = df,nrounds = no.trees,
                   label = y, obj = est.type,verbose = 1, record = TRUE,
                   nfold = cv, early_stopping_rounds = 50L,
                   eval = lossfct)
  return(results)
}

wrapper_default<- function(hyperpars,inputdf = full.df_7, y = full.df_7$age,
                               est.type  ="quantile", cv = 5, no.trees = 100,
                               sdi = FALSE, gpu = FALSE, seed = NA){
  # default wrapper for cross-validation of the model.
  # hyperpars:
  # 1. learning rate (>0)
  # 2. num_leaves (integer >0)
  # if sdi = TRUE
  # 3. number of lags/ size for the weights of sdi
  # 4. alpha/ shape parameter 1 for the weights of sdi
  # 5. beta/ shape parameter 2 for the weights of sdi
  # 6.-8. theta values for the daily temperature of the sdi
  # 9.-11. rho values for the daily humidity of the sdi
  # 12. tau value for the daily interaction between temperature and humidity
  
  #hyperpars<- c(lr,nl,alpha,beta,theta1,theta2,theta3,rho1,rho2,rho3,tau)
  set.seed(seed)
  
  if(est.type=="quantile"){
    lossfct<- "quantile"
  }else if(est.type=="binary"){
    lossfct<- "binary_logloss"
  }else if(est.type=="cross_entropy"){
    lossfct<- "xentropy"
  }
  
  if(isTRUE(sdi)){
    sdi.weights<- dbetabinom.ab(x = seq(0,hyperpars[3]), size = hyperpars[3],
                                shape1 = hyperpars[4], shape2 = hyperpars[5])
    sdi.vec<- SDI(df = inputdf, w = sdi.weights, theta = hyperpars[6:8],
                  rho = hyperpars[9:11], tau = hyperpars[12])
    
    col.selector<- grepl("temperature", colnames(inputdf)) | grepl("humidity", colnames(inputdf))
    df<- cbind(inputdf[,!col.selector],sdi.vec)
    colnames(df)<- c(colnames(inputdf)[!col.selector],"sdi")
  }else{
    df<- inputdf
  }
  
  folds<- rep(seq(1,cv),ceiling(nrow(df)/cv))[1:nrow(df)]
  folds<- sample(folds,length(folds), replace = FALSE)
  
  processing.unit<- "cpu"
  if(isTRUE(gpu)){
    processing.unit<- "gpu"
  }
  
  parameters<- list(objective = est.type, num_trees = no.trees, metric = lossfct,
                    learning_rate = hyperpars[1], num_leaves = hyperpars[2],
                    device_type = processing.unit)
  
  loss<- numeric(cv)
  for(i in seq(1,cv)){
    train<- as.matrix(df[folds!=i,])
    dtrain<- lgb.Dataset(train, label = y[folds!=i])
    test<- as.matrix(df[folds==i,])
    dtest<- lgb.Dataset.create.valid(dtrain, test, label = y[folds==i])
    model<- lgb.train(params = parameters, data = dtrain, nrounds = no.trees,
                      obj = est.type, verbose = -1, record = FALSE,
                      valids = list(test = dtest))
    loss[i]<- model$eval_valid()[[1]]$value
  }

  out<- mean(loss)
  return(out)
}




wrapper_Thom_age<- function(pars){
  wrapper_default(pars, inputdf = rhs_Thom_q1, y = full.df_7$age,
                      est.type  ="quantile", cv = 5, no.trees = 200,
                      sdi = FALSE, gpu = FALSE, seed = 16105)
}

wrapper_Thom_gender<- function(pars){
  wrapper_default(pars, inputdf = rhs_Thom_q1, y = full.df_7$female,
                      est.type  ="binary", cv = 5, no.trees = 200,
                      sdi = FALSE, gpu = FALSE, seed = 16105)
}

wrapper_Thom_phi<- function(pars){
  wrapper_default(pars, inputdf = rhs_Thom_q1, y = full.df_7$PKV,
                      est.type  ="binary", cv = 5, no.trees = 200,
                      sdi = FALSE, gpu = FALSE, seed = 16105)
}

wrapper_Thom_chronic<- function(pars){
  wrapper_default(pars, inputdf = rhs_Thom_q1, y = full.df_7$no_all_chronic_diseases,
                      est.type  ="quantile", cv = 5, no.trees = 200,
                      sdi = FALSE, gpu = FALSE, seed = 16105)
}

wrapper_heatwave_age<- function(pars){
  wrapper_default(pars, inputdf = rhs_heatwave_q1, y = full.df_7$age,
                      est.type  ="quantile", cv = 5, no.trees = 200,
                      sdi = FALSE, gpu = FALSE, seed = 16105)
}

wrapper_heatwave_gender<- function(pars){
  wrapper_default(pars, inputdf = rhs_heatwave_q1, y = full.df_7$female,
                      est.type  ="binary", cv = 5, no.trees = 200,
                      sdi = FALSE, gpu = FALSE, seed = 16105)
}

wrapper_heatwave_phi<- function(pars){
  wrapper_default(pars, inputdf = rhs_heatwave_q1, y = full.df_7$PKV,
                      est.type  ="binary", cv = 5, no.trees = 200,
                      sdi = FALSE, gpu = FALSE, seed = 16105)
}

wrapper_heatwave_chronic<- function(pars){
  wrapper_default(pars, inputdf = rhs_heatwave_q1, y = full.df_7$no_all_chronic_diseases,
                      est.type  ="quantile", cv = 5, no.trees = 200,
                      sdi = FALSE, gpu = FALSE, seed = 16105)
}

wrapper_sdi_age<- function(pars){
  wrapper_default(pars, inputdf = rhs_sdi_q1, y = full.df_7$age,
                      est.type  ="quantile", cv = 5, no.trees = 200,
                      sdi = TRUE, gpu = FALSE, seed = 16105)
}

wrapper_sdi_gender<- function(pars){
  wrapper_default(pars, inputdf = rhs_sdi_q1, y = full.df_7$female,
                      est.type  ="binary", cv = 5, no.trees = 200,
                      sdi = TRUE, gpu = FALSE, seed = 16105)
}

wrapper_sdi_phi<- function(pars){
  wrapper_default(pars, inputdf = rhs_sdi_q1, y = full.df_7$PKV,
                      est.type  ="binary", cv = 5, no.trees = 200,
                      sdi = TRUE, gpu = FALSE, seed = 16105)
}

wrapper_sdi_chronic<- function(pars){
  wrapper_default(pars, inputdf = rhs_sdi_q1, y = full.df_7$no_all_chronic_diseases,
                      est.type  ="quantile", cv = 5, no.trees = 200,
                      sdi = TRUE, gpu = FALSE, seed = 16105)
}
