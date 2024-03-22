#####default wrapper

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
