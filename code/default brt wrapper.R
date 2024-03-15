#####default wrapper

wrapper.brt_default<- function(hyperpars,inputdf = full.df_7, y = full.df_7$age,
                               est.type  ="quantile", cv = 10, no.trees = 200,
                               sdi = FALSE, gpu = FALSE, seed){
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
  
  fold.list<- lapply(seq(1,cv),FUN = function(k) seq(k,nrow(df), by = cv))
  
  parameters<- list(objective = est.type, num_trees = no.trees,
                    learning_rate = hyperpars[1], num_leaves = hyperpars[2],
                    device_type = processing.unit)
  
  results<- lgb.cv(params = parameters, data = df,nrounds = no.trees,
                   label = y, obj = est.type,verbose = 1, record = FALSE,
                   nfold = cv)
  return(results)
}
