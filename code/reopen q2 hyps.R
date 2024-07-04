

wrapper_interior.new<- function(sdi = c(6, 1.851, 3.149, 0.044, -.3, .187, -.375, -.048, -.270.389), lr = 0.359, no.leaves = 37, max.depth = 9, 
                            min.data.in.leaf = 811343, feature.fraction = 0.69, cat.l2 = 20.77,
                            extra.trees = TRUE, top.rate = 0.31, other.rate = 0.47,
                            cat.smooth = 36.43, path.smooth = 8.63,
                            inputdf, y, est.type, alpha = 0.5, cv = 5L,
                            no.trees = 100L, no.threads = 10L, 
                            early.stopping = 10L, seed = NA, 
                            error.rate = TRUE){
  # Interior wrapper for cross-validation of the model. Returns the mean of the error estimated for each fold. If cv<=1, instead of cross validation, a model trained on the entire input data is returned (to be precise, a lgbBooster).
  # sdi is FALSE by default. If the suggested discomfort index is to be used, sdi has to be an atomic vector.
  # if sdi != FALSE:
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
                colnames(inputdf)[grep("chronic", colnames(inputdf))],
                "last_visit")
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
  }else if(est.type %in% c("quantile", "cross_entropy")){
    eval.metric<- lossfct
  }else{
    eval.metric<- lossfct
    warning(paste("Neither binary, multiclass, cross-entropy, or quantile loss was selected. Check your est.type!"))
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

# change objective function to cross-entropy?
