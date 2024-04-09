############# additional notes

###from functions
# add.chronic<- function(diagdf,chronicdf){
#   addage<- as(matrix(0,nrow = nrow(diagdf),ncol = 11), "sparseMatrix")
#   pb<- txtProgressBar(min = 0, max = nrow(chronicdf), initial = 0)
#   for(i in seq(1,nrow(chronicdf))){
#     row.selector<- diagdf$uniPatID==chronicdf[i,1]
#     addage[row.selector,chronicdf[i,2]]<- addage[row.selector,chronicdf[i,2]]+1
#     setTxtProgressBar(pb,i)
#   }
#   addage<- as.data.frame(addage)
#   colnames(addage)<- paste0(rep("chronic_",11),1:11)
#   no_all_chronic_diseases<- rowSums(addage)
#   addage$no_all_chronic_diseases<- no_all_chronic_diseases
#   out<- cbind(diagdf,addage)
#   return(out)
# }
# add.chronic<- function(diagdf,chronicdf){
#   chronicdf<- chronicdf|>
#     group_by(uniPatID, diag_category)|>
#     mutate(counts = n())|>
#     distinct()|>
#     ungroup()
#   addage<- matrix(0,nrow = nrow(diagdf),ncol = 11)
#   pb<- txtProgressBar(min = 0, max = nrow(chronicdf), initial = 0)
#   for(i in seq(1,nrow(chronicdf))){
#     row.selector<- which(diagdf$uniPatID==as.numeric(chronicdf[i,1]))
#     addage[row.selector,as.numeric(chronicdf[i,2])]<- as.numeric(chronicdf[i,3])
#     setTxtProgressBar(pb,i)
#   }
#   addage<- as.data.frame(addage)
#   colnames(addage)<- paste0(rep("chronic_",11),1:11)
#   no_all_chronic_diseases<- rowSums(addage)
#   addage$no_all_chronic_diseases<- no_all_chronic_diseases
#   out<- cbind(diagdf,addage)
#   return(out)
# }
# add.chronic<- function(diagdf,chronicdf){
#   chronicdf<- chronicdf|>
#     group_by(uniPatID, diag_category)|>
#     mutate(counts = n())|>
#     distinct()|>
#     ungroup()
#   addage<- matrix(0,nrow = nrow(diagdf),ncol = 11)
#   pb<- txtProgressBar(min = 0, max = nrow(chronicdf), initial = 0)
#   for(i in seq(1,nrow(chronicdf))){
#     row.selector<- which(diagdf$uniPatID==as.numeric(chronicdf[i,1]))
#     addage[row.selector,as.numeric(chronicdf[i,2])]<- as.numeric(chronicdf[i,3])
#     setTxtProgressBar(pb,i)
#   }
#   addage<- as.data.frame(addage)
#   colnames(addage)<- paste0(rep("chronic_",11),1:11)
#   no_all_chronic_diseases<- rowSums(addage)
#   addage$no_all_chronic_diseases<- no_all_chronic_diseases
#   out<- cbind(diagdf,addage)
#   return(out)
# }

elastic.net<- function(inputdf, y, standardize.y=FALSE, spline.pos=NULL, spline.knots=NULL, sel.loss.function, sel.quantile=NULL, alpha, lambda, no.starts=1, no.workers=2, max.iter=1000, step.size=1, lc.rho=c(1,1), tol=1e-6){
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
      linear.constraint<- lc.rho%*%c(ticker,ticker^2) *sqrt(0.5*sum((constraint.matrix%*%beta.vec)^2))/nrow(constraint.matrix)
      regularization<- lambda*(alpha*sum(abs(beta.vec))+(1-alpha)*sqrt(sum(beta.vec^2)))
      return(likelihood + linear.constraint+ regularization)
    }
    loss.gradient<- function(ind,beta.vec){
      dataframe<- as.matrix(df)
      likelihood<- (sel.quantile-1+as.numeric(dataframe%*%beta.vec))%*%dataframe[,ind]/nrow(dataframe)
      linear.constraint<- lc.rho%*%c(ticker,ticker^2) *constraint.matrix[,ind]%*% (constraint.matrix%*%beta.vec) /(tol+nrow(constraint.matrix)*sqrt(2*sum((constraint.matrix%*%beta.vec)^2)))
      regularization<- lambda*(alpha*sign(beta.vec[ind])+(1-alpha)*2*beta.vec[ind]/sqrt(sum((beta.vec)^2)))
      out<- as.numeric(likelihood +linear.constraint +regularization)
      return(out)
    }
  }else if(sel.loss.function=="proportion"){
    loss.function<- function(beta.vec){
      fitted<- as.numeric(as.matrix(df)%*%beta.vec)
      likelihood<- ((1-y)%*%fitted+sum(log(1+exp(-fitted))))/nrow(df)
      linear.constraint<- lc.rho%*%c(ticker,ticker^2) *sqrt(0.5*sum((constraint.matrix%*%beta.vec)^2))/nrow(constraint.matrix)
      regularization<- lambda*(alpha*sum(abs(beta.vec[1:no.not.spline.params]))+(1-alpha)*sqrt(sum(beta.vec[1:no.not.spline.params]^2)))
      return(likelihood + linear.constraint+ regularization)
    }
    loss.gradient<- function(ind, beta.vec){
      likelihood<- as.matrix(df)[,ind]%*%((1+exp(-as.matrix(df)%*%beta.vec))^(-1)-y) /nrow(df)
      linear.constraint<- lc.rho%*%c(ticker,ticker^2) *constraint.matrix[,ind]%*% (constraint.matrix%*%beta.vec) /(tol+nrow(constraint.matrix)*sqrt(2*sum((constraint.matrix%*%beta.vec)^2)))
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
    beta_start<- rq(formula = form, tau = sel.quantile, data = base.data, method = "fn")$coefficients
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

elastic.net_speed<- function(inputdf, y, standardize.y=FALSE, spline.pos=NULL, spline.knots=NULL, sel.loss.function, sel.quantile=NULL, alpha, lambda, no.starts=1, no.workers=2, max.iter=1000, step.size=1, lc.rho, tol=1e-6){
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
      linear.constraint<- lc.rho *sqrt(0.5*sum((constraint.matrix%*%beta.vec)^2))/nrow(constraint.matrix)
      regularization<- lambda*(alpha*sum(abs(beta.vec))+(1-alpha)*sqrt(sum(beta.vec^2)))
      return(likelihood + linear.constraint+ regularization)
    }
    loss.gradient<- function(ind,beta.vec){
      dataframe<- as.matrix(df)
      likelihood<- (sel.quantile-1+as.numeric(dataframe%*%beta.vec))%*%dataframe[,ind]/nrow(dataframe)
      linear.constraint<- lc.rho *constraint.matrix[,ind]%*% (constraint.matrix%*%beta.vec) /(tol+nrow(constraint.matrix)*sqrt(2*sum((constraint.matrix%*%beta.vec)^2)))
      regularization<- lambda*(alpha*sign(beta.vec[ind])+(1-alpha)*2*beta.vec[ind]/sqrt(sum((beta.vec)^2)))
      out<- as.numeric(likelihood +linear.constraint +regularization)
      return(out)
    }
  }else if(sel.loss.function=="proportion"){
    loss.function<- function(beta.vec){
      fitted<- as.numeric(as.matrix(df)%*%beta.vec)
      likelihood<- ((1-y)%*%fitted+sum(log(1+exp(-fitted))))/nrow(df)
      linear.constraint<- lc.rho *sqrt(0.5*sum((constraint.matrix%*%beta.vec)^2))/nrow(constraint.matrix)
      regularization<- lambda*(alpha*sum(abs(beta.vec[1:no.not.spline.params]))+(1-alpha)*sqrt(sum(beta.vec[1:no.not.spline.params]^2)))
      return(likelihood + linear.constraint+ regularization)
    }
    loss.gradient<- function(ind, beta.vec){
      likelihood<- as.matrix(df)[,ind]%*%((1+exp(-as.matrix(df)%*%beta.vec))^(-1)-y) /nrow(df)
      linear.constraint<- lc.rho *constraint.matrix[,ind]%*% (constraint.matrix%*%beta.vec) /(tol+nrow(constraint.matrix)*sqrt(2*sum((constraint.matrix%*%beta.vec)^2)))
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
    beta_start<- rq(formula = form, tau = sel.quantile, data = base.data, method = "fn")$coefficients
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
    gradient.fun<- function(beta) sapply(seq_along(beta), function(index){loss.gradient(index,beta)})
    result<- optim(par = beta_start, fn = loss.function, gr = gradient.fun, method = "BFGS", control = list(maxit=max.iter, abstol= tol))
    
    out.list<- list()
    out.list$converged<- result$convergence
    out.list$beta<- result$par
    out.list$loss_old<- loss.function(beta_start)
    out.list$loss_new<- loss.function(out.list$beta)
    return(out.list)
  }
  
  if(no.starts>1){
    coordinate.cluster<- makeCluster(no.workers)
    clusterExport(cl = coordinate.cluster, varlist =  c("beta.list", "loss.function", "loss.gradient", "df", "y", "constraint.matrix", "max.iter", "step.size", "tol"), envir = environment())
    results<- parLapply(cl = coordinate.cluster,seq_along(beta.list), fun = z_fun)
  }else{
    results<- lapply(1,z_fun)
  }
  
  out<- list()
  out$converged<- numeric(no.starts)
  out$loss_old<- numeric(no.starts)
  out$loss_new<- numeric(no.starts)
  out$beta<- matrix(NA,ncol = no.params, nrow = no.starts)
  
  for(i in seq(1,no.starts)){
    out$converged[i]<- results[[i]]$converged
    out$beta[i,]<- results[[i]]$beta
    out$loss_old[i]<- results[[i]]$loss_old
    out$loss_new[i]<- results[[i]]$loss_new
    
  }
  colnames(out$beta)<- colnames(df)
  
  for(i in seq_along(cut.list)){
    old.names<- names(out)
    out$im<- cut.list[[i]]
    names(out)<- c(old.names,paste0("cuts_",i))
  }
  
  return(out)
}

tdi.effect<- function(x,beta,cuts){
  #estimates the linear effect of Thom's discomfort index (thus not the partial effect, as it still needs to be transformed)
  #x is a vector of points for which the effect is to be estimated
  #beta is the beta vector, provided by elastic.net and elastic.net_speed
  #cuts is the cuts vector, provided by elastic.net and elastic.net_speed
  params<- as.vector(beta[grepl("thoms_discomfort_index",colnames(beta))])
  names(params)<- colnames(beta)[grepl("thoms_discomfort_index",colnames(beta))]
  for(i in seq_along(params)){
    varname<- substr(names(params)[i],nchar(names(params)[i])-2,nchar(names(params)[i]))
    assign(varname,params[i])
  }
  out<- numeric(length(x))
  cuts_abridged<- cuts[2:(length(cuts)-1)]
  for(i in seq_along(out)){
    if(x[i]<min(cuts_abridged)){
      out[i]<- a_1*x[i]+b_1
    }else if(x[i]>max(cuts_abridged)){
      no<- length(cuts)-1
      out[i]<- get(paste0("a_",no))*x[i]+get(paste0("b_",no))
    }else{
      no<- 1+sum(x[i]>=cuts_abridged)
      out[i]<- get(paste0("a_",no))*x[i]^3+get(paste0("b_",no))*x[i]^2+get(paste0("c_",no))*x[i]+get(paste0("d_",no))
    }
  }
  return(out)
}

hw.effect<- function(x,beta,cuts){
  #estimates the linear effect of heat wave length (thus not the partial effect, as it still needs to be transformed). Similar to tdi.effect
  #x is a vector of points for which the effect is to be estimated
  #beta is the beta vector, provided by elastic.net and elastic.net_speed
  #cuts is the cuts vector, provided by elastic.net and elastic.net_speed
  params<- as.vector(beta[grepl("length_heat_wave",colnames(beta))])
  names(params)<- colnames(beta)[grepl("length_heatwave",colnames(beta))]
  for(i in seq_along(params)){
    varname<- substr(names(params)[i],nchar(names(params)[i])-2,nchar(names(params)[i]))
    assign(varname,params[i])
  }
  out<- numeric(length(x))
  cuts_abridged<- cuts[2:(length(cuts)-1)]
  for(i in seq_along(out)){
    if(x[i]<min(cuts_abridged)){
      out[i]<- a_1*x[i]+b_1
    }else if(x[i]>max(cuts_abridged)){
      no<- length(cuts)-1
      out[i]<- get(paste0("a_",no))*x[i]+get(paste0("b_",no))
    }else{
      no<- 1+sum(x[i]>=cuts_abridged)
      out[i]<- get(paste0("a_",no))*x[i]^3+get(paste0("b_",no))*x[i]^2+get(paste0("c_",no))*x[i]+get(paste0("d_",no))
    }
  }
  return(out)
}

tuning.Thom_q1_age<- function(pars){
  #tuning wrapper for research question 1, TDI, age.
  #pars is a triple of real numbers (as vector). They stand for lambda, alpha, and rho from the linear constraints. Each parameter is transformed within the function such that they all fit their respective domain.
  lambda_value<- exp(pars[1])
  alpha_value<- 1/(1+exp(pars[2]))
  lc<- exp(pars[3])
  
  df<- rhs_Thom_q1[sample(1:nrow(rhs_Thom_q1),nrow(rhs_Thom_q1)),]|>
    arrange(TG_DateNum)|>
    select(-TG_DateNum)
  fold.vec<- rep(1:n.folds, ceiling(nrow(df)/n.folds))[1:n.folds]
  loss.vec<- numeric(n.folds)
  
  quantile<- 0.5
  knots<- 4
  
  for(i in seq(1,n.folds)){
    im<- elastic.net_speed(inputdf = df[fold.vec!=i,], y= full.df_7$age[fold.vec!=i], spline.pos = 1, spline.knots = knots, sel.loss.function = "quantile", sel.quantile = quantile, alpha = alpha_value, lambda = lambda_value, tol = 1e-6, max.iter = 50, lc.rho = lc)
    beta_hat<- im$beta
    
    loss.function<- function(beta.vec){
      n<- nrow(df[fold.vec==i,])
      residual<- full.df_7$age[fold.vec==i]-as.matrix(df[fold.vec==i,-1])%*%beta.vec[seq(1,ncol(df)-1)]-tdi.effect(df$thoms_discomfort_index[fold.vec==i],beta_hat,im$cuts)
      positive<- residual>0
      likelihood<- (sum(quantile*residual[positive])+sum((quantile-1)*residual[!positive]))/n
      return(likelihood)
    }
    loss.vec[i]<- loss.function(as.vector(beta_hat))
  }
  
  return(sum(loss.vec))
}

tuning.Thom_q1_gender<- function(pars){
  #tuning wrapper for research question 1, TDI, age.
  #pars is a triple of real numbers (as vector). They stand for lambda, alpha, and rho from the linear constraints. Each parameter is transformed within the function such that they all fit their respective domain.
  lambda_value<- exp(pars[1])
  alpha_value<- 1/(1+exp(pars[2]))
  lc<- exp(pars[3])
  
  df<- rhs_Thom_q1[sample(1:nrow(rhs_Thom_q1),nrow(rhs_Thom_q1)),]|>
    arrange(TG_DateNum)|>
    select(-TG_DateNum)
  fold.vec<- rep(1:n.folds, ceiling(nrow(df)/n.folds))[1:n.folds]
  loss.vec<- numeric(n.folds)
  
  knots<- 4
  
  for(i in seq(1,n.folds)){
    im<- elastic.net_speed(inputdf = df[fold.vec!=i,], y= full.df_7$female[fold.vec!=i], spline.pos = 1, spline.knots = knots, sel.loss.function = "proportion", alpha = alpha_value, lambda = lambda_value, tol = 1e-6, max.iter = 50, lc.rho = lc)
    beta_hat<- im$beta
    
    loss.function<- function(beta.vec){
      n<- nrow(df[fold.vec==i,])
      residual<- full.df_7$age[fold.vec==i]-as.matrix(df[fold.vec==i,-1])%*%beta.vec[seq(1,ncol(df)-1)]-tdi.effect(df$thoms_discomfort_index[fold.vec==i],beta_hat,im$cuts)
      positive<- residual>0
      likelihood<- (sum(quantile*residual[positive])+sum((quantile-1)*residual[!positive]))/n
      return(likelihood)
    }
    loss.vec[i]<- loss.function(as.vector(beta_hat))
  }
  
  return(sum(loss.vec))
}

tuning.Thom_q1_phi<- function(pars){
  #tuning wrapper for research question 1, TDI, age.
  #pars is a triple of real numbers (as vector). They stand for lambda, alpha, and rho from the linear constraints. Each parameter is transformed within the function such that they all fit their respective domain.
  lambda_value<- exp(pars[1])
  alpha_value<- 1/(1+exp(pars[2]))
  lc<- exp(pars[3])
  
  df<- rhs_Thom_q1[sample(1:nrow(rhs_Thom_q1),nrow(rhs_Thom_q1)),]|>
    arrange(TG_DateNum)|>
    select(-TG_DateNum)
  fold.vec<- rep(1:n.folds, ceiling(nrow(df)/n.folds))[1:n.folds]
  loss.vec<- numeric(n.folds)
  
  knots<- 4
  
  for(i in seq(1,n.folds)){
    im<- elastic.net_speed(inputdf = df[fold.vec!=i,], y= full.df_7$PKV[fold.vec!=i], spline.pos = 1, spline.knots = knots, sel.loss.function = "proportion", alpha = alpha_value, lambda = lambda_value, tol = 1e-6, max.iter = 50, lc.rho = lc)
    beta_hat<- im$beta
    
    loss.function<- function(beta.vec){
      n<- nrow(df[fold.vec==i,])
      residual<- full.df_7$age[fold.vec==i]-as.matrix(df[fold.vec==i,-1])%*%beta.vec[seq(1,ncol(df)-1)]-tdi.effect(df$thoms_discomfort_index[fold.vec==i],beta_hat,im$cuts)
      positive<- residual>0
      likelihood<- (sum(quantile*residual[positive])+sum((quantile-1)*residual[!positive]))/n
      return(likelihood)
    }
    loss.vec[i]<- loss.function(as.vector(beta_hat))
  }
  
  return(sum(loss.vec))
}

tuning.Thom_q1_chronic<- function(pars){
  #tuning wrapper for research question 1, TDI, age.
  #pars is a triple of real numbers (as vector). They stand for lambda, alpha, and rho from the linear constraints. Each parameter is transformed within the function such that they all fit their respective domain.
  lambda_value<- exp(pars[1])
  alpha_value<- 1/(1+exp(pars[2]))
  lc<- exp(pars[3])
  
  df<- rhs_Thom_q1[sample(1:nrow(rhs_Thom_q1),nrow(rhs_Thom_q1)),]|>
    arrange(TG_DateNum)|>
    select(-TG_DateNum)
  fold.vec<- rep(1:n.folds, ceiling(nrow(df)/n.folds))[1:n.folds]
  loss.vec<- numeric(n.folds)
  
  knots<- 4
  
  for(i in seq(1,n.folds)){
    im<- elastic.net_speed(inputdf = df[fold.vec!=i,], y= full.df_7$no_all_chronic_diseases[fold.vec!=i], spline.pos = 1, spline.knots = knots, sel.loss.function = "proportion", alpha = alpha_value, lambda = lambda_value, tol = 1e-6, max.iter = 50, lc.rho = lc)
    beta_hat<- im$beta
    
    loss.function<- function(beta.vec){
      n<- nrow(df[fold.vec==i,])
      residual<- full.df_7$age[fold.vec==i]-as.matrix(df[fold.vec==i,-1])%*%beta.vec[seq(1,ncol(df)-1)]-tdi.effect(df$thoms_discomfort_index[fold.vec==i],beta_hat,im$cuts)
      positive<- residual>0
      likelihood<- (sum(quantile*residual[positive])+sum((quantile-1)*residual[!positive]))/n
      return(likelihood)
    }
    loss.vec[i]<- loss.function(as.vector(beta_hat))
  }
  
  return(sum(loss.vec))
}

tuning.hw_q1_age<- function(pars){
  #tuning wrapper for research question 1, heat wave, age.
  #pars is a triple of real numbers (as vector). They stand for lambda, alpha, and rho from the linear constraints. Each parameter is transformed within the function such that they all fit their respective domain.
  lambda_value<- exp(pars[1])
  alpha_value<- 1/(1+exp(pars[2]))
  lc<- exp(pars[3])
  
  df<- rhs_heatwave_q1[sample(1:nrow(rhs_heatwave_q1),nrow(rhs_heatwave_q1)),]|>
    arrange(TG_DateNum)|>
    select(-TG_DateNum)
  fold.vec<- rep(1:n.folds, ceiling(nrow(df)/n.folds))[1:n.folds]
  loss.vec<- numeric(n.folds)
  
  quantile<- 0.5
  knots<- 4
  
  for(i in seq(1,n.folds)){
    im<- elastic.net_speed(inputdf = df[fold.vec!=i,], y= full.df_7$age[fold.vec!=i], spline.pos = 1, spline.knots = knots, sel.loss.function = "quantile", sel.quantile = quantile, alpha = alpha_value, lambda = lambda_value, tol = 1e-6, max.iter = 50, lc.rho = lc)
    beta_hat<- im$beta
    
    loss.function<- function(beta.vec){
      n<- nrow(df[fold.vec==i,])
      residual<- full.df_7$age[fold.vec==i]-as.matrix(df[fold.vec==i,-1])%*%beta.vec[seq(1,ncol(df)-1)]-hw.effect(df$length_heatwave[fold.vec==i],beta_hat,im$cuts)
      positive<- residual>0
      likelihood<- (sum(quantile*residual[positive])+sum((quantile-1)*residual[!positive]))/n
      return(likelihood)
    }
    loss.vec[i]<- loss.function(as.vector(beta_hat))
  }
  
  return(sum(loss.vec))
}

tuning.hw_q1_gender<- function(pars){
  #tuning wrapper for research question 1, heat wave, gender.
  #pars is a triple of real numbers (as vector). They stand for lambda, alpha, and rho from the linear constraints. Each parameter is transformed within the function such that they all fit their respective domain.
  lambda_value<- exp(pars[1])
  alpha_value<- 1/(1+exp(pars[2]))
  lc<- exp(pars[3])
  
  df<- rhs_heatwave_q1[sample(1:nrow(rhs_heatwave_q1),nrow(rhs_heatwave_q1)),]|>
    arrange(TG_DateNum)|>
    select(-TG_DateNum)
  fold.vec<- rep(1:n.folds, ceiling(nrow(df)/n.folds))[1:n.folds]
  loss.vec<- numeric(n.folds)
  
  knots<- 4
  
  for(i in seq(1,n.folds)){
    im<- elastic.net_speed(inputdf = df[fold.vec!=i,], y= full.df_7$female[fold.vec!=i], spline.pos = 1, spline.knots = knots, sel.loss.function = "proportion", alpha = alpha_value, lambda = lambda_value, tol = 1e-6, max.iter = 50, lc.rho = lc)
    beta_hat<- im$beta
    
    loss.function<- function(beta.vec){
      n<- nrow(df[fold.vec==i,])
      residual<- full.df_7$age[fold.vec==i]-as.matrix(df[fold.vec==i,-1])%*%beta.vec[seq(1,ncol(df)-1)]-hw.effect(df$length_heatwave[fold.vec==i],beta_hat,im$cuts)
      positive<- residual>0
      likelihood<- (sum(quantile*residual[positive])+sum((quantile-1)*residual[!positive]))/n
      return(likelihood)
    }
    loss.vec[i]<- loss.function(as.vector(beta_hat))
  }
  
  return(sum(loss.vec))
}

tuning.hw_q1_phi<- function(pars){
  #tuning wrapper for research question 1, heat wave, gender.
  #pars is a triple of real numbers (as vector). They stand for lambda, alpha, and rho from the linear constraints. Each parameter is transformed within the function such that they all fit their respective domain.
  lambda_value<- exp(pars[1])
  alpha_value<- 1/(1+exp(pars[2]))
  lc<- exp(pars[3])
  
  df<- rhs_heatwave_q1[sample(1:nrow(rhs_heatwave_q1),nrow(rhs_heatwave_q1)),]|>
    arrange(TG_DateNum)|>
    select(-TG_DateNum)
  fold.vec<- rep(1:n.folds, ceiling(nrow(df)/n.folds))[1:n.folds]
  loss.vec<- numeric(n.folds)
  
  knots<- 4
  
  for(i in seq(1,n.folds)){
    im<- elastic.net_speed(inputdf = df[fold.vec!=i,], y= full.df_7$PKV[fold.vec!=i], spline.pos = 1, spline.knots = knots, sel.loss.function = "proportion", alpha = alpha_value, lambda = lambda_value, tol = 1e-6, max.iter = 50, lc.rho = lc)
    beta_hat<- im$beta
    
    loss.function<- function(beta.vec){
      n<- nrow(df[fold.vec==i,])
      residual<- full.df_7$age[fold.vec==i]-as.matrix(df[fold.vec==i,-1])%*%beta.vec[seq(1,ncol(df)-1)]-hw.effect(df$length_heatwave[fold.vec==i],beta_hat,im$cuts)
      positive<- residual>0
      likelihood<- (sum(quantile*residual[positive])+sum((quantile-1)*residual[!positive]))/n
      return(likelihood)
    }
    loss.vec[i]<- loss.function(as.vector(beta_hat))
  }
  
  return(sum(loss.vec))
}

tuning.hw_q1_chronic<- function(pars){
  #tuning wrapper for research question 1, heat wave, gender.
  #pars is a triple of real numbers (as vector). They stand for lambda, alpha, and rho from the linear constraints. Each parameter is transformed within the function such that they all fit their respective domain.
  lambda_value<- exp(pars[1])
  alpha_value<- 1/(1+exp(pars[2]))
  lc<- exp(pars[3])
  
  df<- rhs_heatwave_q1[sample(1:nrow(rhs_heatwave_q1),nrow(rhs_heatwave_q1)),]|>
    arrange(TG_DateNum)|>
    select(-TG_DateNum)
  fold.vec<- rep(1:n.folds, ceiling(nrow(df)/n.folds))[1:n.folds]
  loss.vec<- numeric(n.folds)
  
  knots<- 4
  
  for(i in seq(1,n.folds)){
    im<- elastic.net_speed(inputdf = df[fold.vec!=i,], y= full.df_7$no_all_chronic_diseases[fold.vec!=i], spline.pos = 1, spline.knots = knots, sel.loss.function = "proportion", alpha = alpha_value, lambda = lambda_value, tol = 1e-6, max.iter = 50, lc.rho = lc)
    beta_hat<- im$beta
    
    loss.function<- function(beta.vec){
      n<- nrow(df[fold.vec==i,])
      residual<- full.df_7$age[fold.vec==i]-as.matrix(df[fold.vec==i,-1])%*%beta.vec[seq(1,ncol(df)-1)]-hw.effect(df$length_heatwave[fold.vec==i],beta_hat,im$cuts)
      positive<- residual>0
      likelihood<- (sum(quantile*residual[positive])+sum((quantile-1)*residual[!positive]))/n
      return(likelihood)
    }
    loss.vec[i]<- loss.function(as.vector(beta_hat))
  }
  
  return(sum(loss.vec))
}

###holdout selection (for hyperparameter tuning)


###create right hand side input matrices. Some dummies are dropped to avoid multicollinearity.
rhs_Thom_q1<- as.data.frame(cbind(full.df_7$TG_DateNum,
                                  full.df_7$thoms_discomfort_index,
                                  model.matrix(~as.factor(full.df_7$PraxisID)-1)[,-1],
                                  model.matrix(~as.factor(full.df_7$dow)-1)[,-1],
                                  full.df_7$public_holiday,
                                  full.df_7$school_holiday,
                                  model.matrix(~as.factor(full.df_7$week_of_month))[,-1],
                                  model.matrix(~as.factor(full.df_7$month)-1)[,-1],
                                  model.matrix(~as.factor(full.df_7$year)-1)[,-1],
                                  full.df_7$daylight_hours,
                                  full.df_7$covid_7_day_incidence
))
rhs_Thom_q1<- as.data.frame(cbind(full.df_7$TG_DateNum,
                                  full.df_7$thoms_discomfort_index,
                                  as.factor(full.df_7$PraxisID),
                                  as.factor(full.df_7$dow),
                                  full.df_7$public_holiday,
                                  full.df_7$school_holiday,
                                  as.factor(full.df_7$week_of_month),
                                  as.factor(full.df_7$month),
                                  as.factor(full.df_7$year),
                                  full.df_7$daylight_hours,
                                  full.df_7$covid_7_day_incidence
))
colnames(rhs_Thom_q1)<-c("TG_DateNum",
                         "thoms_discomfort_index",
                         paste0("praxis_", levels(as.factor(full.df_6$PraxisID)))[-1],
                         paste0("dow_", levels(as.factor(full.df_6$dow)))[-1],
                         "public_holiday",
                         "school_holiday",
                         paste0("week_", levels(as.factor(full.df_6$week_of_month)))[-1],
                         paste0("month_", levels(as.factor(full.df_6$month)))[-1],
                         paste0("year_", levels(as.factor(full.df_6$year)))[-1],
                         "daylight_hours",
                         "covid_7_day_incidence"
)
rhs_heatwave_q1<- as.data.frame(cbind(full.df_7$TG_DateNum,
                                      full.df_7$length_heatwave,
                                      model.matrix(~as.factor(full.df_7$PraxisID)-1)[,-1],
                                      model.matrix(~as.factor(full.df_7$dow)-1)[,-1],
                                      full.df_7$public_holiday,
                                      full.df_7$school_holiday,
                                      model.matrix(~as.factor(full.df_7$week_of_month))[,-1],
                                      model.matrix(~as.factor(full.df_7$month)-1)[,-1],
                                      model.matrix(~as.factor(full.df_7$year)-1)[,-1],
                                      full.df_7$daylight_hours,
                                      full.df_7$covid_7_day_incidence
))

colnames(rhs_heatwave_q1)<-c("TG_DateNum",
                             "length_heatwave",
                             paste0("praxis_", levels(as.factor(full.df_6$PraxisID)))[-1],
                             paste0("dow_", levels(as.factor(full.df_6$dow)))[-1],
                             "public_holiday",
                             "school_holiday",
                             paste0("week_", levels(as.factor(full.df_6$week_of_month)))[-1],
                             paste0("month_", levels(as.factor(full.df_6$month)))[-1],
                             paste0("year_", levels(as.factor(full.df_6$year)))[-1],
                             "daylight_hours",
                             "covid_7_day_incidence"
)

rhs_sdi_q1<- as.data.frame(cbind(full.df_7$TG_DateNum,
                                 model.matrix(~as.factor(full.df_7$PraxisID)-1)[,-1],
                                 model.matrix(~as.factor(full.df_7$dow)-1)[,-1],
                                 full.df_7$public_holiday,
                                 full.df_7$school_holiday,
                                 model.matrix(~as.factor(full.df_7$week_of_month))[,-1],
                                 model.matrix(~as.factor(full.df_7$month)-1)[,-1],
                                 model.matrix(~as.factor(full.df_7$year)-1)[,-1],
                                 full.df_7$daylight_hours,
                                 full.df_7$covid_7_day_incidence
))

colnames(rhs_sdi_q1)<-c("TG_DateNum",
                        paste0("praxis_", levels(as.factor(full.df_6$PraxisID)))[-1],
                        paste0("dow_", levels(as.factor(full.df_6$dow)))[-1],
                        "public_holiday",
                        "school_holiday",
                        paste0("week_", levels(as.factor(full.df_6$week_of_month)))[-1],
                        paste0("month_", levels(as.factor(full.df_6$month)))[-1],
                        paste0("year_", levels(as.factor(full.df_6$year)))[-1],
                        "daylight_hours",
                        "covid_7_day_incidence"
)

col.selector_sdi<- grepl("temperature",colnames(full.df_7)) | grepl("humidity",colnames(full.df_7))
addage_sdi_q1<- full.df_7[,col.selector_sdi]
colnames(addage_sdi_q1)<- colnames(full.df_7)[col.selector_sdi]
rhs_sdi_q1<- cbind(rhs_sdi_q1, addage_sdi_q1)

#########################
im_1<- wrapper.brt_default(c(.1,10L),rhs_Thom_q1,full.df_7$age)
im_2<- wrapper.brt_Thom_age(c(.1,10L))
im_3<- wrapper.brt_Thom_gender(c(.1,2L))
im_3
tic()
wrapper.brt_Thom_phi(c(.1,10L))
toc() #0.29 sec
wrapper.brt_Thom_chronic(c(.1,2L))

library(tictoc)
tic()
another.trial<- optim(par = c(log(9),8),
                      fn = function(pars){
                        pars[1]<- 2/(1+exp(pars[1]))
                        pars[2]<- 2+round(max(c(0,pars[2])))
                        wrapper_Thom_age(pars)
                      },
                      method = "Nelder-Mead",
                      control = list(maxit = 20))
toc()

#cv test
folds<- 2:20
losses<- numeric(length(folds))
for(i in seq_along(folds)){
  losses[i]<- wrapper_default(c(0.1,31), cv = folds[i], seed = 51651)
}
plot(losses~folds)
st.losses<- losses/folds
plot(st.losses~folds, ylim = c(0,0.5))
abline(h=mean(losses/folds))


library(smoof)
library(mlrMBO)

q1.Thom.age<- makeSingleObjectiveFunction(name = "Q1 Thom Age",
                                          fn = function(lr,nl) wrapper.brt_default(hyperpars = c(lr,nl)),
                                          par.set = makeParamSet(
                                            makeNumericParam("lr", lower = 0, upper = 1),
                                            makeIntegerParam("nl", lower = 2, upper = 30)),
                                          noisy = TRUE,
                                          minimize = TRUE
)
test.control<- makeMBOControl()
test.control<- setMBOControlTermination(control = test.control, iters = 20,
                                        time.budget = 180)
test.mbo<- mbo(fun = q1.Thom.age, control = test.control)

library(ParBayesianOptimization)
imb_1<- bayesOpt(FUN = wrapper.brt_default, bounds = list(lr = c(0,1),nl = c(2L,10L)), initPoints = 4)

lr<- c(seq(0.1,1, by=.1),2:5)
tic()
im_4<- lapply(lr, function(k) wrapper.brt_Thom_gender(c(k,100L)))
toc() #1109.93 sec elapsed
im_5<- numeric(length(lr))
for(i in seq_along(lr)){
  im_5[i]<- im_4[[i]]
}

# hyperparameter optimization
n.folds<- 5
tuning.its<- 3
#5 folds, 3 its: approx. 4 min per optimization

set.seed(840606)
hyper.pars_Thom_q1_age<- optim(par = c(0,log(9),log(10)), fn = tuning.Thom_q1_age, method = "SANN", control = list(maxit=tuning.its))
hyper.pars_Thom_q1_gender<- optim(par = c(0,log(9),log(10)), fn = tuning.Thom_q1_gender, method = "SANN", control = list(maxit=tuning.its))
hyper.pars_Thom_q1_phi<- optim(par = c(0,log(9),log(10)), fn = tuning.Thom_q1_phi, method = "SANN", control = list(maxit=tuning.its))
hyper.pars_Thom_q1_chronic<- optim(par = c(0,log(9),log(10)), fn = tuning.Thom_q1_chronic, method = "SANN", control = list(maxit=tuning.its))
save(hyper.pars_Thom_q1_age,hyper.pars_Thom_q1_gender,hyper.pars_Thom_q1_phi,hyper.pars_Thom_q1_chronic,file = "hyperpars_Thom_q1.RData")

set.seed(840606)
hyper.pars_hw_q1_age<- optim(par = c(0,log(9),log(10)), fn = tuning.hw_q1_age, method = "SANN", control = list(maxit=tuning.its))
hyper.pars_hw_q1_gender<- optim(par = c(0,log(9),log(10)), fn = tuning.hw_q1_gender, method = "SANN", control = list(maxit=tuning.its))
hyper.pars_hw_q1_phi<- optim(par = c(0,log(9),log(10)), fn = tuning.hw_q1_phi, method = "SANN", control = list(maxit=tuning.its))
hyper.pars_hw_q1_chronic<- optim(par = c(0,log(9),log(10)), fn = tuning.hw_q1_chronic, method = "SANN", control = list(maxit=tuning.its))

