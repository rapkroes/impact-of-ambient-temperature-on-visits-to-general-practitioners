#coordinated gradient descent

min.quantile.loss<- function(beta_start,dataframe,y.vec,quantile_value,alpha_value,lambda_value,constraint.matrix, maxiter=100, stepsize=1, tol= 1e-6){
  ticker<- 1
  lfct<- function(beta.vec){
    n<- nrow(dataframe)
    residual<- y-as.matrix(dataframe)%*%beta.vec
    positive<- residual>0
    likelihood<- sum(quantile_value*residual[positive])/n+sum((quantile_value-1)*residual[!positive])/n
    linear.constraint<- 0.5*sum((constraint.matrix%*%beta.vec)^2)
    regularization<- lambda_value*(alpha_value*sum(abs(beta.vec))+(1-alpha_value)*sqrt(sum(beta.vec^2)))
    out<- likelihood + linear.constraint+ regularization
    return(out)
  }
  lgrad<- function(ind,beta.vec){
    dataframe<- as.matrix(dataframe)
    likelihood<- (quantile_value-1+as.numeric(dataframe%*%beta.vec))%*%dataframe[,ind]/nrow(dataframe)
    linear.constraint<- constraint.matrix[,ind]%*% (constraint.matrix%*%beta.vec)
    regularization<- lambda_value*(alpha_value*sign(beta.vec[ind])+(1-alpha_value)*2*beta.vec[ind]/sqrt(sum((beta.vec)^2)))
    out<- as.numeric(likelihood +linear.constraint +regularization)
    return(out)
  } 
  convergence<- FALSE
  beta_new<- beta_start
  beta_sugg<- beta_new
  step.vec<- rep(stepsize,length(beta_start))
  while(ticker<=maxiter && !convergence){
    beta_baseline<- beta_new
    for(i in seq_along(beta_start)){
      beta_sugg[i]<- beta_new[i]-step.vec[i]*lgrad(i, beta_new)
      while(lfct(beta_sugg)>lfct(beta_new)){
        step.vec[i]<- 0.5*step.vec[i]
        beta_sugg[i]<- beta_new[i]-step.vec[i]*lgrad(i, beta_new)
      }
      if(abs(beta_sugg[i])<=tol){
        beta_round<- beta_sugg
        beta_round[i]<- 0
        if(lfct(beta_round)<=lfct(beta_sugg)){
          beta_sugg<- beta_round
        }
      }
      beta_new<- beta_sugg
    }
    ticker<- ticker+1
    convergence<- lfct(beta_baseline)-lfct(beta_new) <=tol
    #rework convergence criteria such that each of these has to converge!
  }
  out.list<- list()
  out.list$converged<- convergence
  out.list$beta<- beta_new
  out.list$loss_old<- lfct(beta_start)
  out.list$loss_new<- lfct(beta_new)
  out.list$iterations<- ticker
  return(out.list)
}


trial_1<- min.quantile.loss(random.beta,x,y,.5,.5,1,A_mat)

library(tictoc)
nmc<- 1000
beta_hat<- matrix(NA,nrow = nmc, ncol = 50)
loss.vec<- numeric(nmc)
iterations.vec<- numeric(nmc)
result.list<- list()
tic()
for(i in 1:nmc){
  im<- min.quantile.loss(rnorm(50),x,y,0.5,0.5,1,A_mat)
  result.list[[i]]<- im
  beta_hat[i,]<- im$beta
  loss.vec[i]<- im$loss_new
  iterations.vec[i]<- im$iterations
}
toc() #5378.08 sec elapsed
#lesson: more start, fewer iterations to avoid being hung in divergence!

q.loss.gradient<- function(beta_no, beta_value,d,y,q,a,l,A){
  #browser()
  d<- as.matrix(d)
  likelihood<- (q-1+as.numeric(d%*%beta_value))%*%d[,beta_no]/nrow(d)
  linear.constraint<- A[,beta_no]%*% (A%*%beta_value)
  regularization<- l*(a*sign(beta_value[beta_no])+(1-a)*2*beta_value[beta_no]/sqrt(sum((beta_value)^2)))
  return(as.numeric(likelihood +linear.constraint +regularization))
}
q.loss.gradient(1,random.beta, x, y, .5,.5,1,A_mat)


A_mat<- cbind(matrix(0,nrow = 15, ncol = p-20),cmat)
set.seed(165)
random.beta<- rnorm(p)



coordinate.descent<- function(beta_start,dataframe,y.vec,quantile_value,alpha_value,lambda_value,constraint.matrix, maxiter=100, stepsize=1, tol= 1e-6){
  ticker<- 1
  lfct<- function(beta.vec){
    n<- nrow(dataframe)
    residual<- y-as.matrix(dataframe)%*%beta.vec
    positive<- residual>0
    likelihood<- sum(quantile_value*residual[positive])/n+sum((quantile_value-1)*residual[!positive])/n
    linear.constraint<- 0.5*sum((constraint.matrix%*%beta.vec)^2)
    regularization<- lambda_value*(alpha_value*sum(abs(beta.vec))+(1-alpha_value)*sqrt(sum(beta.vec^2)))
    out<- likelihood + linear.constraint+ regularization
    return(out)
  }
  lgrad<- function(ind,beta.vec){
    dataframe<- as.matrix(dataframe)
    likelihood<- (quantile_value-1+as.numeric(dataframe%*%beta.vec))%*%dataframe[,ind]/nrow(dataframe)
    linear.constraint<- constraint.matrix[,ind]%*% (constraint.matrix%*%beta.vec)
    regularization<- lambda_value*(alpha_value*sign(beta.vec[ind])+(1-alpha_value)*2*beta.vec[ind]/sqrt(sum((beta.vec)^2)))
    out<- as.numeric(likelihood +linear.constraint +regularization)
    return(out)
  } 
  convergence<- FALSE
  beta_new<- beta_start
  beta_sugg<- beta_new
  step.vec<- rep(stepsize,length(beta_start))
  while(ticker<=maxiter && !convergence){
    beta_baseline<- beta_new
    for(i in seq_along(beta_start)){
      beta_sugg[i]<- beta_new[i]-step.vec[i]*lgrad(i, beta_new)
      while(lfct(beta_sugg)>lfct(beta_new)){
        step.vec[i]<- 0.5*step.vec[i]
        beta_sugg[i]<- beta_new[i]-step.vec[i]*lgrad(i, beta_new)
      }
      if(abs(beta_sugg[i])<=tol){
        beta_round<- beta_sugg
        beta_round[i]<- 0
        if(lfct(beta_round)<=lfct(beta_sugg)){
          beta_sugg<- beta_round
        }
      }
      beta_new<- beta_sugg
    }
    ticker<- ticker+1
    convergence<- lfct(beta_baseline)-lfct(beta_new) <=tol
  }
  out.list<- list()
  out.list$converged<- convergence
  out.list$beta<- beta_new
  out.list$loss_old<- lfct(beta_start)
  out.list$loss_new<- lfct(beta_new)
  out.list$iterations<- ticker
  return(out.list)
}












### new coordinate descent
coordinate.descent<- function(dist.env){
  coordinate.cluster<- makeCluster(get("no.workers", envir = dist.env))
  clusterExport(cl = coordinate.cluster, varlist =  c(loss.function,loss.gradient,beta.list,constraint.matrix,max.iter,step.size,tol), envir = dist.env)
  result.list<- parLapply(cl = coordinate.cluster,seq_along(beta.list), fun = function(z){
    beta_start<- beta.list[[z]]
    ticker<- 1
    convergence<- FALSE
    beta_new<- beta_start
    beta_sugg<- beta_new
    step.vec<- rep(step.size,length(beta_start))
    while(ticker<=maxiter && !convergence){
      beta_baseline<- beta_new
      for(i in seq_along(beta_start)){
        beta_sugg[i]<- beta_new[i]-step.vec[i]*loss.gradient(i, beta_new)
        while(loss.function(beta_sugg)>loss.function(beta_new)){
          step.vec[i]<- 0.5*step.vec[i]
          beta_sugg[i]<- beta_new[i]-step.vec[i]*loss.gradient(i, beta_new)
        }
        if(abs(beta_sugg[i])<=tol){
          beta_round<- beta_sugg
          beta_round[i]<- 0
          if(loss.function(beta_round)<=loss.function(beta_sugg)){
            beta_sugg<- beta_round
          }
        }
        beta_new<- beta_sugg
      }
      ticker<- ticker+1
      convergence<- loss.function(beta_baseline)-loss.function(beta_new) <=tol & 0.5*sum((constraint.matrix%*%beta_new)^2) <=tol
    }
    out.list<- list()
    out.list$converged<- convergence
    out.list$beta<- beta_new
    out.list$loss_old<- loss.function(beta_start)
    out.list$loss_new<- loss.function(beta_new)
    out.list$iterations<- ticker
    return(out.list)
  })
  return(result.list)
}
