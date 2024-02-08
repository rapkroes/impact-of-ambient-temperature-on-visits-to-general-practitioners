elastic.net.function<- function(inputdf, y, sel.loss.function, sel.quantile, spline.pos=NA, spline.knots, alpha,lambda, no.starts, lc.penalty = 100, seed){
  #linear regression with elastic net regularisation, written by myself.
  #inputdf= data frame of inputs
  #y= response variable
  #loss.function is the loss function to be used. Either "quantile", "proportion", or "cross-entropy" may be chosen.
  #spline.pos gives the numeric position which columns are to be fitted with a spline
  #spline.knots gives the number of knots for each of the splines
  #alpha shifts the weight between l1 and l2 penalty
  #lambda is the regularization penalty
  #no.starts is the number of different starting points in the optimisation process
  
  set.seed(seed)
  
  #standardize data
  mean.vec<- colMeans(inputdf)
  var.vec<- apply(inputdf,2,var)
  df<- (inputdf-matrix(mean.vec,nrow = nrow(inputdf), ncol = ncol(inputdf), byrow = TRUE))/matrix(sqrt(var.vec),nrow = nrow(inputdf), ncol = ncol(inputdf), byrow = TRUE)
  colnames(df)<- colnames(inputdf)
  
  #create new spline data and contraints
  spline.list<- list()
  constraint.list<- list()
  constraint.level.list<- list()
  if(!any(is.na(spline.pos))){
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
  
  #browser()
  #generate loss function
  if(sel.loss.function=="quantile"){
    loss.function<- function(beta){
      quantile.loss.fct(beta,df,y,sel.quantile,alpha,lambda)+lc.penalty*sum((complete.constraint.matrix%*%beta)^2)
    }
  }else if(sel.loss.function=="proportion"){
    loss.function<- function(beta){
      proportion.loss.fct(beta,df,y,alpha,lambda)+lc.penalty*sum((complete.constraint.matrix%*%beta)^2)
    }
  }else if(sel.loss.function=="cross-entropy"){
    loss.function<- function(beta){
      cross.entropy.loss.fct(beta,df,y,alpha,lambda)+lc.penalty*sum((complete.constraint.matrix%*%matrix(beta,nrow = ncol(d)))^2)
    }
  }
  
  #create complete linear constraint matrix
  complete.constraint.matrix<- matrix(0,ncol = no.not.spline.params+sum(spline.knots)*4, nrow = sum(spline.knots)*3)
  start.col<- no.not.spline.params+1
  start.row<- 1
  for(i in seq_along(constraint.list)){
    n<- nrow(constraint.list[[i]])
    p<- ncol(constraint.list[[i]])
    complete.constraint.matrix[seq(start.row,start.row+n-1),seq(start.col,start.col+p-1)]<- constraint.list[[i]]
    start.row<- start.row+n
    start.col<- start.col+p
  }

  browser()
  result.list<- list()
  if(any(sel.loss.function %in% c("quantile","proportion"))){
    for (i in seq(1,no.starts)) {
      result.list[[i]]<- optim(par = rnorm(ncol(df)),
                               fn = loss.function,
                               method = "BFGS")
    }
  }else if(sel.loss.function=="cross-entropy"){
    for (i in seq(1,no.starts)) {
      beta.length<- ncol(df)*length(levels(as.factor(y)))
      result.list[[i]]<- optim(par = rnorm(beta.length),
                               fn = loss.function,
                               method = "BFGS")
    }
  }
  
  return(result.list)
}






model.trial<- elastic.net.function(x,y, sel.loss.function = "cross-entropy", spline.pos = c(1,4), spline.knots = c(5,5),no.starts = 10, seed = 4576975, alpha = 0.5, lambda = 1)

model.trial<- elastic.net.function(x,y, sel.loss.function = "proportion", spline.pos = c(1,4), spline.knots = c(5,5),no.starts = 10, seed = 4576975, alpha = 0.5, lambda = 1)

model.trial<- elastic.net.function(x,y, sel.loss.function = "quantile", sel.quantile = 0.5, spline.pos = c(1,4), spline.knots = c(5,5),no.starts = 30, seed = 158165, alpha = 0.5, lambda = 1)


























quantile.loss.fct<- function(beta,d,y,q,a,l){
  residual<- y-as.matrix(d)%*%matrix(beta,ncol = 1)
  positive<- residual>0
  return(sum(q*residual[positive])+sum((q-1)*residual[!positive])+l*(a*sum(abs(beta))+(1-a)*sqrt(sum(beta^2))))
}

proportion.loss.fct<- function(beta,d,y,a,l){
  transformed.fitted<- 1/(1+exp(-as.matrix(d)%*%matrix(beta,ncol = 1)))
  out<- sum(-y*log(transformed.fitted)-(1-y)*log(1-transformed.fitted)) + l*(a*sum(abs(beta))+(1-a)*sqrt(sum(beta^2)))
  return(out)
}

cross.entropy.loss.fct<- function(beta,d,y,a,l){
  beta.matrix<- matrix(beta,nrow = ncol(d))
  fitted<- exp(as.matrix(d)%*%beta.matrix)
  q<- diag(rowSums(fitted)^(-1))%*%fitted
  y_dummy<- model.matrix(~as.factor(y))
  out<- y_dummy*q + l*(a*sum(abs(beta))+(1-a)*sqrt(sum(beta^2)))
  return(out)
}
cross.entropy.loss.fct(rnorm(p*3),x,y,0.5,1)


set.seed(2624)
n<- 100
p<- 50
y<- as.factor(sample(LETTERS[1:3],n,replace = TRUE))
# y<- rnorm(n)
# y<- round(runif(n))
x<- as.data.frame(matrix(rnorm(n*p), nrow = n))
colnames(x)<- paste0(sample(LETTERS,ncol(x),replace = TRUE),round(rnorm(ncol(x)),4))
