#elastic net function
elastic.net.function<- function(inputdf, y, sel.loss.function, sel.quantile, spline.pos=NA, spline.knots, alpha,lambda, no.starts){
  #linear regression with elastic net regularisation, written by myself.
  #inputdf= data frame of inputs
  #y= response variable
  #loss.function is the loss function to be used. Either "quantile", "proportion", or "cross-entropy" may be chosen.
  #spline.pos gives the numeric position which columns are to be fitted with a spline
  #spline.knots gives the number of knots for each of the splines
  #alpha shifts the weight between l1 and l2 penalty
  #lambda is the regularization penalty
  #no.starts is the number of different starting points in the optimisation process
  
  #standardize data
  mean.vec<- colMeans(inputdf)
  var.vec<- apply(inputdf,2,var)
  df<- (inputdf-matrix(mean.vec,nrow = nrow(inputdf), ncol = ncol(inputdf), byrow = TRUE))/matrix(sqrt(var.vec),nrow = nrow(inputdf), ncol = ncol(inputdf), byrow = TRUE)
  
  #create new spline data and contraints
  spline.list<- list()
  constraint.list<- list()
  if(!is.na(spline.pos)){
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
      spline.list[[i]]<- addage
      
      #spline constraints
      part.constraint.matrix<- matrix(0,nrow = no.knots*2, ncol = no.knots*4)
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
      part.constraint.matrix[1,acc("a_2")]<- 3*(k[1])^2
      part.constraint.matrix[1,acc("b_2")]<- 2*k[1]
      part.constraint.matrix[1,acc("c_2")]<- 1
      
      part.constraint.matrix[2,acc(paste0("a_",no.knots+1))]<- -1
      part.constraint.matrix[2,acc(paste0("a_",no.knots))]<- 3*(k[no.knots])^2
      part.constraint.matrix[2,acc(paste0("b_",no.knots))]<- 2*(k[no.knots])
      part.constraint.matrix[2,acc(paste0("c_",no.knots))]<- 1
      
      part.constraint.matrix[3,acc("a_2")]<- 6*k[1]
      part.constraint.matrix[3,acc("b_2")]<- 2
      
      part.constraint.matrix[4,acc(paste0("a_",no.knots))]<- 6*k[no.knots]
      part.constraint.matrix[4,acc(paste0("b_",no.knots))]<- 2
      
      if(no.knots>2){
        ticker<- 5
        for(j in seq(1,no.knots-2)){
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
      
      colnames(part.constraint.matrix)<- NULL
      constraint.list[[i]]<- part.constraint.matrix
      
    }
    
    df<- df[,-spline.pos]
  }
  
  no.not.spline.params<- ncol(df)
  
  for (i in seq_along(spline.list)) {
    df<- cbind(df,spline.list[[i]])
  }
  
  #generate loss function
  if(sel.loss.function=="quantile"){
    loss.function<- function(beta){
      residual<- y-df%*%matrix(beta,ncol = 1)
      positive<- residual>0
      return(sum(sel.quantile*residual[positive]+(sel.quantile-1)*residual[!positive])+lambda*(alpha*sum(abs(beta))+(1-alpha)*sqrt(sum(beta^2))))
      }
  # }else if(sel.loss.function=="proportion"){
  #   loss.function<- function(beta){
  #     transformed.fitted<- 1/(1+exp(-df%*%matrix(beta,ncol = 1)))
  #     return(sum(-y*log(transformed.fitted)-(1-y)*log(1-transformed.fitted))+lambda*(alpha*sum(abs(beta))+(1-alpha)*sqrt(sum(beta^2)))
  #   }
  # }else if(sel.loss.function=="cross-entropy"){
  #   loss.function<- function(beta.matrix){
  #     transformed.fitted<- exp(df%*%beta.matrix)/sum(exp(df%*%beta.matrix))
  #     y.dummy.matrix<- model.matrix(~as.factor(y))
  #     sel.transformed.fitted<- rowSums(-y.dummy.matrix*log(transformed.fitted))
  #     return(sum(sel.transformed.fitted)+lambda*(alpha*sum(abs(beta.matrix))+(1-alpha)*sqrt(sum(beta.matrix^2)))
  #   }
  }else if(sel.loss.function=="proportion"){
    loss.function<- function(beta){
      transformed.fitted<- 1/(1+exp(-df%*%matrix(beta,ncol = 1)))
      out<- sum(-y*log(transformed.fitted)-(1-y)*log(1-transformed.fitted))+lambda*(alpha*sum(abs(beta))+(1-alpha)*sqrt(sum(beta^2)))
      return(out)
    }
  }else if(sel.loss.function=="cross-entropy"){
    loss.function<- function(beta.matrix){
      transformed.fitted<- exp(df%*%beta.matrix)
      transformed.fitted<- log(transformed.fitted*matrix((rowSums(transformed.fitted))^(-1),nrow = nrow(transformed.fitted), ncol = ncol(ncol(transformed.fitted))))
      y.dummy.matrix<- model.matrix(~as.factor(y))
      sel.transformed.fitted<- -y.dummy.matrix*transformed.fitted
      out<- sum(sel.transformed.fitted)+lambda*alpha*sum(abs(beta))+lambda*(alpha*sum(abs(beta))+(1-alpha)*sqrt(sum(beta^2)))
      return(out)
    }
  }
  
  #create complete linear constraint matrix
  complete.constraint.matrix<- matrix(0,ncol = no.not.spline.params+sum(spline.knots)*4, nrow = sum(spline.knots)*2)
  start.col<- no.not.spline.params+1
  start.row<- 1
  for(i in seq_along(constraint.list)){
    n<- nrow(constraint.list[[i]])
    p<- ncol(constraint.list[[i]])
    complete.constraint.matrix[seq(start.row,start.row+n-1),seq(start.col,start.col+p-1)]<- constraint.list[[i]]
    start.row<- start.row+n
    start.col<- start.col+p
  }
  
  #next up: draw start points, run the constrained optimisation problem
  start.value.list<- list()
  for (i in seq(1,no.starts)) {
    start.value.list[[i]]<- rnorm(ncol(df))
  }
  
  browser()
  
  result.list<- list()
  for (i in seq_along(start.value.list)) {
    result.list[[i]]<- constrOptim(theta = start.value.list[[i]], f = loss.function, ui = complete.constraint.matrix, ci = rep(0,sum(spline.knots)*2))
  }
  return(result.list)
}

set.seed(2624)
n<- 100
p<- 50
y<- as.factor(sample(LETTERS[1:3],n,replace = TRUE))
x<- as.data.frame(matrix(rnorm(n*p), nrow = n))
model.trial<- elastic.net.function(x,y, sel.loss.function = "quantile", sel.quantile = 0.5, spline.pos = 1, spline.knots = 5,no.starts = 30)
#dimension of complete.constraint.matrix is off. Should have as many columns as df.
