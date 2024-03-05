#elastic net function

library(pracma)
library(Matrix)

elastic.net.function<- function(inputdf, y, sel.loss.function, sel.quantile, alpha,lambda, spline.pos=NA, spline.knots, no.starts, seed){
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
  constraint.rref.list<- list()
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
      constraint.rref.list[[i]]<- rref(part.constraint.matrix)
      # constraint.level.list[[i]]<- c(1,1,2,2,3,3,rep(c(1,2,3),no.knots-2))
      spline.list[[i]]<- addage_2
    }
    
    df<- df[,-spline.pos]
  }
  
  df$intercept<- 1
  no.not.spline.params<- ncol(df)
  
  for (i in seq_along(spline.list)) {
    df<- cbind(df,spline.list[[i]])
  }
  
  
  #generate loss function
  if(sel.loss.function=="quantile"){
    loss.function<- function(beta){
      residual<- y-as.matrix(df)%*%matrix(beta,ncol = 1)
      positive<- residual>0
      return(sum(sel.quantile*residual[positive]+(sel.quantile-1)*residual[!positive])+lambda*(alpha*sum(abs(beta))+(1-alpha)*sqrt(sum(beta^2))))
      }
  }else if(sel.loss.function=="proportion"){
    loss.function<- function(beta){
      transformed.fitted<- 1/(1+exp(-as.matrix(df)%*%matrix(beta,ncol = 1)))
      out<- sum(-y*log(transformed.fitted)-(1-y)*log(1-transformed.fitted))+lambda*(alpha*sum(abs(beta))+(1-alpha)*sqrt(sum(beta^2)))
      return(out)
    }
  }else if(sel.loss.function=="cross-entropy"){
    loss.function<- function(beta.matrix){
      transformed.fitted<- exp(as.matrix(df)%*%beta.matrix)
      transformed.fitted<- log(transformed.fitted*matrix((rowSums(transformed.fitted))^(-1),nrow = nrow(transformed.fitted), ncol = ncol(ncol(transformed.fitted))))
      y.dummy.matrix<- model.matrix(~as.factor(y))
      sel.transformed.fitted<- -y.dummy.matrix*transformed.fitted
      out<- sum(sel.transformed.fitted)+lambda*(alpha*sum(abs(beta))+(1-alpha)*sqrt(sum(beta^2)))
      return(out)
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
  
  complete.constraint.matrix_2<- rbind(complete.constraint.matrix,-complete.constraint.matrix)
  
  complete.rref.constraint.matrix<- matrix(0,ncol = no.not.spline.params+sum(spline.knots)*4, nrow = sum(spline.knots)*3)
  start.col<- no.not.spline.params+1
  start.row<- 1
  for(i in seq_along(constraint.list)){
    n<- nrow(constraint.rref.list[[i]])
    p<- ncol(constraint.rref.list[[i]])
    complete.constraint.matrix[seq(start.row,start.row+n-1),seq(start.col,start.col+p-1)]<- constraint.rref.list[[i]]
    start.row<- start.row+n
    start.col<- start.col+p
  }
  
  browser()
  #next up: draw start points subject to the linear constraints
  start.value.list<- list()
  
  rref.constraints<- rref(complete.constraint.matrix)
  independent.cols<- colSums(rref.constraints!=0)==0
  dfs<- ncol(independent.cols)-sum(independent.cols)
  for(s in seq(1,no.starts)){
    start.values<- rep(NA,nrow(rref.constraints))
    start.values[independent.cols]<- rnorm(sum(independent.cols))
    start.values[seq(nrow(rref.constraints)+1,ncol(rref.constraints))]<- rnorm(length(seq(nrow(rref.constraints)+1,ncol(rref.constraints))))
    for(j in seq(nrow(rref.constraints),1)){
      start.values[j]<- -1*sum(start.values[seq(j+1,length(start.values))]*rref.constraints[j,seq(j+1,length(start.values))], na.rm = TRUE)
    }
    start.value.list[[s]]<- start.values
  }
  
  residual.list<- list()
  sign.list<- list()
  for (i in seq(1,no.starts)) {
    # constraint.rns<- optim(par = rnorm(ncol(df)),
    #                        fn = function(beta){
    #                          sum((complete.rref.constraint.matrix%*%beta)^2)
    #                        },
    #                        gr = function(beta){
    #                          as.vector(2*t(complete.rref.constraint.matrix)%*%complete.rref.constraint.matrix%*%beta)
    #                        },
    #                        method = "BFGS")
    # start.value.list[[i]]<- constraint.rns$par
    # residual.list[[i]]<- complete.rref.constraint.matrix%*%constraint.rns$par
    
    
    
    
    
    # constraint.rns<- optim(par = rnorm(ncol(df)),
    #                        fn = function(beta){
    #                          log(sum((complete.constraint.matrix%*%beta)^2))
    #                        },
    #                        gr = function(beta){
    #                          t(complete.rref.constraint.matrix)%*%complete.constraint.matrix%*%beta/(sum((complete.constraint.matrix%*%beta)^2))
    #                        },
    #                        method = "BFGS")
    # start.value.list[[i]]<- constraint.rns$par
    # sign.list[[i]]<- sign(complete.constraint.matrix%*%start.value.list[[i]])
    
    
    
    
    constraint.rns<- optim(par = rnorm(ncol(df)),
                           fn = function(beta){
                             sum((complete.constraint.matrix%*%beta)^2)
                           },
                           gr = function(beta){
                             as.vector(2*t(complete.constraint.matrix)%*%complete.constraint.matrix%*%beta)
                           },
                           method = "BFGS")
    start.value.list[[i]]<- constraint.rns$par
    sign.list[[i]]<- sign(complete.constraint.matrix%*%start.value.list[[i]])
    
    #old but promising#old but promising#old but promising#old but promising#old but promising#old but promising#old but promising#old but promising#old but promising#old but promising#old but promising#old but promising
    # start.values<- rnorm(no.not.spline.params)
    # 
    # constraint2rns<- function(constraint,params){
    #   out<- params
    #   used.params<- constraint!=0
    #   undetermined.params<- is.na(params)
    #   new.param.selector<- which(used.params & undetermined.params)
    #   if(length(new.param.selector)>1){
    #     free<- new.param.selector[1]
    #     out[new.param.selector[-1]]<- rnorm(length(new.param.selector)-1)
    #   }else if(length(new.param.selector)==1){
    #     free<- new.param.selector
    #   }else{
    #     stop(paste("There is an issue with constraint", constraint,"and parameters",params, "since there is no undetermined parameter in this problem!"))
    #   }
    #   im<- sum(out*constraint,na.rm = TRUE)
    #   out[free]<- -im/constraint[free]
    #   return(out)
    # }
    # level2order<- function(levelvec){
    #   order_3<- which(levelvec==3)
    #   order_3<- c(order_3[-2],order_3[2])
    #   order_2<- which(levelvec==2)
    #   order_2<- c(order_2[-2],order_2[2])
    #   order_1<- which(levelvec==1)
    #   order_1<- c(order_1[-2],order_1[2])
    #   out<- c(order_3,order_2,order_1)
    #   return(out)
    # }
    # 
    # for(j in seq_along(constraint.list)){
    #   order.vec<- level2order(constraint.level.list[[j]])
    #   constraint.mat<- constraint.list[[j]]
    #   sel.params<- rep(NA,ncol(constraint.mat))
    #   for(s in seq_along(order.vec)){
    #     sel<- order.vec[s]
    #     sel.constraint<- constraint.mat[sel,]
    #     sel.params<- constraint2rns(sel.constraint,sel.params)
    #   }
    #   if(any(is.na(sel.params))) stop(paste("For spline",j,"there is at least one undetermined parameter. The chosen parameters are",sel.params))
    #   start.values<- c(start.values,sel.params)
    # }
    # start.value.list[[i]]<- start.values
    
    # #new version using rref
    # start.values<- rnorm(no.not.spline.params)
    # 
    # rref2rns<- function(cm){
    #   out<- rep(NA,ncol(cm))
    #   rank<- rankMatrix(cm)[[1]]
    #   p<- ncol(cm)
    #   
    #   row<- rank
    #   constraint<- cm[row,]
    #   out[seq(rank+1,p)]<- rnorm(length(seq(rank+1,p)))
    #   out[rank]<- -sum(constraint*out, na.rm = TRUE)
    #   for(row in seq(rank-1,1)){
    #     constraint<- cm[row,]
    #     out[row]<- -sum(constraint*out, na.rm = TRUE)
    #   }
    #   return(out)
    # }
    # 
    # for(j in seq_along(constraint.list)){
    #   constraint.mat<- constraint.list[[j]]
    #   sel.params<- rep(NA,ncol(constraint.mat))
    #   constraint.rref<- rref(constraint.mat)
    #   
    #   start.values<- c(start.values,sel.params)
    # }
    # start.value.list[[i]]<- start.values
    
    # #redone old procedure
    # start.values<- rnorm(no.not.spline.params)
    # for(j in seq_along(constraint.list)){
    #   constraint.mat<- constraint.list[[j]]
    #   p<- ncol(constraint.mat)
    #   rank<- rankMatrix(rref(constraint.mat))
    #   beta_p<- rnorm(p-rank)
    #   full.system<- constraint.mat[,-seq(rank+1,p)]
    #   rhs<- constraint.mat[,seq(rank+1,p)]%*%matrix(beta_p,ncol = 1)
    #   beta<- solve(full.system,rhs)
    #   start.values<- c(start.values,beta,beta_p)
    # }
    # start.value.list[[i]]<- start.values
  }
  browser()
  #run the constrained optimisation problem
  result.list<- list()
  for (i in seq_along(start.value.list)) {
    result.list[[i]]<- constrOptim(theta = start.value.list[[i]], 
                                   f = loss.function, 
                                   ui = complete.constraint.matrix_2, 
                                   ci = c(sign.list[[i]]*rep(10^(-5),0.5*nrow(complete.constraint.matrix_2)),
                                          -1*sign.list[[i]]*rep(10^(-5),0.5*nrow(complete.constraint.matrix_2)))
    )
  }
  return(result.list)
}

# set.seed(2624)
# n<- 100
# p<- 50
# y<- as.factor(sample(LETTERS[1:3],n,replace = TRUE))
# x<- as.data.frame(matrix(rnorm(n*p), nrow = n))
# colnames(x)<- paste0(sample(LETTERS,ncol(x),replace = TRUE),1:p)
model.trial<- elastic.net.function(x,y, sel.loss.function = "quantile", sel.quantile = 0.5, spline.pos = c(1,4), spline.knots = c(5,5),no.starts = 30, seed = 158165)
#dimension of complete.constraint.matrix is off. Should have as many columns as df.


quantile.loss.fct<- function(beta,d,y,q,a,l){
  residual<- y-as.matrix(d)%*%matrix(beta,ncol = 1)
  positive<- residual>0
  return(sum(q*residual[positive]+(q-1)*residual[!positive])+l*(a*sum(abs(beta))+(1-a)*sqrt(sum(beta^2))))
}

proportion.loss.fct<- function(beta,d,y,a,l){
  transformed.fitted<- 1/(1+exp(-as.matrix(d)%*%matrix(beta,ncol = 1)))
  out<- sum(-y*log(transformed.fitted)-(1-y)*log(1-transformed.fitted))+l*(a*sum(abs(beta))+(1-a)*sqrt(sum(beta^2)))
  return(out)
}

cross.entropy.loss.fct<- function(beta.matrix,d,y,a,l){
  transformed.fitted<- exp(as.matrix(d)%*%beta.matrix)
  transformed.fitted<- log(transformed.fitted*matrix((rowSums(transformed.fitted))^(-1),nrow = nrow(transformed.fitted), ncol = ncol(ncol(transformed.fitted))))
  y.dummy.matrix<- model.matrix(~as.factor(y))
  sel.transformed.fitted<- -y.dummy.matrix*transformed.fitted
  out<- sum(sel.transformed.fitted)+l*a*sum(abs(beta))+l*(a*sum(abs(beta))+(1-a)*sqrt(sum(beta^2)))
  return(out)
}




rref.constraints<- rref(complete.constraint.matrix)
dfs<- ncol(rref.constraints)-which(rref.constraints[nrow(rref.constraints),]==1)[1]
for(s in seq(1,no.starts)){
  start.values<- c(rep(NA,ncol(rref.constraints)),rnorm(dfs))
  for(j in seq(nrow(rref.constraints),1)){
    beta_i[j]<- -1*beta_i[seq(j+1,length(beta_i))]%*%rref.constraints[j,seq(j+1,length(beta_i))]
  }
  start.value.list[[s]]<- start.values
}















constraint.graph<- function(A){
  p<- ncol(A)
  out<- diag(p)
  
  B<- matrix(as.numeric(A!=0), ncol = p)
  for(j in seq(1,p)){
    for(i in seq(1,nrow(A))[B[,j]==1]){
      out[j,j:p]<- as.vector(out[j,j:p])+as.vector(B[i,j:p])
    }
  }
  out<- matrix(as.numeric(out!=0), ncol = p)
  colnames(out)<- rownames(out)<- colnames(A)
  
  return(out)
}
constraint.graph(cmat)
cgraph<- constraint.graph(cmat)
cgraph_2<- cgraph[1:16,1:16]
plot(graph.adjacency(cgraph_2, mode = "undirected"))
cgraph<- as(constraint.graph(cmat), "graphNEL")
plot(constraint.graph(cmat))







library(splines2)
nsk(x[,1],knots = quantile(x[,1], probs = seq(0.2,0.8,by=0.2)))




















































elastic.net<- function(inputdf, y, standardize.y=FALSE, spline.pos=NULL, spline.knots=NULL, sel.loss.function, sel.quantile=NULL, alpha, lambda, seed, no.starts=40, no.workers, max.iter=60, step.size=1, tol=1e-6){
  
  set.seed(seed)
  
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
      linear.constraint<- 0.5*sum((constraint.matrix%*%beta.vec)^2)
      regularization<- lambda*(alpha*sum(abs(beta.vec))+(1-alpha)*sqrt(sum(beta.vec^2)))
      return(likelihood + linear.constraint+ regularization)
    }
    loss.gradient<- function(ind,beta.vec){
      dataframe<- as.matrix(df)
      likelihood<- (sel.quantile-1+as.numeric(dataframe%*%beta.vec))%*%dataframe[,ind]/nrow(dataframe)
      linear.constraint<- constraint.matrix[,ind]%*% (constraint.matrix%*%beta.vec)
      regularization<- lambda*(alpha*sign(beta.vec[ind])+(1-alpha)*2*beta.vec[ind]/sqrt(sum((beta.vec)^2)))
      out<- as.numeric(likelihood +linear.constraint +regularization)
      return(out)
    }
  }else if(sel.loss.function=="proportion"){
    loss.function<- function(beta.vec){
      fitted<- as.numeric(as.matrix(df)%*%beta.vec)
      likelihood<- (1-y)%*%fitted+sum(log(1+exp(-fitted)))
      #likelihood<- as.numeric((as.matrix(df)%*%beta.vec)%*%(1-y)+sum(log(1+exp(-as.matrix(df)%*%beta.vec))))
      linear.constraint<- 0.5*sum((constraint.matrix%*%beta.vec)^2)
      regularization<- lambda*(alpha*sum(abs(beta.vec))+(1-alpha)*sqrt(sum(beta.vec^2)))
      return(likelihood + linear.constraint+ regularization)
    }
    loss.gradient<- function(ind, beta.vec){
      likelihood<- as.matrix(df)[,ind]%*%((1+exp(-as.matrix(df)%*%beta.vec))^(-1)-y)
      linear.constraint<- constraint.matrix[,ind]%*% (constraint.matrix%*%beta.vec)
      regularization<- lambda*(alpha*sign(beta.vec[ind])+(1-alpha)*2*beta.vec[ind]/sqrt(sum((beta.vec)^2)))
      out<- as.numeric(likelihood +linear.constraint +regularization)
      return(out)
    }
  }else if(sel.loss.function=="cross-entropy"){
    loss.function<- function(beta.vec){
      beta.matrix<- matrix(beta.vec,nrow = ncol(df))
      fitted<- exp(as.matrix(df)%*%beta.matrix)
      q<- diag(rowSums(fitted)^(-1))%*%fitted
      y_dummy<- model.matrix(~as.factor(y)+0)
      likelihood<- sum(y_dummy*q) #sum?
      linear.constraint<- 0.5*sum((constraint.matrix%*%beta.matrix)^2)
      regularization<- lambda*(alpha*sum(abs(beta.vec))+(1-alpha)*sqrt(sum(beta.vec^2)))
      return(likelihood + linear.constraint+ regularization)
    }
    loss.gradient<- function(ind, beta.vec){
      k<- (ind-1) %% ncol(df)+1
      m<- ((ind-1) %/% ncol(df))+1
      beta.matrix<- matrix(beta.vec,nrow = ncol(df))
      fitted<- exp(as.matrix(df)%*%beta.matrix)
      y_dummy<- model.matrix(~as.factor(y)+0)
      likelihood_1<- y_dummy[,m]%*%as.matrix(df)[,k]
      likelihood_2<- sum(t(y_dummy)%*%diag((rowSums(fitted))^(-1))%*%(as.matrix(df)[,k]*fitted[,m]))
      likelihood<- likelihood_1-likelihood_2
      linear.constraint<- constraint.matrix[,k]%*% as.numeric(constraint.matrix%*%beta.matrix[,m])
      regularization<- lambda*(alpha*sign(beta.vec[ind])+(1-alpha)*2*beta.vec[ind]/sqrt(sum((beta.vec)^2)))
      out<- as.numeric(likelihood +linear.constraint +regularization)
      return(out)
    }
  }
  
  #start values
  beta.list<- list()
  if(sel.loss.function=="cross-entropy"){
    no.params<- length(levels(as.factor(y)))*ncol(constraint.matrix)
  }else{
    no.params<- ncol(constraint.matrix)
  }
  for(i in seq(1, no.starts)){
    beta.list[[i]]<- rnorm(no.params)
  }
  browser()
  #optimization process
  coordinate.cluster<- makeCluster(no.workers)
  clusterExport(cl = coordinate.cluster, varlist =  c("beta.list", "loss.function", "loss.gradient", "df", "y", "constraint.matrix", "max.iter", "step.size", "tol"), envir = environment())
  
  results<- parLapply(cl = coordinate.cluster,seq_along(beta.list), fun = function(z){
  #results<- lapply(seq_along(beta.list), FUN = function(z){
    beta_start<- beta.list[[z]]
    ticker<- 1
    convergence<- FALSE
    beta_new<- beta_start
    beta_sugg<- beta_new
    step.vec<- rep(step.size,length(beta_start))
    while(ticker<=max.iter && !convergence){
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
      convergence<- loss.function(beta_baseline)-loss.function(beta_new) <=tol
    }
    out.list<- list()
    out.list$converged<- convergence
    out.list$beta<- beta_new
    out.list$loss_old<- loss.function(beta_start)
    out.list$loss_new<- loss.function(beta_new)
    out.list$iterations<- ticker-1
    return(out.list)
  })
  
  out<- list()
  out$converged<- numeric(no.starts)
  out$loss_old<- numeric(no.starts)
  out$loss_new<- numeric(no.starts)
  out$iterations<- numeric(no.starts)
  out$beta<- matrix(NA,ncol = no.params, nrow = no.starts)
  
  for(i in seq(1,no.starts)){
    out$converged[i]<- results[[i]]$converged
    out$beta[i,]<- results[[i]]$beta
    out$loss_old[i]<- results[[i]]$loss_old
    out$loss_new[i]<- results[[i]]$loss_new
    out$iterations[i]<- results[[i]]$iterations
  }
  
  return(out)
}

library(parallel)
set.seed(2624)
beta_true<- rnorm(p)
y<- as.matrix(x)%*%beta_true
model.trial_q<- elastic.net(inputdf=x, y, standardize.y=FALSE, spline.pos=c(49,50), spline.knots=c(5,5), sel.loss.function = "quantile", sel.quantile=0.5, alpha=0.5, lambda=1, seed = 1681, no.starts=4, no.workers=2, max.iter=60, step.size=1, tol=1e-6)


set.seed(18565)
y<- round(runif(n))
model.trial_p<- elastic.net(inputdf=x, y, standardize.y=FALSE, spline.pos=c(49,50), spline.knots=c(5,5), sel.loss.function = "proportion", sel.quantile=0.5, alpha=0.5, lambda=1, seed = 1681, no.starts=4, no.workers=2, max.iter=60, step.size=1, tol=1e-6)

set.seed(2624)
y<- as.factor(sample(LETTERS[1:3],n,replace = TRUE))
model.trial_c<- elastic.net(inputdf=x, y, standardize.y=FALSE, spline.pos=c(49,50), spline.knots=c(5,5), sel.loss.function = "cross-entropy", sel.quantile=0.5, alpha=0.5, lambda=4, seed = 1681, no.starts=2, no.workers=2, max.iter=60, step.size=1, tol=1e-6)








 
proportion.loss.fct<- function(beta){
  browser()
  transformed.fitted<- 1/(1+exp(-as.matrix(x)%*%beta))
  out<- sum(-y*log(transformed.fitted)-(1-y)*log(1-transformed.fitted))
  return(out)
}

logistic_2<- function(beta){
  as.numeric(t(as.matrix(x)%*%beta)%*%(1-y)+sum(log(1+exp(-as.matrix(x)%*%beta))))
}


logistic_2(1:p)
proportion.loss.fct(1:p)

logistic_grad<- function(ind, beta){
  as.matrix(x)[,ind]%*%((1+exp(-as.matrix(x)%*%beta))^(-1)-y)
}
logistic_grad(2,rnorm(p))
for(i in 1:p){
  print(logistic_grad(i,rnorm(p, sd=0.1)))
}

cross.entropy<- function(beta){
  beta.matrix<- matrix(beta,nrow = ncol(x))
  fitted<- exp(as.matrix(x)%*%beta.matrix)
  log_q<- log(diag(rowSums(fitted)^(-1))%*%fitted)
  y_dummy<- model.matrix(~as.factor(y))
  likelihood<- y_dummy*log_q
  return(likelihood)
}



































elastic.net<- function(inputdf, y, standardize.y=FALSE, spline.pos=NULL, spline.knots=NULL, sel.loss.function, sel.quantile=NULL, alpha, lambda, seed, no.starts=1, no.workers=2, max.iter=1000, step.size=1, tol=1e-6){
  #A function which calculates the elastic net estimate of a regression problem with splines using coordinate descent. The regression problems that can be solved are quantile regression and logistic regression (multiclass cross-entropy is currently not supported)
  #inputdf is a dataframe of variables used to estimate y.
  #y is the 'dependent' variable
  #standardize.y can be used to standardize y
  #spline.pos is a vector which specifies which columns of inputdf are to be estimated with a spline.
  #spline.knots are the number of knots that are used for each spline. It is a vector, too; the entry on spline.knots corresponds to the entry on spline.pos.
  #sel.loss.function is the loss function: either "quantile" or "proportion"
  #alpha is the parameter that shifts between lasso (alpha=0) and Ridge (alpha=1) penalty.
  #lambda is the regularization parameter: the higher lambda, the more the parameters are shrunk towards 0.
  #seed is a seed for the random number generator. It must be given to ensure replicability of the operations.
  #no.starts is the number of starts of the estimation. Usually should be set to 1. For a single start, no. starts will use the ordinary least squares estimates as start values. For more starts, the start values are proportionally shrunk towards 0.
  #no.workers is the numbers of workers used to calculate the coefficients if more than one start is chosen.
  #max.iter is the maximum number of iterations.
  #step.size is the starting length of the step.
  #tol is a tolerance threshold used in several places: convergence, shrinkage to values close to zero, and step lengths. In these places, numbers below the threshold are considered zero.
  
  set.seed(seed)
  
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
      linear.constraint<- 0.5*sum((constraint.matrix%*%beta.vec)^2)
      regularization<- lambda*(alpha*sum(abs(beta.vec))+(1-alpha)*sqrt(sum(beta.vec^2)))
      return(likelihood + linear.constraint+ regularization)
    }
    loss.gradient<- function(ind,beta.vec){
      dataframe<- as.matrix(df)
      likelihood<- (sel.quantile-1+as.numeric(dataframe%*%beta.vec))%*%dataframe[,ind]/nrow(dataframe)
      linear.constraint<- constraint.matrix[,ind]%*% (constraint.matrix%*%beta.vec)
      regularization<- lambda*(alpha*sign(beta.vec[ind])+(1-alpha)*2*beta.vec[ind]/sqrt(sum((beta.vec)^2)))
      out<- as.numeric(likelihood +linear.constraint +regularization)
      return(out)
    }
  }else if(sel.loss.function=="proportion"){
    loss.function<- function(beta.vec){
      fitted<- as.numeric(as.matrix(df)%*%beta.vec)
      likelihood<- (1-y)%*%fitted+sum(log(1+exp(-fitted)))
      linear.constraint<- 0.5*sum((constraint.matrix%*%beta.vec)^2)
      regularization<- lambda*(alpha*sum(abs(beta.vec))+(1-alpha)*sqrt(sum(beta.vec^2)))
      return(likelihood + linear.constraint+ regularization)
    }
    loss.gradient<- function(ind, beta.vec){
      likelihood<- as.matrix(df)[,ind]%*%((1+exp(-as.matrix(df)%*%beta.vec))^(-1)-y)
      linear.constraint<- constraint.matrix[,ind]%*% (constraint.matrix%*%beta.vec)
      regularization<- lambda*(alpha*sign(beta.vec[ind])+(1-alpha)*2*beta.vec[ind]/sqrt(sum((beta.vec)^2)))
      out<- as.numeric(likelihood +linear.constraint +regularization)
      return(out)
    }
  }else if(sel.loss.function=="cross-entropy"){
    loss.function<- function(beta.vec){
      beta.matrix<- matrix(beta.vec,nrow = ncol(df))
      fitted<- exp(as.matrix(df)%*%beta.matrix)
      q<- diag(rowSums(fitted)^(-1))%*%fitted
      y_dummy<- model.matrix(~as.factor(y)+0)
      likelihood<- sum(y_dummy*q) #sum?
      linear.constraint<- 0.5*sum((constraint.matrix%*%beta.matrix)^2)
      regularization<- lambda*(alpha*sum(abs(beta.vec))+(1-alpha)*sqrt(sum(beta.vec^2)))
      return(likelihood + linear.constraint+ regularization)
    }
    loss.gradient<- function(ind, beta.vec){
      k<- (ind-1) %% ncol(df)+1
      m<- ((ind-1) %/% ncol(df))+1
      beta.matrix<- matrix(beta.vec,nrow = ncol(df))
      fitted<- exp(as.matrix(df)%*%beta.matrix)
      y_dummy<- model.matrix(~as.factor(y)+0)
      likelihood_1<- y_dummy[,m]%*%as.matrix(df)[,k]
      likelihood_2<- sum(t(y_dummy)%*%diag((rowSums(fitted))^(-1))%*%(as.matrix(df)[,k]*fitted[,m]))
      likelihood<- likelihood_1-likelihood_2
      linear.constraint<- constraint.matrix[,k]%*% as.numeric(constraint.matrix%*%beta.matrix[,m])
      regularization<- lambda*(alpha*sign(beta.vec[ind])+(1-alpha)*2*beta.vec[ind]/sqrt(sum((beta.vec)^2)))
      out<- as.numeric(likelihood +linear.constraint +regularization)
      return(out)
    }
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
  browser()
  if(sel.loss.function=="quantile"){
    form<- as.formula(paste("y~0+",paste(colnames(df), sep = "", collapse = "+ ")))
    beta_start<- rq(formula = form, tau = sel.quantile, data = base.data)$coefficients
  }else if(sel.loss.function=="proportion"){
    form<- as.formula(paste("y~0+",paste(colnames(df), sep = "", collapse = "+ ")))
    beta_start<- glm(formula = form, family = binomial, data = base.data)$coefficients
  }else if(sel.loss.function=="cross-entropy"){
    beta_start<- optim(rnorm(no.params), fn = function(beta.vec){
      beta.matrix<- matrix(beta.vec,nrow = ncol(df))
      fitted<- exp(as.matrix(df)%*%beta.matrix)
      q<- diag(rowSums(fitted)^(-1))%*%fitted
      y_dummy<- model.matrix(~as.factor(y)+0)
      likelihood<- sum(y_dummy*q)
      return(likelihood)
    },
    gr = function(beta.vec){
      k<- (seq(1,length(beta.vec))-1) %% ncol(df)+1
      m<- ((seq(1,length(beta.vec))-1) %/% ncol(df))+1
      beta.matrix<- matrix(beta.vec,nrow = ncol(df))
      fitted<- exp(as.matrix(df)%*%beta.matrix)
      y_dummy<- model.matrix(~as.factor(y)+0)
      likelihood_1<- y_dummy[,m]%*%as.matrix(df)[,k]
      likelihood_2<- sum(t(y_dummy)%*%diag((rowSums(fitted))^(-1))%*%(as.matrix(df)[,k]*fitted[,m]))
      likelihood<- likelihood_1-likelihood_2
      return(likelihood)
    },
    method = "BFGS")
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
          beta_round<- beta_sugg
          beta_round[i]<- 0
          if(loss.function(beta_round)<=loss.function(beta_sugg)){
            beta_sugg<- beta_round
          }
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
  
  return(out)
}


set.seed(2624)
beta_true<- rnorm(p)
y<- as.matrix(x)%*%beta_true+rnorm(n, sd = 0.5)
tic()
model.trial_q<- elastic.net(inputdf=x, y, standardize.y=FALSE, spline.pos=c(49,50), spline.knots=c(5,5), sel.loss.function = "quantile", sel.quantile=0.5, alpha=0.1, lambda=0.1, seed = 1681, no.starts = 1, max.iter=10000, step.size=1, tol=1e-6)
toc()
rbind(beta_true[1:48], model.trial_q$beta[,1:48])


set.seed(18565)
y<- round(runif(n))
as.formula(paste("y~0+",paste(colnames(x), sep = "", collapse = "+ ")))
glm(as.formula(paste("y~0+",paste(colnames(x), sep = "", collapse = "+ "))), family = "binomial")
tic()
model.trial_p<- elastic.net(inputdf=x, y, standardize.y=FALSE, spline.pos=c(49,50), spline.knots=c(5,5), sel.loss.function = "proportion", sel.quantile=0.5, alpha=0.1, lambda=.1, seed = 1681, no.starts=1, max.iter=60, step.size=1, tol=1e-6)
toc()

set.seed(2624)
y<- as.factor(sample(LETTERS[1:3],n,replace = TRUE))
model.trial_c<- elastic.net(inputdf=x, y, standardize.y=FALSE, spline.pos=c(49,50), spline.knots=c(5,5), sel.loss.function = "cross-entropy", sel.quantile=0.5, alpha=0.5, lambda=4, seed = 1681, no.starts=2, no.workers=2, max.iter=60, step.size=1, tol=1e-6)
