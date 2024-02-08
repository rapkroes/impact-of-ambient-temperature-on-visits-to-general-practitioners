#elastic net function

library(pracma)
library(Matrix)

elastic.net.function<- function(inputdf, y, sel.loss.function, sel.quantile, spline.pos=NA, spline.knots, alpha,lambda, no.starts, seed){
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
# colnames(x)<- paste0(sample(LETTERS,ncol(x),replace = TRUE),round(rnorm(ncol(x)),4))
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





