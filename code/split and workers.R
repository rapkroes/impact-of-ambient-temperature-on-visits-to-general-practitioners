###############Test for workers and size
n_mc<- 20
duration<- numeric(n_mc)
set.seed(45165)
data.size<- 1000*sample(1:100,n_mc, replace = TRUE)
split.vec<- sample(50:500, n_mc, replace = TRUE)
workers<- sample(seq(10, 24), n_mc, replace = TRUE)
for(i in seq(1, n_mc)){
  if(data.size[i]>8*split.vec[i]){
    art.data(data.size[i])
    tic()
    add.stamm.new.par(Diag3,Stamm3,no.splits = split.vec[i], no.workers = workers[i])
    im<- toc()
    duration[i]<- im$toc - im$tic
  } 
}
selector<- duration>1
split.tuning.data<- as.data.frame(cbind(duration,data.size,split.vec,workers)[selector,])
split.tuning.data$nsquare_byk<- (data.size^2/split.vec)[selector]
split.tuning.data$n_byk<- (data.size/split.vec)[selector]
split.tuning.data$nsquare<- (data.size^2)[selector]
split.tuning.data$n_byworker<- (data.size/workers)[selector]
split.tuning.data$n_bysqrtworkers<- (data.size/sqrt(workers))[selector]
im<- lm(duration~., data = split.tuning.data)
summary(im)
im_2<- lm(duration~n_byworker, data = split.tuning.data)
summary(im_2) #number of splits makes no difference. Duration scales in n/no.workers.
im_3<- lm(log(duration)~log(data.size)+log(workers), data = split.tuning.data)
summary(im_3)
im_4<- lm(duration~n_bysqrtworkers, data = split.tuning.data)
summary(im_4)

dur<- list()
ticker<- 1
for(i in c(6, 10, 14, 18)){
  tic()
  add.stamm.new.par(Diag3, Stamm3, no.splits = 500, no.workers = workers[i])
  im<- toc()
  dur[[ticker]]<- im$toc - im$tic
  ticker<- ticker + 1
}
