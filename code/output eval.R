###booster evaluation

booster.eval<- function(booster, inputdf, no.draws, eval.var, eval.seq, seed, eval.type = "response"){
  # Evaluation algorithm for an lgbBooster. Draws entries from the training dataset and "twists" one selected variable, creating a ceteris paribus comparison. Thus, it can be seen how a variable changes with different inputs. At the end, a list of length no.draws is returned. Each element of the list is a data frame with two columns each: One is the "twisted" variable, the other one is the prediction of the booster.
  # booster is an lgbBooster object
  # inputdf is the dataset that the booster has been trained on. It serves as the basis for further evaluation.
  # no.draws is the number of entries drawn from inputdf. 
  # eval.var is the character name of the variable to be evaluated (the "X" variable) 
  # eval.seq is the vector of different values that are to be evaluated (the different values "X" may take)
  # seed is the random seed
  # eval.type prediction output type. Should be "response". For further details, check https://lightgbm.readthedocs.io/en/latest/R/reference/predict.lgb.Booster.html
  library(lightgbm)
  set.seed(seed)
  initial.data<- inputdf[sample(seq(1,nrow(inputdf)),no.draws,replace = FALSE),]
  
  eval.data<- as.data.frame(matrix(NA, nrow = no.draws * length(eval.seq),
                                   ncol = ncol(inputdf)))
  colnames(eval.data)<- colnames(inputdf)
  eval.var.col<- which(colnames(eval.data)==eval.var)
  for(i in seq_along(eval.seq)){
    sel.rows<- seq(1, no.draws) + (i-1) * no.draws
    eval.data[sel.rows,]<- initial.data
    eval.data[sel.rows,eval.var.col]<- eval.seq[i]
  }
  
  prediction.output<- matrix(predict(object = booster, 
                                     newdata = data.matrix(eval.data), 
                                     type = eval.type),
                             nrow = nrow(eval.data))
  
  out<- list()
  for(i in seq(1,no.draws)){
    selection<- seq(1, nrow(eval.data), by = no.draws)
    im<- cbind(eval.data[selection, eval.var.col], prediction.output[selection,])
    colnames(im)<- c(eval.var, paste(rep("V", ncol(prediction.output)), 
                                     seq(1, ncol(prediction.output)), 
                                     sep = "_"))
    out[[i]]<- im
  }
  return(out)
}

x<- booster.eval(booster = model_q2_TDI, inputdf = df_qx(di = "TDI", q = 2), 
             no.draws = 100, eval.var = "age", eval.seq = seq(18,30),
             seed = 18521)

eval.plot<- function(eval.list){
  browser()
  
  no.draws<- length(eval.list)
  x.seq<- eval.list[[1]][,1]
  range_x<- range(x.seq)
  no.y<- ncol(eval.list[[1]])-1
  
  for(i in seq(1, no.y)){
    new.df<- as.data.frame(matrix(NA, nrow = length(x.seq), ncol = no.y + 1))
    new.df[,1]<- x.seq
    for(j in seq(1, no.draws)){
      new.df[,j + 1]<- eval.list[[j]][,i + 1]
    }
    colnames(new.df)<- c("x", paste(rep("P", no.y), seq(1, no.y), sep = "_"))
    p<- ggplot(data = new.df)
    for(j in seq(1, no.draws)){
      p<- p + geom_line(data = eval.list[[j]], aes_string(x = x, y = paste0("P_",j)), col = j)
    }
    filename <- paste0('plot_', i, '.png')
    ggsave(filename, plot = p, device = "png")
  }
}
eval.plot(x)
library(ggplot2)

#ChatGPT
save_plots <- function(eval_list) {
  browser()
  k <- length(eval_list)
  
  # Loop through each plot (p - 1 plots)
  for (i in 1:(ncol(eval_list[[1]]) - 1)) {
    # Create a new ggplot object
    p <- ggplot()
    
    # Add lines for each element in eval_list
    for (j in 1:k) {
      p <- p + geom_line(data = eval_list[[j]], aes_string(x = names(eval_list[[j]])[1], y = names(eval_list[[j]])[i + 1]), col = j)
    }
    
    # Customize the plot
    p <- p + labs(x = 'X', y = paste('Y', i, sep = ''), title = paste('Plot', i))
    
    # Save the plot to working directory
    filename <- paste0('plot_', i, '.png')
    ggsave(filename, plot = p, device = "png")
  }
}
save_plots(x)
