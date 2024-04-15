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
  browser()
  library(lightgbm)
  set.seed(seed)
  initial.data<- inputdf[sample(seq(1,nrow(inputdf)),no.draws,replace = FALSE),]
  
  eval.data<- as.data.frame(matrix(NA, nrow = no.draws * length(eval.seq),
                                   ncol = ncol(inputdf)))
  colnames(eval.data)<- colnames(inputdf)
  eval.var.col<- which(colnames(eval.var)==eval.var)
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
    out[[i]]<- cbind(eval.data[selection,], prediction.output[selection,])
  }
  colnames(out)<- c(eval.var, paste(rep("V", ncol(prediction.output)), 
                                    seq(1, ncol(prediction.output))))
  return(out)
}

