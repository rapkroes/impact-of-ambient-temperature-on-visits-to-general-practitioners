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

eval.plot<- function(eval_list, y.max, plot.title, y.names = NA, 
                     y.label = "density") {
  # eval.plot takes list of simulated outcomes from booster.eval and plots the outcome on the y-axis.
  # eval_list is a list of simulations created through the booster.eval function.
  # y.max is the upper limit of the y-range of the plot
  # plot.title is a character string which serves as the name of the plot
  # y.names is a vector of names for the classes of the variable whose estimated density is shown on the y-axis. y.names is used in the creation of the plot title. If set to NA, the y.names argument is not considered in the creation of the plot title
  # y.label is the label attached to the y-axis
  k <- length(eval_list)
  x.seq<- eval_list[[1]][,1]
  for (i in 1:(ncol(eval_list[[1]]) - 1)) {
    x<- eval_list[[1]][,1]
    y<- eval_list[[1]][,i + 1]
    if(any(is.na(y.names))){
      plot.name<- plot.title
    }else{
      plot.name<- paste(plot.title, y.names[i], sep = ": ")
    }
    if(any(is.na(y.names))){
      file.name<- paste0(plot.title, ".png")
    }else{
      file.name<- paste0(plot.title, "_", y.names[i], ".png")
    }
    png(file.name)
    plot(y~x, type = 'l', xlab = colnames(eval_list[[1]])[1], ylab = y.label, 
         main = plot.name, xlim = range(x.seq), ylim = c(0,y.max))
    for (j in 2:k) {
      lines(x = x, y = eval_list[[j]][,i + 1])
    }
    dev.off()
  }
}

eval.plot(x, y.max = 1, plot.title = "Z plot", y.names = paste(rep("Var",11), 1:11))

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

model.eval<- function(booster, DI, sdi, Q, no.draws, eval.var, eval.seq, 
                      seed, y.max, plot.title, x.factor.names = NA, 
                      y.label = "density"){
  # simulates data from a booster and a df_qx-created data frame. Returns for each class outcome of the 'dependent' variable a plot with evaluation results.
  # booster is a booster, extracted from ga2model
  # DI is the discomfort index, given as "TDI", "HW", or "SDI"
  # sdi is the vector of SDI hyperparameters iff DI == "SDI"
  # Q is the numeric research question (either 1 or 2). It is fed into df_qx.
  # eval.var is the name of the variable that will be varied. In the returned plots, it will be on the x-axis.
  # eval.seq is the sequence of variables that will be inserted for the evaluated variable.
  # seed is the random seed.
  library(lightgbm)
  set.seed(seed)
  
  inputdf<- df_qx(di = DI, q = Q)
  blacklist<- c("thoms_discomfort_index", "length_heatwave", "sdi", 
                "daylight_hours", "covid_7_day_incidence", "age", 
                colnames(inputdf)[grep("chronic", colnames(inputdf))])
  if(DI == "SDI"){
    sdi.weights<- dbetabinom.ab(x = seq(0,sdi[1]), size = sdi[1],
                                shape1 = sdi[2], shape2 = sdi[3])
    sdi.vec<- SDI(df = inputdf, w = sdi.weights, theta = sdi[4:6],
                  rho = sdi[7:9], tau = sdi[10])
    
    col.selector<- grepl("temperature", colnames(inputdf)) | grepl("humidity", colnames(inputdf))
    df<- inputdf[,!col.selector]
    df$sdi<- sdi.vec
    factor.vars<- colnames(df)[!colnames(df) %in% blacklist]
    df<- data.matrix(df)
  }else{
    factor.vars<- colnames(inputdf)[!colnames(inputdf) %in% blacklist]
    df<- data.matrix(inputdf)
  }
  
  initial.data<- df[sample(seq(1, nrow(inputdf)), no.draws, replace = FALSE),]
  eval.data<- as.data.frame(matrix(NA, nrow = no.draws * length(eval.seq),
                                   ncol = ncol(df)))
  colnames(eval.data)<- colnames(df)
  eval.var.col<- which(colnames(eval.data)==eval.var)
  for(i in seq_along(eval.seq)){
    sel.rows<- seq(1, no.draws) + (i - 1) * no.draws
    eval.data[sel.rows,]<- initial.data
    eval.data[sel.rows,eval.var.col]<- eval.seq[i]
  }
  
  prediction.output<- matrix(predict(object = booster, 
                                     newdata = data.matrix(eval.data), 
                                     type = "response"),
                             nrow = nrow(eval.data))
  k<- ncol(prediction.output)
  possible.var.names<- c("thoms_discomfort_index", "PraxisID", "dow",
                         "public_holiday", "school_holiday", "week_of_month",
                         "month", "year", "daylight_hours", 
                         "covid_7_day_incidence", "age", "female", "PKV",
                         "smoking", "alcohol", "sport", "chronic_1", 
                         "chronic_2", "chronic_3", "chronic_4", "chronic_5", 
                         "chronic_6", "chronic_7", "chronic_8", "chronic_9", 
                         "chronic_10", "chronic_11", "no_all_chronic_diseases",
                         "length_heatwave", "sdi")
  possible.xlab.names<- c("Thom's Discomfort Index", "practice", 
                          "day of the week", "public holiday", "school holiday",
                          "week of the month", "month", "year", 
                          "daylight hours", "Covid-19 7-day-incidence", "age",
                          "gender", "health insurance", "smoking", "alcohol", 
                          "sport", "chronic cold-related injuries", 
                          "chronic injuries due to excessive heat", 
                          "chronic major cardivascular diseases", 
                          "chronic major external causes for injury", 
                          "chronic mental and behavioural disorders", 
                          "chronic diseases of the respiratory system", 
                          "chronic endocrine, nutritional, and metabolic disorders", 
                          "chronic diseases of the digestive system", 
                          "chronic genitourinary disorders", 
                          "chronic musculoskeletal disorders", 
                          "chronic other diseases and injuries", 
                          "all chronic diseases", "length heatwave", 
                          "suggested discomfort index")
  x.name<- possible.xlab.names[eval.var == possible.var.names]
  for(l in seq(1, k)){
    if(Q == 2){
      y.names<- c("cold-related injuries", 
                  "injuries due to excessive heat", 
                  "major cardivascular diseases", 
                  "major external causes for injury", 
                  "mental and behavioural disorders", 
                  "diseases of the respiratory system", 
                  "endocrine, nutritional, and metabolic disorders", 
                  "diseases of the digestive system", 
                  "genitourinary disorders", 
                  "musculoskeletal disorders", 
                  "other diseases and injuries")
      plot.name<- paste(plot.title, y.names[l], sep = ": ")
      file.name<- paste0(plot.title, "_", y.names[l], ".png")
    }else{
      plot.name<- plot.title
      file.name<- paste0(plot.title, ".png")
    }
    png(file.name)
    sel<- seq(1, nrow(eval.data), by = no.draws)
    plot(prediction.output[sel, l]~eval.data[sel, eval.var.col], type = "l", 
         xlab = x.name, ylab = y.label, main = plot.name, ylim = c(0, y.max))
    if(length(x.factor.names)>=2){
      text(x = eval.seq, y = -0.1, labels = x.factor.names, adj = 0.5)
    }
    for(i in seq(2, no.draws)){
      sel<- sel + 1
      lines(x = eval.data[sel, eval.var.col], y = prediction.output[sel, l])
    }
    dev.off()
  }
}

