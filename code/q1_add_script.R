###additional script

#lade folgende Datei (Datei öffnen): neuste Version von ws_[Datum]_new.RData im Ordner N:/StudentischeHilfskraefte/_Kroes (Christoph)/Routinedaten/03_Hitze/R

library(devtools)
library(readxl)
library(lubridate)
library(dplyr)
library(parallel)
library(lightgbm)
library(VGAM)
library(Matrix)
library(tictoc)

full.df_7$sdi_q1<- ga2sdi(hyperpars_age, full.df_7) #exploiting the fact, that all hyperparameters are identical

tdi_q1_quantiles<- quantile(full.df_7$thoms_discomfort_index, 
                            probs = c(0.05, 0.95))
sdi_q1_quantiles<- quantile(full.df_7$sdi_q1, probs = c(0.05, 0.95))
x.eval.seq<- list(
  seq(tdi_q1_quantiles[1], tdi_q1_quantiles[2], length.out = 10), 
  seq(0, 7), 
  seq(sdi_q1_quantiles[1], sdi_q1_quantiles[2], length.out = 10)
  )
model.list_1<- list(age = "quantile", gender = "cross_entropy",
                    phi = "cross_entropy", chronic = "quantile")
model.list_2<- list(age = y.age, gender = y.gender, phi = y.phi, 
                    chronic = y.chronic)

for(i in 1:3){
  di <- c("TDI", "HW", "SDI")[i]
  di.var<- c("thoms_discomfort_index", "length_heatwave", "sdi_q1")[i]
  for(quant in c(".05", ".25", ".50", ".75", ".95")){
    model.eval(booster = get(paste0("model_age", quant, "_", di)), DI = di,
               sdi = unlist(hyperpars_age$best_in_class[12:21]), Q = 1, 
               eval.var = di.var, eval.seq = x.eval.seq[[i]], 
               quant = as.numeric(quant), y.max = 100, y.name = "age", 
               y.label = "age")
    model.eval(booster = get(paste0("model_chronic", quant, "_", di)), DI = di,
               sdi = unlist(hyperpars_age$best_in_class[12:21]), Q = 1, 
               eval.var = di.var, eval.seq = x.eval.seq[[i]], 
               quant = as.numeric(quant), y.max = 30, y.label = "total no.",
               y.name = "total number of chronic diseases")
  }
  model.eval(booster = get(paste0("model_gender", "_", di)), DI = di,
             sdi = unlist(hyperpars_age$best_in_class[12:21]), Q = 1, 
             eval.var = di.var, eval.seq = x.eval.seq[[i]], 
             y.max = 100, y.label = "proportion", 
             y.name = "proportion of female patients")
  model.eval(booster = get(paste0("model_phi", "_", di)), DI = di,
             sdi = unlist(hyperpars_age$best_in_class[12:21]), Q = 1, 
             eval.var = di.var, eval.seq = x.eval.seq[[i]], 
             y.max = 100, y.label = "proportion", 
             y.name = "proportion of privately insured individuals")
}

wrapper.fun<- function(i){
  browser()
  di <- c("TDI", "HW", "SDI")[i]
  di.var<- c("thoms_discomfort_index", "length_heatwave", "sdi_q1")[i]
  for(quant in c(".05", ".25", ".50", ".75", ".95")){
    model.eval(booster = get(paste0("model_age", quant, "_", di), envir = .GlobalEnv), DI = di,
               sdi = unlist(hyperpars_age$best_in_class[12:21]), Q = 1, 
               eval.var = di.var, eval.seq = x.eval.seq[i], 
               quant = as.numeric(quant), y.max = 100, y.name = "age", 
               y.label = "age")
    model.eval(booster = get(paste0("model_chronic", quant, "_", di)), DI = di,
               sdi = unlist(hyperpars_age$best_in_class[12:21]), Q = 1, 
               eval.var = di.var, eval.seq = x.eval.seq[i], 
               quant = as.numeric(quant), y.max = 30, y.label = "total no.",
               y.name = "total number of chronic diseases")
  }
  model.eval(booster = get(paste0("model_gender", "_", di)), DI = di,
             sdi = unlist(hyperpars_age$best_in_class[12:21]), Q = 1, 
             eval.var = di.var, eval.seq = x.eval.seq[i], 
             y.max = 100, y.label = "proportion", 
             y.name = "proportion of female patients")
  model.eval(booster = get(paste0("model_phi", "_", di)), DI = di,
             sdi = unlist(hyperpars_age$best_in_class[12:21]), Q = 1, 
             eval.var = di.var, eval.seq = x.eval.seq[i], 
             y.max = 100, y.label = "proportion", 
             y.name = "proportion of privately insured individuals")
}
wrapper.fun(1)
