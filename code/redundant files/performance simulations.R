# SDI scale simulation

q_trial<- 1
trial.df_K<- df_qx(di = "SDI", q = q_trial)
trial.df_C<- df_qx(di = "SDI", q = q_trial)
trial.df_C[,grepl("temperature", colnames(trial.df_C))]<- trial.df_C[,grepl("temperature", colnames(trial.df_C))] -273.16
set.seed(8516)
sdi.params.trial_true<- unlist(sapply(1:3, FUN = function(k){
  sdi.sampler(k,1)
}))
# 1 Data generation process of SDI uses parameters from Thom's discomfort index
inits_thom<- c(NA, NA, NA, 0.45, 0, 0, -0.07975, 0, 0, 0.0055)
sdi.trial_true<- SDI(trial.df_C, 
                     w = dbetabinom.ab(seq(0,sdi.params.trial_true[1]), 
                                       sdi.params.trial_true[1],
                                       sdi.params.trial_true[2],
                                       sdi.params.trial_true[3]),
                     theta = inits_thom[4:6],
                     rho = inits_thom[7:9],
                     tau = inits_thom[10])
pollution_low<- rnorm(length(sdi.trial_true), mean = 0, sd = 2)
pollution_high<- rnorm(length(sdi.trial_true), mean = 0, sd = 4)
outcome_lp<- sdi.trial_true + pollution_low
outcome_hp<- sdi.trial_true + pollution_high

trial.model_Clp<- wrapper_interior(sdi = c(sdi.params.trial_true, 
                                           inits_thom[-seq(1,3)]),
                                   lr = 0.1, no.leaves = 31, max.depth = 8, 
                                   min.data.in.leaf = 31, 
                                   feature.fraction = 0.8, cat.l2 = 10,
                                   extra.trees = FALSE, top.rate = 0.2, 
                                   other.rate = 0.1, cat.smooth = 10, 
                                   path.smooth = 0, inputdf = trial.df_C,
                                   y = outcome_lp, est.type = "quantile")
trial.model_Chp<- wrapper_interior(sdi = c(sdi.params.trial_true, 
                                           inits_thom[-seq(1,3)]),
                                   lr = 0.1, no.leaves = 31, max.depth = 8, 
                                   min.data.in.leaf = 31, 
                                   feature.fraction = 0.8, cat.l2 = 10,
                                   extra.trees = FALSE, top.rate = 0.2, 
                                   other.rate = 0.1, cat.smooth = 10, 
                                   path.smooth = 0, inputdf = trial.df_C,
                                   y = outcome_hp, est.type = "quantile")
trial.model_Klp<- wrapper_interior(sdi = c(sdi.params.trial_true, 
                                           inits_thom[-seq(1,3)]),
                                   lr = 0.1, no.leaves = 31, max.depth = 8, 
                                   min.data.in.leaf = 31, 
                                   feature.fraction = 0.8, cat.l2 = 10,
                                   extra.trees = FALSE, top.rate = 0.2, 
                                   other.rate = 0.1, cat.smooth = 10, 
                                   path.smooth = 0, inputdf = trial.df_K,
                                   y = outcome_lp, est.type = "quantile")
trial.model_Khp<- wrapper_interior(sdi = c(sdi.params.trial_true, 
                                           inits_thom[-seq(1,3)]),
                                   lr = 0.1, no.leaves = 31, max.depth = 8, 
                                   min.data.in.leaf = 31, 
                                   feature.fraction = 0.8, cat.l2 = 10,
                                   extra.trees = FALSE, top.rate = 0.2, 
                                   other.rate = 0.1, cat.smooth = 10, 
                                   path.smooth = 0, inputdf = trial.df_K,
                                   y = outcome_hp, est.type = "quantile")

abs.diff_l<- - trial.model_Clp + trial.model_Klp
abs.diff_h<- - trial.model_Chp + trial.model_Khp
rel.diff_l<- - 1 + trial.model_Klp / trial.model_Clp
rel.diff_h<- - 1 + trial.model_Khp / trial.model_Chp

# 2 Data generation process of SDI uses normally distributed parameters
inits_nd<- c(NA, NA, NA, rnorm(7, sd = 1.5))
sdi.trial_true<- SDI(trial.df_C, 
                     w = dbetabinom.ab(seq(0,sdi.params.trial_true[1]), 
                                       sdi.params.trial_true[1],
                                       sdi.params.trial_true[2],
                                       sdi.params.trial_true[3]),
                     theta = inits_nd[4:6],
                     rho = inits_nd[7:9],
                     tau = inits_nd[10])
pollution_low<- rnorm(length(sdi.trial_true), mean = 0, sd = 2)
pollution_high<- rnorm(length(sdi.trial_true), mean = 0, sd = 4)
outcome_lp<- sdi.trial_true + pollution_low
outcome_hp<- sdi.trial_true + pollution_high

trial.model_Clp<- wrapper_interior(sdi = c(sdi.params.trial_true, 
                                           inits_nd[-seq(1,3)]),
                                   lr = 0.1, no.leaves = 31, max.depth = 8, 
                                   min.data.in.leaf = 31, 
                                   feature.fraction = 0.8, cat.l2 = 10,
                                   extra.trees = FALSE, top.rate = 0.2, 
                                   other.rate = 0.1, cat.smooth = 10, 
                                   path.smooth = 0, inputdf = trial.df_C,
                                   y = outcome_lp, est.type = "quantile")
trial.model_Chp<- wrapper_interior(sdi = c(sdi.params.trial_true, 
                                           inits_nd[-seq(1,3)]),
                                   lr = 0.1, no.leaves = 31, max.depth = 8, 
                                   min.data.in.leaf = 31, 
                                   feature.fraction = 0.8, cat.l2 = 10,
                                   extra.trees = FALSE, top.rate = 0.2, 
                                   other.rate = 0.1, cat.smooth = 10, 
                                   path.smooth = 0, inputdf = trial.df_C,
                                   y = outcome_hp, est.type = "quantile")
trial.model_Klp<- wrapper_interior(sdi = c(sdi.params.trial_true, 
                                           inits_nd[-seq(1,3)]),
                                   lr = 0.1, no.leaves = 31, max.depth = 8, 
                                   min.data.in.leaf = 31, 
                                   feature.fraction = 0.8, cat.l2 = 10,
                                   extra.trees = FALSE, top.rate = 0.2, 
                                   other.rate = 0.1, cat.smooth = 10, 
                                   path.smooth = 0, inputdf = trial.df_K,
                                   y = outcome_lp, est.type = "quantile")
trial.model_Khp<- wrapper_interior(sdi = c(sdi.params.trial_true, 
                                           inits_nd[-seq(1,3)]),
                                   lr = 0.1, no.leaves = 31, max.depth = 8, 
                                   min.data.in.leaf = 31, 
                                   feature.fraction = 0.8, cat.l2 = 10,
                                   extra.trees = FALSE, top.rate = 0.2, 
                                   other.rate = 0.1, cat.smooth = 10, 
                                   path.smooth = 0, inputdf = trial.df_K,
                                   y = outcome_hp, est.type = "quantile")

trial.model_Clp
trial.model_Klp
trial.model_Chp
trial.model_Klp

abs.diff_l<- - trial.model_Clp + trial.model_Klp
abs.diff_h<- - trial.model_Chp + trial.model_Khp
rel.diff_l<- - 1 + trial.model_Klp / trial.model_Clp
rel.diff_h<- - 1 + trial.model_Khp / trial.model_Chp

abs.diff_l
abs.diff_h
rel.diff_l
rel.diff_h
