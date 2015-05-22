source('settings.R') #uncomment install.packages as needed
source('util.R')
source('read_data.R')  # train, test, cpi

library(doParallel)
cl<-makeCluster(4)
registerDoParallel(cl)

source('preprocess_data.R') # train, test, mac
source('preprocess_impute_manual.R') # tt, mac
source('preprocess_merge.R') # merged, unmatched
source('preprocess_feats.R') # merged -> merged with MfgID based features
source('preprocess_impute_gbm.R') # merged -> ttm-imputed
source('submit_init_feats.R') # feats

data_type <- 'orig'
source('split_data.R') # split data

data_type <- 'rank'
source('map_to_ranks.R') # ttm-imputed -> ttm-imputed-rank
source('split_data.R') # split data

source('submit_rpart.R') # bag.rda
source('submit_gbm.R') # gbm-orig6-base.rda, ...
source('submit_rf_to_python.R')

# need to run some code in python first
# submit_rf2.py

source('submit_blend.R')