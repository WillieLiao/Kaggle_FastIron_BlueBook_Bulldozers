source('submit_models.R')
source('cv_fit_rpart_tree.R')
source('cv_fit_rpart_bag.R')

mtrys.group1 <- c(25, 35, 10, 20, 30, 23)  # orig-feats1
n.core1.group1 <- c(18, 18, 8, 20, 18, 18)  #orig-feats1
n.core2.group1 <- c(6, 6, 4, 14, 4, 4)  #orig-feats1
n.core3.group1 <- c(6, 6, 4, 4, 4, 6)  #orig-feats1
n.bags.group1 <- c(75,40,25,125,55,75)  #orig-feats1

mtrys.group2 <- c(30, 35, 10, 23, 35, 25)  # orig-feats2
n.core1.group2 <- c(16, 16, 10, 16, 20, 16)  #orig-feats2
n.core2.group2 <- c(4, 4, 4, 10, 4, 6)  #orig-feats2
n.core3.group2 <- c(10, 4, 8, 4, 4, 10)  #orig-feats2
n.bags.group2 <- c(78,33,23,135,55,100) #orig-feats2

mtrys.group <- list(mtrys.group1, mtrys.group2)
n.core1.group <- list(n.core1.group1, n.core1.group2)
n.core2.group <- list(n.core2.group1, n.core2.group2)
n.core3.group <- list(n.core3.group1, n.core3.group2)
n.bags.group <- list(n.bags.group1, n.bags.group2)

ds.type.group <- list('-orig6', '-orig6')
cv.type <- ''
load(paste0(TRAIN_DATA_PATH, "unmatched.rda"))

library(doParallel)
cl<-makeCluster(4)
clusterExport(cl, list('unmatched.ModelID', 'unmatched.all', 'fit.tree'))
registerDoParallel(cl)

seed <- 20130410
preds.bags <- vector('list')
for (i in 1:2){
  ds.type=ds.type.group[[i]]
  load(paste0(TRAIN_DATA_PATH, cv.type, ds.type, ".rda"))
  preds.bags[[i]] <- foreach(group=groups, ds=all.ds,
                             mtry=mtrys.group[[i]],
                             n.core1=n.core1.group[[i]],
                             n.core2=n.core2.group[[i]],
                             n.core3=n.core3.group[[i]],
                             n.bag=n.bags.group[[i]],
                             .packages="rpart") %dopar% {
    n.bag <- max(n.bag, n.core1 + n.core2 + n.core3)
    preds <- fit.bag(seed+i*n.bag+n.core1-n.core2, n.bag, mtry, n.core1, n.core2, n.core3, ds=ds, cv=F)
    preds        
  }
  names(preds.bags[[i]]) <- groups
}
save(preds.bags, file=paste0(SUBMISSION_PATH, "bag.rda"))
# 
# obs <- vector('list')
# ds.type='orig6'
# obs <- lapply(all.ds, function(ds) data.frame(SalesID=as.numeric(names(ds[["y_ts"]]))))
# o <- unlist(obs)
# 
# submit <- matrix(0,0,2)
# for (group in groups){
#   p <- .5*preds.bags[[1]][[group]] + .5*preds.bags[[2]][[group]] 
#   p <- data.frame(SalesID=obs[[group]]$SalesID, SalePrice=round(exp(p)))
#   submit <- rbind(submit,p)
# }
# 
# write.csv(submit, file=paste0(SUBMISSION_PATH, "submit_bag.csv"), row.names=F)
# rm(all.ds, ds, p, submit, list=ls(pattern="mtry|n.core|n.bag"))
